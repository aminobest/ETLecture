#' For a specific gazepoint, check whether it is invalid (i.e. if X, Y or distance is missing for this point)
#'
#' @param gazeX The respondent gaze X coordinates vector.
#' @param gazeY The respondent gaze Y coordinates vector.
#' @param interpolatedDistance The respondent gaze distance vector.
#' @param idxToCheck The index of the gaze to check.
#'
#' @return A boolean with a TRUE value if any of the X, Y or distance is missing for this gazepoint.
isInvalidGaze <- function(gazeX, gazeY, interpolatedDistance, idxToCheck) {
    isInvalid <- is.na(gazeX[idxToCheck]) || is.na(gazeY[idxToCheck]) || is.na(interpolatedDistance[idxToCheck])
}




#' Detect potential gazepoints being part of a fixation based on the maxAngle parameter.
#'
#' @param data A data.table containing the eyetracker sample.
#' @param maxAngle The maximal visual angle allowed between two consecutive gazepoint to create a cluster.
#' @param dpi The dots per inch value to transform distances in mm.
#'
#' @return A vector of potential fixations indexes (0 if not a fixation).
fixationDetection <- function(data, maxAngle, dpi) {
    gazeX <- data[["InterpolatedGazeX"]]
    gazeY <- data[["InterpolatedGazeY"]]
    interpolatedDistance <- data[["InterpolatedDistance"]]

    fixationEvents <- rep(0, length(gazeX))
    maxAngleRadius <- maxAngle / 2

    # Trailing NA at the end of the data won't be processed
    maxDataIdx <- max(which(!(is.na(gazeX) | is.na(gazeY) | is.na(interpolatedDistance))), 1, na.rm = TRUE)

    # Initialize the gaze current index and fixation index
    currentIdx <- 1
    fixIdx <- 1

    # In case no good data is available or data is too short, the dispersion is not run
    if (maxDataIdx == 1) {
        return(fixationEvents)
    }

    # Repeat for all gazes and detect potential fixations
    repeat {
        # Invalid gazepoints are ignored
        while (isInvalidGaze(gazeX, gazeY, interpolatedDistance, currentIdx) && currentIdx <= maxDataIdx) {
            currentIdx <- currentIdx + 1
        }

        # When a valid gazepoint is found, start a potential fixation
        inFix <- TRUE
        startIdx <- currentIdx

        # Loop through gazes as long as they keep being below the dispersion threshold
        while (inFix) {
            currentIdx <- currentIdx + 1

            # We keep count of NA consecutive occurrences to remove them if they occurs at the end of a fixation
            countNA <- 0

            # Invalid gazepoints are ignored
            while (isInvalidGaze(gazeX, gazeY, interpolatedDistance, currentIdx) && currentIdx <= maxDataIdx) {
                currentIdx <- currentIdx + 1
                countNA <- countNA + 1
            }

            # Get current fixation centroid (average of gazes) and use it to compute the distance
            centroid <- c(mean(gazeX[startIdx:(currentIdx - 1)], na.rm = T),
                          mean(gazeY[startIdx:(currentIdx - 1)], na.rm = T))

            distance <- distanceToVisualAngle(toMm(dist(rbind(c(gazeX[currentIdx], gazeY[currentIdx]), centroid)), dpi),
                                              mean(interpolatedDistance[startIdx:currentIdx], na.rm = TRUE))

            # Checking if the distance is less than the maxAngleRadius
            if (distance < maxAngleRadius) {
                # If it's the last gazepoint - we need to end the fixation early
                if (currentIdx == maxDataIdx) {
                    fixationEvents[startIdx:currentIdx] <- fixIdx
                    inFix <- FALSE
                } else {
                    # We are still in a fixation and will check the next gazepoint to see if it is part of it too
                    next()
                }
            } else if (currentIdx - startIdx - countNA > 1) {
                # A potential fixation has just ended so we mark it before escaping the loop
                fixationEvents[startIdx:(currentIdx - countNA - 1)] <- fixIdx
                fixIdx <- fixIdx + 1
                inFix <- FALSE
            } else {
                # A potential fixation has just ended but it was composed of only one point so we don't mark it
                inFix <- FALSE
            }
        }

        # If it's the last gazepoint we exit the repeat process
        if (currentIdx == maxDataIdx) {
            break()
        }
    }

    return(fixationEvents)
}


#' Compute Fixation events information (start/end time, duration and averaged gaze/eye).
#'
#' @param data A data.table containing the eyetracker sample.
#' @param fixationEvents A vector of potential fixations indexes (0 if not a fixation).
#' @param dpi The dots per inch value to transform distances in mm.
#'
#' @return A data.table with information about fixations.
dispersionFixationProcessing <- function(data, fixationEvents, dpi) {
    # Get start / end index and groupSize for each fixation event
    epFix <- with(rle(fixationEvents), {
        end <- cumsum(lengths)
        start <- end - lengths + 1
        groupSize <- end - start + 1
        ok <- values > 0
        data.table(start, end, groupSize)[ok, ]
    })

    # If no fixation are found return an empty table
    if (nrow(epFix) == 0) {
        fixations <- data.table(matrix(data = NA_integer_, ncol = 8, nrow = 0))
        names(fixations) <- c("ID", "startTime", "endTime", "centroidX", "centroidY", "percentageNonNA", "dispersion",
                              "duration")
        return(fixations)
    }

    fixations <- data.table(ID = seq(nrow(epFix)))

    # The fixation start (end) will correspond to the average between the first (last) sample being part of it and the
    # one just before (after)
    # Note: centroidZmm, eyeXmm and eyeYmm are constant so we can just use the start value
    fixations <- with(epFix, {
        fixations[, `:=`(startTime = data$Timestamp[start[ID]],
                         endTime = data$Timestamp[end[ID]],
                         centroidX = mean(data$InterpolatedGazeX[start[ID]:end[ID]], na.rm = T),
                         centroidY = mean(data$InterpolatedGazeY[start[ID]:end[ID]], na.rm = T),
                         percentageNonNA = sum(!is.na(data$InterpolatedGazeX[start[ID]:end[ID]]) &
                                                   !is.na(data$InterpolatedGazeY[start[ID]:end[ID]])) /
                             groupSize[ID] * 100),
                  by = "ID"]

        fixations[, `:=`(dispersion = ifelse(end[ID] != start[ID],
                                             dispersionDegree(cbind(data$InterpolatedGazeX[start[ID]:end[ID]],
                                                                    data$InterpolatedGazeY[start[ID]:end[ID]]),
                                                              centroidXY = cbind(centroidX[1], centroidY[1]),
                                                              mean(data$InterpolatedDistance[start[ID]:end[ID]],
                                                                   na.rm = T),
                                                              dpi),
                                             NA_real_),
                         duration = endTime - startTime),
                  by = "ID"]
    })

    return(fixations)
}


#' Verify that fixations duration are higher than the duration threshold and that at least 50% of the data is
#  available.
#'
#' @param fixations A data.table with information about the fixations to verify.
#' @param minDuration The minimal duration (in ms) of a gazepoint cluster to be counted as an actual fixation.
#'
#' @return A data.table with information about verified fixations only.
verifyFixation <- function(fixations, minDuration) {
    # Remove fixations with a duration below minDuration
    fixations <- fixations[duration >= minDuration, ]

    # Remove fixations with less than 50% data available
    fixations <- fixations[percentageNonNA >= 50, ]

    # Recompute fixations ID
    fixations <- fixations[, ID := seq(nrow(fixations))]
    return(fixations)
}

#' Based on a maximal visual angle of dispersion (maxAngle) and minimal duration (minDuration), classify gazepoints as
#' fixations and return information about them.
#'
#' @param params The list of parameters as given to the dispersion algorithm.
#' @param data A data.table containing the eyetracker sample for dispersion processing.
#' @param maxAngle The maximal visual angle allowed between two consecutive gazepoint to create a cluster.
#' @param minDuration The minimal duration (in ms) of a gazepoint cluster to be counted as an actual fixation.
#'
#' @return A list with gaze data and fixations information.
durationDispersion <- function(params, data, maxAngle, minDuration) {
    # Compute dpi (dots per inch) to transform pixel distances in mm
    dpi <- sqrt(params$screenResolutionWidth ^ 2 + params$screenResolutionHeight ^ 2) / params$monitorSize

    # 1) Detect potential fixations based on the maximal visual angle =================================================
    message("DD - detecting fixations")
    fixationEvents <- fixationDetection(data, maxAngle, dpi)

    # 2) Compute Fixations events information =========================================================================
    # A temporary column percentageNonNA is also created to be used by the verifyFixation function
    message("DD - processing fixations")
    fixations <- dispersionFixationProcessing(data, fixationEvents, dpi)

    # 3) Verify that fixations duration are higher than the duration threshold and that at least 50% of the data is
    #  available
    if (nrow(fixations) != 0) {
        message("DD - verifying fixations")
        fixations <- verifyFixation(fixations, minDuration)
    }

    return(list(data = data, fixations = fixations))
}

# The contents of this script are licensed under the MIT license:
# Copyright (c) 2018 iMotions
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
