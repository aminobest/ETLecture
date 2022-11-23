#' Create metadata for the IVT output.
#'
#' @param names The column names of the gaze processed data.
#'
#' @return A data.table with metadata information. Column names will be converted to metadata headers and there must be
#'         a row corresponding to each column name.
ivtMetadata <- function(names) {
    identifier <- c("", "ET_IVT_Gaze_X", "ET_IVT_Gaze_Y", "ET_IVT_Interpolated_Gaze_X", "ET_IVT_Interpolated_Gaze_Y",
                    "ET_IVT_Interpolated_Distance", "ET_IVT_Gaze_Velocity", "ET_IVT_Gaze_Acceleration",
                    "ET_IVT_Fixation_Index", "ET_IVT_Fixation_Index_by_Stimulus", "ET_IVT_Fixation_X",
                    "ET_IVT_Fixation_Y", "ET_IVT_Fixation_Start", "ET_IVT_Fixation_End", "ET_IVT_Fixation_Duration",
                    "ET_IVT_Fixation_Dispersion", "ET_IVT_Saccade_Index", "ET_IVT_Saccade_Index_by_Stimulus",
                    "ET_IVT_Saccade_Start", "ET_IVT_Saccade_End", "ET_IVT_Saccade_Duration", "ET_IVT_Saccade_Amplitude",
                    "ET_IVT_Saccade_Peak_Velocity", "ET_IVT_Saccade_Peak_Acceleration",
                    "ET_IVT_Saccade_Peak_Deceleration", "ET_IVT_Saccade_Direction")

    unit <- c("", rep("Pixels", 4), "Millimeters", "Degrees per second", "Degrees per second squared",
              rep("Index", 2), rep("Pixels", 2), rep("Milliseconds", 3), "Degrees", rep("Index", 2),
              rep("Milliseconds", 3), "Degrees", "Degrees per second", rep("Degrees per second squared", 2), "Degrees")

    shortUnit <- c("", rep("px", 4), "mm", "deg/s", "deg/s^2", "", "", "px", "px", rep("ms", 3), "deg", "", "",
                   rep("ms", 3), "deg", "deg/s", "deg/s^2", "deg/s^2", "deg")

    description <- c("", '"Gaze data X-coordinate (relative to top-left corner of the screen). Average of the
                     X-coordinates of the left and right eye."', '"Gaze data Y-coordinate (relative to top-left corner
                     of the screen). Average of the Y-coordinates of the left and right eye."',
                     '"Gaze data X-coordinate (relative to top-left corner of the screen) with missing coordinates
                     interpolated. Average of the X-coordinates of the left and right eye."', '"Gaze data Y-coordinate
                     (relative to top-left corner of the screen) with missing coordinates interpolated. Average of the
                     Y-coordinates of the left and right eye."', '"Estimated distance between the eye-tracker and
                     the eyes, with missing values interpolated. Average of the eye tracker\'s distance to the left and
                     right eye."', '"Angular velocity of the gaze at the current sample point, i.e. how fast the eyes
                     are moving at this point in time."', '"Angular acceleration of the gaze at the current sample
                     point, i.e. how much the eyes increased in speed at the current sample."',
                     '"Fixation number, counting from start of the recording."', '"Fixation number, counting from start
                     of the stimulus."', '"X-coordinate of the fixation centroid (relative to top-left corner of
                     the screen)."', '"Y-coordinate of the fixation centroid (relative to top-left corner of the
                     screen)."', '"Start time of the fixation (relative to recording start)."', '"End time of the
                     fixation (relative to recording start)."', '"Duration of the fixation."', '"Dispersion of the
                     fixation, i.e. how much the fixation\'s gaze points are spread out."', '"Saccade number, counting
                     from start of the recording."', '"Saccade number, counting from start of the stimulus."', '"Start
                     time of the saccade (relative to recording start)."', '"End time of the saccade (relative to
                     recording start)."', '"Duration of the saccade."', '"Amplitude of the saccade, i.e. angular
                     distance that the eyes travelled from start point to end point."', '"Peak velocity of the saccade,
                     i.e. the maximal speed of the eyes during this saccade."', '"Peak acceleration of the saccade, i.e.
                     the maximal increase in speed of the eyes during this saccade."', '"Peak deceleration of the
                     saccade, i.e. the maximal decrease in speed of the eyes during this saccade."', '"Direction of the
                     saccade from its start point to end point, indicated as counterclockwise angles: 0 degrees mean a
                     horizontal saccade from left to right, 90 degrees a vertical saccade from bottom to top."')

    # As description are long we need to strip line returns
    description <- strwrap(description, width = 10000)

    category <- c("Timestamp", rep('"ET(Gaze data, 2D)"', 4), "ET(Distance)", rep("ET(Gaze movements)", 2),
                  rep("ET(Fixations)", 8), rep("ET(Saccades)", 10))

    displayName <- c("", "X-coordinate of gaze point", "Y-coordinate of gaze point",
                     "X-coordinate of gaze point (interpolated)", "Y-coordinate of gaze point (interpolated)",
                     "Distance (interpolated)", "Gaze velocity", "Gaze acceleration", "Index", "Index (per stimulus)",
                     "X-coordinate of fixation", "Y-coordinate of fixation", "Start time", "End time", "Duration",
                     "Dispersion", "Index", "Index (per stimulus)", "Start time", "End time", "Duration", "Amplitude",
                     "Peak velocity", "Peak acceleration", "Peak deceleration", "Direction")

    group <- c("", rep("Gaze 2D", 4), "Distance", rep("Gaze movements", 2), rep("Fixations", 8), rep("Saccades", 10))

    showInUI <- c(rep("FALSE", 3), rep("TRUE", 5), rep("FALSE", 2), rep("TRUE", 2), rep("FALSE", 2), rep("TRUE", 2),
                  rep("FALSE", 4), rep("TRUE", 6))

    metadata <- data.table("Channel Identifier" = identifier, "Unit" = unit, "Short Unit" = shortUnit,
                           "Description" = description, "Provided By" = "iMotions", "Category" = category,
                           "Display name" = displayName, "Group" = group, "ImotionsInternal" = "")

    metadata$ImotionsInternal[c(13:14, 19:20)] <- "SlideShowTimestamp"
    hideIndex <- c(1:3, 9:10, 13:14, 17:20)
    metadata$ImotionsInternal[hideIndex] <- paste(metadata$ImotionsInternal[hideIndex], "NoGraphDisplay")

    return(metadata)
}

#' Create metadata for the dispersion output.
#'
#' @param metadata A data.table with metadata information. Column names will be converted to metadata headers and
#'                 there must be a row corresponding to each column name.
#'
#' @return A data.table with metadata information. Column names will be converted to metadata headers and there must be
#'         a row corresponding to each column name.
dispersionMetadata <- function(metadata) {
    identifier <- c("", "ET_DD_Gaze_X", "ET_DD_Gaze_Y", "ET_DD_Interpolated_Gaze_X", "ET_DD_Interpolated_Gaze_Y",
                    "ET_DD_Interpolated_Distance", "ET_DD_Fixation_Index", "ET_DD_Fixation_Index_by_Stimulus",
                    "ET_DD_Fixation_X", "ET_DD_Fixation_Y", "ET_DD_Fixation_Start", "ET_DD_Fixation_End",
                    "ET_DD_Fixation_Duration", "ET_DD_Fixation_Dispersion")

    metadata$`Channel Identifier` <- identifier

    return(metadata)
}


#' Prepare the final output of the gaze processing.
#'
#' @param results A list with gaze data, fixations and saccades information.
#'
#' @return An updated eyetracker sample data.table with fixations and saccades information.
outputProcessing <- function(results, fixationFilter) {
    data <- results$data
    fixations <- results$fixations

    message("processing fixations")

    colNamesGaze <- c("Gaze X", "Gaze Y", "Interpolated Gaze X", "Interpolated Gaze Y", "Interpolated Distance",
                      "Gaze Velocity", "Gaze Acceleration")

    # Add fixations columns
    colNamesFix <- c("Fixation Index by Stimulus", "Fixation X", "Fixation Y", "Fixation Start", "Fixation End",
                     "Fixation Duration", "Fixation Dispersion")


    if (is.null(fixations) || nrow(fixations) == 0) {
        set(data, j = c("FixID", colNamesFix), value = NA_integer_)
    } else {
        data <- with(fixations, {
            invisible(lapply(seq(nrow(fixations)), function(x) {
                set(data, i = which(data$Timestamp >= startTime[x] & data$Timestamp <= endTime[x]), "FixID", ID[x])
            }))

            data[, (colNamesFix) := if (!is.na(FixID)) {
                list("", centroidX[FixID], centroidY[FixID], startTime[FixID], endTime[FixID],
                     duration[FixID], dispersion[FixID])}, by = FixID] #nolint
        })
    }

    message("processing saccades")

    # Add saccades columns

    colNamesSac <- c("Saccade Index by Stimulus", "Saccade Start", "Saccade End", "Saccade Duration",
                     "Saccade Amplitude", "Saccade Peak Velocity", "Saccade Peak Acceleration",
                     "Saccade Peak Deceleration", "Saccade Direction")

    if (fixationFilter == "IVT") {
        saccades <- results$saccades

        if (is.null(saccades) || nrow(saccades) == 0) {
            set(data, j = c("SacID", colNamesSac), value = NA_integer_)
        } else {
            data <- with(saccades, {
                invisible(lapply(seq(nrow(saccades)), function(x) {
                    set(data, i = which(data$Timestamp >= startTime[x] & data$Timestamp <= endTime[x]), "SacID", ID[x])
                }))

                data[, (colNamesSac) := if (!is.na(SacID)) {
                    list("", startTime[SacID], endTime[SacID], duration[SacID], amplitude[SacID],
                         peakVelocity[SacID], peakAcceleration[SacID], peakDeceleration[SacID],
                         dirAngle[SacID])}, by = SacID] #nolint
            })
        }
    } else {
        # Adding temporary empty columns that will be removed before output
        data[, c("GazeVelocityAngle", "GazeAccelerationAngle")] <- NA_integer_
        set(data, j = c(colNamesSac), value = NA_integer_)
        data$SacID <- NA_integer_
        setcolorder(data, c(1:7, 16, 17, 8:15, 27, 18:26))
    }

    data <- data[, -c("Distance")]

    #names(data) <- c("Timestamp", colNamesGaze, "Fixation Index", colNamesFix, "Saccade Index", colNamesSac)
    #metadata <- ivtMetadata(names(data))

    # In case of the duration dispersion filter we remove some empty columns and modify channel identifier
    if (fixationFilter != "IVT") {
        colsToRemove <- which(names(data) %in% c("Gaze Velocity", "Gaze Acceleration", "Saccade Index", colNamesSac))
        data <- data[, -..colsToRemove]
        metadata <- metadata[-colsToRemove, ]
        metadata <- dispersionMetadata(metadata)
    }

    return(data)
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
