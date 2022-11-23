library(data.table)
library(zoo)
library(arrow)

source(paste(getwd(),"/gazePreprocessing.R",sep = ""))
source(paste(getwd(),"/ivt.R",sep = ""))
source(paste(getwd(),"/outputProcessing.R",sep = ""))
source(paste(getwd(),"/durationDispersion.R",sep = ""))


#* @post /eventDetection
#* @param config
#* @param data
function(config,data) {

  output <<- mainProcessing(config,data)

  message("returning output")

  return(output)

}


#' Main function for the gaze processing calling either the IVT filter or Duration Dispersion filter
#'
#' @param params The list of parameters as given to the script.
#' @param data gaze data
mainProcessing <- function(params,data) {

    # Parse as data.table
    data <- as.data.table(data)

    # Set NA to -1 values 
    data[data == -1, ] <- NA

    # Compute the average time difference between timestamps for further processing based on the 100 first samples
    diffTimestamps <- mean(abs(diff(data[1:100, ]$Timestamp)), na.rm = TRUE)

    # Get pre-processed data (GazeX, GazeY, InterpolatedGazeX, InterpolatedGazeY)
    message("Gaze pre-processing")
    data <- gazePreProcessing(params, data, diffTimestamps)

    # Get fixations / saccades informations using either the IVT filter or the duration dispersion algorithm
    if (params$fixationFilter == "IVT") {
        message("IVT processing")
        results <- ivt(params, data, diffTimestamps)
        sensorName <- "I-VT filter"
    } else {
        ## Defining default parameters for the duration dispersion filter
        maxAngle <- 1 # maximal visual angle allowed between two consecutive gazepoint to create a cluster
        minDuration <- 100 # minimal duration (in ms) of a gazepoint cluster to be counted as an actual fixation

        message("DD processing")
        results <- durationDispersion(params, data, maxAngle, minDuration)
        sensorName <- "Duration Dispersion filter"
    }

    # Prepare final out
    message("Creating output")
    output <- outputProcessing(results, params$fixationFilter)

    return(output)

}



# Adapted version

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
