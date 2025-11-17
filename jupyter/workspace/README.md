# Pipeline for Processing Eye-tracking Data

## Overview

![alt text](https://github.com/aminobest/ETLecture/blob/main/jupyter/workspace/pipeLineBPMNModel.png?raw=true)


## Notebooks and Procedures


1. __Gaze Projections and Area of Interest (AOIs) Mapping__ 
	* Gaze Data import and computation of X and Y gaze coordinates 
	*  Data exploration and visual quality assessment (projecting gaze points on AOIs)
	* Assign gaze points to AOIs 
	     
	     
2.  __Event Detection__
	* Detection of fixations and saccades from gaze data



3. __Fixation, Scan-path, Heatmap plots__
	* Fixations plots: Projections of participants' fixations on the stimuli.
	* Scan-path plots: Projections of participants' scan-paths on the stimuli.
	* Heatmap plots: Projections of participants' heatmaps on the stimuli.



4.  __Mapping Fixations and Saccades to AOIs__
	* Mapping Fixations and Saccades to AOIs



5. __AOI Visits__
	* Identify visits to AOIs (Dwells)



6. __AOI Sequence Charts__
	* Scarf plots illustrating participants' transitions between Areas of Interest (AOIs) over time



7. __Fixation, Saccade, Dwell Measures__ (See [documentation/ETmetrics.csv] for metrics and definitions) 
	* Fixation measures at the stimulus and AOI levels
	* Saccade measures at the stimulus and AOI levels
	* Dwell measures
	


8. __Transition Matrix and Markov Model__
	* Transition matrix: counts the transitions between different AOIs
	* Markov Model: shows the probabilities of transitions between different AOIs



