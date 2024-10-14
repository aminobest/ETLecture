# Pipeline for Processing Eye-tracking Data

## Overview

![alt text](https://github.com/aminobest/ETLecture/blob/main/jupyter/workspace/pipeLineBPMNModel.png?raw=true)

(click on the image for better quality)

## Notebooks and Procedures

1. Gaze Projections and AOIs Mapping: 
	* Gaze Data import and computation of X and Y gaze coordinates 
	*  Data exploration and visual quality assessment (projecting gaze points on AOIs)
	* Assign gaze points to AOIs 
	
2.  Event Detection
	* Oculomotor event detection (i.e., Fixations and saccades)

3. Fixation, Scan-path, Heatmap plots
	* Fixations plots
	* Scan-path plots
	* Heatmap plots

4.  Mapping Fixations and Saccades to AOIs
	* Mapping Fixations and Saccades to AOIs

5. AOI Visits
	* Identify Dwells


6. AOI Sequence Charts


7. Fixation, Saccade, Dwell Measures (See [documentation/ETmetrics.csv] 
(https://github.com/aminobest/ETLecture/blob/main/jupyter/workspace/documentation/ETmetrics.csv) for metrics and definions) 
	* Fixation measures at stimulus and AOI levels
	* Saccade measures at stimulus and AOI levels
	* Dwell measures
	
8. Transition Matrix and Markov Model
	* Identify transitions (including self-transitions e.g., transitions from AOI1 to AOI1) 
	* Identify transitions (with no self-transitions)



## Installation

### Prerequisites

1. Docker https://docs.docker.com/get-docker/
2. Git https://git-scm.com/downloads

### Procedure

```powershell
git clone https://github.com/aminobest/ETLecture.git
```

```powershell
cd ETLecture
docker compose up --build
```

The build process can take some minutes, once it's over, a link to the jupyter lab web-application (starting with https://127.0.0.1:8888/lab?token=) will be showned in the terminal (cf. screnshot below)

![alt text](https://github.com/aminobest/ETLecture/blob/main/jupyter/jupyterLinkExample.png?raw=true)

Copy this link and paste it in your browser

## Use

Browse to "ETLecture" and start the docker instance using the following command

```powershell
docker compose up
```

 A new link to the jupyter lab web-application (starting with https://127.0.0.1:8888/lab?token=) will be showned in the terminal (similar to the screenshot above)

## Update notebooks

Browse to "ETLecture" and run the following

```powershell
git pull
```

```powershell
docker compose up --build
```
