# Pipeline for Processing Eye-tracking Data

## Notebooks and Procedures

1. Gaze Projections and AOIs Mapping: 
	1.1 Gaze Data import and computation of X and Y gaze coordinates
	1.2  Data exploration and visual quality assessment (projecting gaze points on AOIs)
	1.3 Assign gaze points to AOIs
2.  Event Detection
	2.1 Oculomotor event detection (i.e., Fixations and saccades)
3. Fixation, Scan-path, Heatmap plots
	3.1 Fixations plots
	3.2 Scan-path plots
	3.3 Heatmap plots
4.  Mapping Fixations and Saccades to AOIs
	4.1 Mapping Fixations and Saccades to AOIs
5. AOI Visits
	5.1 Identify Dwells
6. AOI Sequence Charts
7. Fixation, Saccade, Dwell Measures (See [documentation/ETmetrics.csv](https://github.com/aminobest/ETLecture/blob/main/jupyter/workspace/documentation/ETmetrics.csv) for metrics and definions)
	7.1 Fixation measures at stimulus and AOI levels
	7.2 Saccade measures at stimulus and AOI levels
	7.3 Dwell measures
8. Transition Matrix and Markov Model
	8.1 Identify transitions (including self-transitions e.g., transitions from AOI1 to AOI1)
	8.2 Identify transitions (with no self-transitions)

## Pipeline Flow


![alt text](https://github.com/aminobest/ETLecture/blob/main/jupyter/workspace/pipeLineBPMNModel.png?raw=true)

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
docker compose up
```

Once the installation of the dependencies is over, a link to the jupyter lab web-application (starting with https://127.0.0.1...) will be showned in the terminal (cf. screnshot below)

![alt text](https://github.com/aminobest/ETLecture/blob/main/jupyter/jupyterLinkExample.png?raw=true)


## Use

Browse to "ETLecture" and start the docker instance using the following command

```powershell
docker compose up
```

 A new link to the jupyter lab web-application (starting with https://127.0.0.1...) will be showned in the terminal (similar to the screenshot above)


## Notes:

- To update a notebook, you should make a copy of it (Using File menu -> Save notebook as -> provide a new name)


