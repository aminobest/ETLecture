# Pipeline for Processing Eye-tracking data

## Notebooks and procedures

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
7. Fixation, Saccade, Dwell Measures (See documentation/ETmetrics.csv for metrics and definions)
	7.1 Fixation measures at stimulus and AOI levels
	7.2 Saccade measures at stimulus and AOI levels
	7.3 Dwell measures
8. Transition Matrix and Markov Model
	8.1 Identify transitions (including self-transitions e.g., transitions from AOI1 to AOI1)
	8.2 Identify transitions (with no self-transitions)

## Pipeline Flow

[![](https://raw.githubusercontent.com/aminobest/ETLecture/main/docker%20container/jupyter/workspace/pipeLineBPMNModel.png?token=GHSAT0AAAAAABYEFMOPLPMOIDKWO4SKRNCWY3552HA)](https://raw.githubusercontent.com/aminobest/ETLecture/main/docker%20container/jupyter/workspace/pipeLineBPMNModel.png?token=GHSAT0AAAAAABYEFMOPLPMOIDKWO4SKRNCWY3552HA)

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