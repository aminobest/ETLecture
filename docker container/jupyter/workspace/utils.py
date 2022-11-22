import matplotlib
import matplotlib.pyplot as plt
import random
import numpy
import pandas as pd
import math 

ANNOTATION_DISTANCE_THRESHOLD = 40
ANNOTATION_DISTANCE_INCREMENT = 40


# Plot gazes and AOIs
def initPlotContainer(backgroundImagePath):
    """ Method Description: initiate a coordinate system with a background image
        Method Arguments: 
                backgroundImagePath: path of the background image
         Method Return: fig (figure), ax (array of axes) """   
    # set plot dimensions
    plt.rcParams['figure.figsize'] = [40, 20]
    # initiate plot
    fig, ax = plt.subplots()
    #invert axses
    ax.invert_yaxis()
    # plot stimulus as background
    img = plt.imread(backgroundImagePath)
    ax.imshow(img)
    return fig, ax

def generateRandomColors(nColors):
    """ Method Description: generate a list of random colors
        Method Arguments: 
                nColors: number of random colors in the list
         Method Return: list of random colors """       
    
    colors = ["#"+''.join([random.choice('0123456789ABCDEF') for j in range(6)])
                 for i in range(nColors)]
    return colors


def pointInRect(px,py,x1,y1,w,h):
	""" Method Description: check if a point is within a rectrangle
	    Method Arguments: 
	            px: point x cooridinate value
	            py: point y cooridinate value
	            x1: retangle starting point in x coordinate 
	            y1: retangle starting point om y coordinate 
	            w: retangle width
	            h: rectangle height
	     Method Return: 0 or 1 (0: gaze point is not in AOI, 1: gaze point is within AOI)"""

	x2 = x1+w
	y2 = y1+h
	return 1 if px >= x1 and px <= x2 and py >= y1 and py <= y2 else 0; 



def gaussian(x, sx, y=None, sy=None):
	"""Method Description: Returns an array of numpy arrays (a matrix) containing values between
	1 and 0 in a 2D Gaussian distribution
	
	Method Arguments: 
	x		-- width in pixels
	sx		-- width standard deviation
	
	Method Keyword Arguments: 
	y		-- height in pixels (default = x)
	sy		-- height standard deviation (default = sx)

	Method Return: M, matrix
	
    reference: PyGazeAnalyser https://github.com/esdalmaijer/PyGazeAnalyser/
    """
	
	# square Gaussian if only x values are passed
	if y == None:
		y = x
	if sy == None:
		sy = sx
	# centers	
	xo = x/2
	yo = y/2
	# matrix of zeros
	M = numpy.zeros([y,x],dtype=float)
	# gaussian matrix
	for i in range(x):
		for j in range(y):
			M[j,i] = numpy.exp(-1.0 * (((float(i)-xo)**2/(2*sx*sx)) + ((float(j)-yo)**2/(2*sy*sy)) ) )

	return M



def identifyTransition(data,rowIndex):
	"""Method Description: Identify transations between AOIs
	
	
	Method Arguments: 
	data		    -- dataframe
	rowIndex		-- index of a row in the dataframe data


	Method Return: pd.Series([Respondent, TransitionSource, TransitionTarget]) 
	"""

	# check that the last row is not reached yet
	if rowIndex<data.shape[0]-1:
		# check that the participant is the same
		if data.iloc[rowIndex]['Respondent']==data.iloc[rowIndex+1]['Respondent']:
			return pd.Series([data.iloc[rowIndex]['Respondent'],data.iloc[rowIndex]['VisitedAOI'], data.iloc[rowIndex+1]['VisitedAOI']]) 
	return pd.Series([numpy.nan,numpy.nan,numpy.nan]) 



def generateTransitionMatrix(transitionsDf,sourceColumnName,targetColumnName, normalize=False):
	"""Method Description: generate transition matrix 
	
	
	Method Arguments: 
	transitionsDf		    -- dataframe with transitions
	sourceColumnName		-- name of the column with the source of the transitions
	targetColumnName		-- name of the column with the target of the transitions
	normalize				-- normalization method. Default is False

	Method Return: transition matrix (as dataframe)
	"""	


	#drop nans from transitions
	transitionsDf = transitionsDf.dropna(subset=[sourceColumnName,targetColumnName])

	#get unique aois
	aoisInTransitions = pd.unique(transitionsDf[[sourceColumnName,targetColumnName]].values.ravel('K')).tolist()

	#using pandas.crosstab to generate transition matrix
	transitionMatrix = pd.crosstab(transitionsDf[sourceColumnName],
	                           transitionsDf[targetColumnName],normalize=normalize)

	#reindex to show all aois in the transitionMatrix 
	transitionMatrix = transitionMatrix.reindex(columns=aoisInTransitions, index=aoisInTransitions, fill_value=0)

	return transitionMatrix



def calculateInitialAnnotationPosition(aois,index):    
	"""Method Description: calculate the initial positions on the text annotations showing the AOIs names


	Method Arguments: 
	aois					-- dataframe (should contain the following columns: p1x,p1y, width, height)
	index                   -- row index

	Method Return: pd.series with two columns 
	"""	
	return pd.Series([aois.iloc[index]["p1x"]+aois.iloc[index]["width"]/2, aois.iloc[index]["p1y"]+aois.iloc[index]["height"]/2])


def avoidAnnotationOverlap(x0,y0,index,aois):
	"""Method Description: avoid annotations overlap if it exists


	Method Arguments: 
	x0 					-- annotation x position
	y0                   -- annotation y position
	index                   -- row index
	aois						-- dataframe (should contain the following columns: annotationX,annotationY )

	Method Return: pd.series with two columns 
	"""	

	# Emulating a do-while in python   
	while True:
		#derive a list (of distances) where the Euclidian distance between the annotation (defined by x0,y0) and each of the other annotations is calculated
		annotationDistanceToOthers = [math.dist([x0,y0],[x, y]) for x,y in aois[["annotationX","annotationY"]].loc[(aois['annotationX'] != x0) & (aois['annotationY'] != y0)].to_numpy()]
		#check if all distances are above or equal to ANNOTATION_DISTANCE_THRESHOLD
		if all(x >= ANNOTATION_DISTANCE_THRESHOLD for x in annotationDistanceToOthers):
			break
		else:
			# add an increment to y0
			y0 = y0 + ANNOTATION_DISTANCE_INCREMENT
			#update aois dataframe
			aois.at[index, 'annotationY'] = y0

	return (x0, y0)




