import matplotlib
import matplotlib.pyplot as plt
import random
import numpy



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