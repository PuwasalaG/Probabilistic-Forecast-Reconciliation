Optimal_M1

This will first train the G using a set of inner rolling windows and
then evaluate that using the next rolling window. 

N <- nrow(Bottom_level)   #N - Length of the original data dagenerated
L <- 500                  #L - Length of the inner rolling window 
r <- 100                  #r - Length of the training set using to learn G matrix
m <- 4                    #m - Number of bottom level series
B <- 2500                 #B - The size of the future paths generated 
H <- 3                    #H - Forecast horizons
