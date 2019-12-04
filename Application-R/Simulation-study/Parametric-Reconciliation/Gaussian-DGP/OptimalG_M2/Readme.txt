Optimal_M2

Devide the whole outer rolling window to small windows to train the 
optimal G

C <- 1000   #C - Length of the outer rolling window
L <- 500    #L - Length of the rolling window 
r <- 100    #r - Length of the training set to learn G matrix
I <- 398    #I - Length of inner rolling window for training G
m <- 4      #m - Number of bottom level series
B <- 2500   #B - Number of random numbers generated from the predictive distributions
H <- 3      #H - Forecast horizons
