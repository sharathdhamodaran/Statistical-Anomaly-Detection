changepointDetection <- function(windSeedData,
                                 statTest,
                                 avgrunLength,
                                 startObs) {
  # Computes the sequential change point detection within a given time period
  #
  # Args:
  #   windSeedData: The vector list of target variable
  #   statTest: the statistical test to use 
  #   avgrunLength: average number of obs before a false positive
  #   startObs: Number of observations after which monitoring begins
  #
  # Returns:
  #   A vector of change point locations
  
  # The observations after which the changepoint will be detected
  detectionTimes <- numeric()  
  # The best estimate of the changepoint location
  changepoints <- numeric()  
  # intialize
  i <- 0 
  # create a change point model S4 object
  changepointModel <- makeChangePointModel(cpmType = statTest,
                                           ARL0 = avgrunLength,
                                           startup = startObs)
  # changepoint detection process
  while (i < length(windSeedData)) {
    i <- i + 1
    # process each observation in turn
    changepointModel <- processObservation(changepointModel,windSeedData[i])
    # if a change has been found, log it, and reset the CPM
    if (changeDetected(changepointModel) == TRUE) { 
      detectionTimes <- c(detectionTimes,i)
      # the change point estimate is the maximum statistic
      teststatSequence <- getStatistics(changepointModel)
      tau <- which.max(teststatSequence)
      
      if (length(changepoints) > 0) {
        tau <- tau + changepoints[length(changepoints)]
      }
      changepoints <- c(changepoints,tau)
      # reset the CPM
      changepointModel <- cpmReset(changepointModel) 
      # resume monitoring from the obs following the changepoint
      i <- tau 
    }
  }
  
  return(changepoints)
}