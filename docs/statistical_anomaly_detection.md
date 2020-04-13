---
title: "Statistical Anomaly Detection in wind speed data"
author: "Sharath Kumar Dhamodaran"
---

# `Statistical Anomaly Detection`

Wind turbines have always fascinated me with the amount of power they generate. Wind turbine maintenance is a tedious task which is also associated with unnecessary downtimes due to manual nature of work. Wind Speed is one of the critical data that wind analysts use for maintenence tasks. I was curious to see if I could design an automated system that could alert operators in real-time with abrupt changes in wind speed. Once developed, the hope is that it will allow better prioritization of maintenance tasks. 

## `Data Generation & Extraction`

Wind speed data (meters per second) is generated from a device called Anemometer. The data is collected at 4 different locations in Tamil Nadu, India. For confidential reasons, I extraceted only a fraction of the original dataset for this analysis. They are all stored as `.csv's` and stored in the `data` folder of this project.

```r
# load the libraries 
source("scripts/libraries.R")
source("scripts/functions/functions.R")

#  make a file path by specifying the location
filesLocation <- here("data")

# look for all the .csv files and makes a list of them called "files"
files <- dir(filesLocation, pattern = "*.csv")

files
[1] "windspeed_location_A.csv" "windspeed_location_B.csv" "windspeed_location_C.csv" "windspeed_location_D.csv"
```

Let's look at the data and understand the variables. 

```r
# bind all the .csv's into one dataframe
windspeedData <- files %>%
  # read in all the files, appending the path before the filename
  map(~ read_csv(file.path(filesLocation, .))) %>%
  # reduce with rbind into one dataframe
  reduce(rbind) 

# peak into the windspeed data
head(windspeedData)
# A tibble: 6 x 5
  DATETIME       ID    LOCATION TARGET AVERAGE
  <chr>          <chr> <chr>     <dbl>   <dbl>
1 7/29/2016 1:59 4E1GS A           0.4   0.553
2 7/29/2016 2:09 K93EY A           0.4   0.464
3 7/29/2016 2:19 L52UC A           0.4   0.482
4 7/29/2016 2:29 L52V4 A           0.4   0.451
5 7/29/2016 2:39 K93EU A           0.4   0.410
6 7/29/2016 2:49 L52TU A           0.4   0.444
```
The wind speeds are averaged over 10 minutes interval. The `TARGET` variable specifies the optimal wind speed per location. This knowledge is incorporated by wind analysts.   

## `Data Manipulation`

For better visualization, I multiplied `TARGET` & `AVERAGE` by a constant. This is an optional step. 

```r
constantVal <- 10
windspeedData <- windspeedData %>%
  # multiply the windspeed and target by a constant
  mutate(AVERAGE = AVERAGE * constantVal, TARGET = TARGET * constantVal) %>% 
  # change the datetime format for analysis
  mutate(DATETIME = as.POSIXct(DATETIME, format = "%m/%d/%Y %H:%M"))
  ```

Let's visualize the data by location
```r
windspeedData %>%
  ggplot(aes(x = DATETIME, y = AVERAGE, color = LOCATION)) +
  geom_line(size = 0.8) +
  labs(title = "Wind speeds by location", 
       y = "Average wind speed (meters/second)",
       x = "") +
  facet_wrap(~ LOCATION, ncol = 2, scale = "free") +
  theme_few() +
  theme(legend.position = "none",
        axis.text = element_text(size = 10, face = "bold"), 
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(colour = "blue", hjust = 0.5,
                                  size = 15, face = "bold"),
        strip.text = element_text(size = 10, face = "bold")) +
  scale_color_viridis(discrete = TRUE) +
  scale_x_datetime(breaks = date_breaks("2 days"),
                   labels = date_format("%b-%d\n%H:%M"))
```
![windspeed data](/outputs/plots/windspeeds_data.png)

## `Statistical Checks`

Let's understand the changes in trends by visualizing the data. We can use rolling functions to better understand how trends are changing over time. I use a window size of 30 min intervals to compute the moving average. 

```R
windspeedData %>%
  tq_mutate(select = AVERAGE, 
            mutate_fun = rollapply, 
            width = 3, 
            align = "right", 
            FUN = mean, 
            na.rm = TRUE, 
            col_rename = "rollavg") %>%
  ggplot(aes(x = DATETIME, color = LOCATION)) +
  geom_line(aes(y = rollavg), size = 1) +
  facet_wrap(~ LOCATION, ncol = 2, scale = "free") +
  labs(title = "Trend analysis of wind speeds using moving average", 
       y = "Average wind speed (meters/second)",
       x = "") +
  theme_few() +
  theme(legend.position = "none",
        axis.text = element_text(size = 10, face = "bold"), 
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(colour = "blue", hjust = 0.5,
                                  size = 15, face = "bold"),
        strip.text = element_text(size = 10, face = "bold")) +
  scale_color_viridis(discrete = TRUE) +
  scale_x_datetime(breaks = date_breaks("2 days"),
                   labels = date_format("%b-%d\n%H:%M"))
```
![trends](/outputs/plots/windspeeds_trends.png)

What we are looking for are points where the fast trend is above (has momentum) or below (is slowing) the slow trend. We can see that `Location B` is gaining a strong upward momentum, while `Location C` is clearly showing a trend. Let's verify which series are stationary using the `Augmented Dickey-Fuller test` (ADF)

```r
windspeedData <- windspeedData %>%
  group_by(LOCATION) %>%
  # add the p-values of wind speeds at different locations to the dataset
  mutate(PVALUE = adf.test(AVERAGE)$p.value)

windspeedData %>%
  select(LOCATION, PVALUE) %>%
  group_by(LOCATION) %>%
  unique()

  # A tibble: 4 x 2
# Groups:   LOCATION [4]
  LOCATION PVALUE
  <chr>     <dbl>
1 A        0.01  
2 B        0.01  
3 C        0.0795
4 D        0.01
``` 
Statistical tests make strong assumptions about our data and so the result must be interpreted carefully. ADF test provides a quick check and confirmatory evidence that our time series is stationary or non-stationary. We can interpret the results using the p-value from the test. A `p-value <= 0.05` suggests we reject the null hypothesis (stationary), otherwise a `p-value > 0.05` suggests we fail to reject the null hypothesis (non-stationary). 

Looking at the results, only the series `Location C` appears to be non-stationary. 

## `Outlier Detection`

Outliers are type of anomalies that are several standard deviations away from the mean of a distribution. They can be used to detect a major change quickly. There are many ways to handle outliers. 

- `Inclusion`: Best when assessing the effects of outliers. Run analyses both with and without the outlier
- `Accomodation`: Changing the outlier to mean, median, or mode. Might bias the dataset, but not nearly as much as the outlier
- `Replacement`: Keeping the outlier using “robust” statistical tests. Use tests that are less sensitive to outliers
- `Deletion`: Removing the outlier from the dataset. Best when assessing general trends

Visual analysis is one of the simplest and old-fashioned ways to detect outliers.  

### `Boxplots`
```r
windspeedData %>%
  ggplot(aes(x = LOCATION, y = AVERAGE, color = LOCATION)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8,
               outlier.size = 4) + 
  labs(title = "Boxplot of wind speeds by location", 
       x = "",
       y = "Average wind speed (meters/second)") +
  theme_few() +
  theme(legend.position="none",
        axis.text = element_text(size = 10, face = "bold"), 
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(colour = "blue", hjust = 0.5,
                                  size = 15, face = "bold"),
        strip.text = element_text(size = 10, face = "bold")) +
  scale_color_viridis(discrete = TRUE)
```
![boxplots](/outputs/plots/windspeeds_boxplots.png)

### `Statistical Process Control (SPC)`
```r
windspeedData %>%
  group_by(LOCATION) %>%
  # avoid analysis on dates in random order
  arrange(DATETIME) %>% 
  # compute the upper control limit (UCL) using (mean + 3 SD)
  mutate(UCL = mean(AVERAGE, na.rm = TRUE) + 3 * sd(AVERAGE, na.rm = TRUE)) %>%
  ggplot(aes(x = DATETIME,y = AVERAGE)) + 
  geom_point(alpha = 0.6, position = position_jitter(w = 0.05, h = 0.0), 
             aes(colour = (AVERAGE < UCL)), size = 2) +
  geom_hline(aes(yintercept = UCL, linetype = "UCL"), 
             color = 'red', size = 1) +      
  geom_text(aes(y = UCL,x = windspeedData$DATETIME[30],
                label = paste("UCL = ", round(UCL, 3)),
                hjust = 0, vjust = 1.5)) +
  facet_wrap(~ LOCATION, ncol = 2, scale = "free") +
  labs(title = "Outlier Detection in wind speeds using SPC",
       y = "Average wind speed (meters/second)",
       x = "") +
  theme_few() +
  theme(legend.position = "none",
        axis.text = element_text(size = 10, face = "bold"), 
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(colour = "blue", hjust = 0.5,
                                  size = 15, face = "bold"),
        strip.text = element_text(size = 10, face = "bold")) +
  scale_color_viridis(discrete = TRUE) +
  scale_x_datetime(breaks = date_breaks("2 days"),
                   labels = date_format("%b-%d\n%H:%M"))
```
![boxplots](/outputs/plots/windspeeds_spc.png)

# `Modeling`

## `Changepoint Detection`

Unlike outlier detection, change point analysis is used for identifying points where some aspect of data of interest (location, scale, or distribution) change. They are used for detecting subtle and small changes. 

### `Process`
- Definitions & Assumptions


- Detection
  - Changepoints are determined by calculating a vector of the sum of differences between each data point and the mean of all data points. 
  - The maximum or minimum point on this vector indicate the location of a changepoint. 
  - This process is repeated to find additional changepoints within each newly identified region

### `Questions to ask before implementing a changepoint method`
- what is the desired detection method? - Online (Real-time) or Offline (Batch)
- How many changepoints? - Single or Mulitple
- Where was it identified? - Location and Time
- What type of change? - Location (mean) or Scale (variance) or Distribution shift

I decided to design a system that was real-time, able to detect multiple changepoints and finally it should be computationally efficient. There are several R packages for changepoint analysis. [Here](http://www.changepoint.info/software.html) is a repository of different R packages with thier release year. 

I am using the `cpm` package in R which has functions and methods for non-parametric sequential changepoint detection. For more information on the `cpm` package, look [here](http://www2.uaem.mx/r-mirror/web/packages/cpm/vignettes/cpm.pdf)

`functions.R` has the `changepointDetection` function that returns the changepoints in the dataset
```r
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
````
The `algorithm `works as follows

- Observations are received and processed sequentially over time 
- When each observation has been received, a decision is made whether a change has occurred based only on the observations which have been received so far 
- If no change is flagged, then next observation in the sequence is processed
- Whenever a CP is detected, the detector is simply restarted from the following observation in the sequence

```r
set.seed(300) 
# choose a desired cpm type
statTest <- "Mann-Whitney"  
# optimal average run length before a false positive occurs
avgrunLength <- 500  
# set monitoring to begin after 20 data points
startObs <- 20  

# dataframe that lists the locations and thier p-values
windspeedLocations <- windspeedData %>%
  select(LOCATION, PVALUE) %>%
  group_by(LOCATION) %>%
  unique()

# create a list to store the dataframe results
changepointsData <- list()  
for (l in 1:(nrow(windspeedLocations))) {
  cat(l, "\n")
  location <- as.character(windspeedLocations$LOCATION[[l]])
  # create a subset of data by location
  windspeeddataSubset <- windspeedData %>%
    filter(LOCATION == location) %>%
    mutate(SEQ = c(1:n())) %>%
    # filter out any stationary datasets using p-values
    filter(PVALUE > 0.05)
  # exit out of the loop if the below conidition is true
  if(nrow(windspeeddataSubset) == 0)
    next
  # identify the changepoint location indices
  changepointLocations <- changepointDetection(windspeeddataSubset$AVERAGE,
                                               statTest,
                                               avgrunLength,
                                               startObs)
  # exit out of the loop if no changepoints are found
  if(!length(changepointLocations))
    next
  # create a table with all the changepoints
  changepointTable <- windspeeddataSubset %>%
    filter(SEQ %in% changepointLocations) %>%
    mutate(IS_CHANGEPOINT = "CP") %>%
    mutate(AVG_RUNLENGTH = avgrunLength) %>%
    mutate(STAT_METHOD = statTest)
  # merge the changepoint table with the subset dataframe for plotting
  windspeeddataSubset <- windspeeddataSubset %>%
    left_join(changepointTable)
  # store each dataframe as a list
  changepointsData[[l]] <- windspeeddataSubset
  # plotting of chnagepoints in the dataset
  plotCP <- windspeeddataSubset %>%
    ggplot(aes(x = DATETIME, y = AVERAGE)) + 
    geom_point(size = 2) +
    geom_smooth(col = "blue", size = 1, se = FALSE) +
    geom_point(data = windspeeddataSubset[
      which(windspeeddataSubset$IS_CHANGEPOINT %in%  "CP"),],
               aes(x = DATETIME, y = AVERAGE), size = 4, show.legend = FALSE) +
    geom_vline(data = windspeeddataSubset[
      which(windspeeddataSubset$IS_CHANGEPOINT %in% "CP"),],
               aes(xintercept = DATETIME),
               linetype = 2, col = "brown",size = 1) +
    geom_label_repel(aes(DATETIME, AVERAGE,
                         label = IS_CHANGEPOINT,),
                     size = 4,
                     fontface = 'bold',
                     box.padding = 0.5,
                     point.padding = 0.5,
                     segment.color = 'red',
                     segment.size = 0.5,
                     arrow = arrow(length = unit(0.01, 'npc'))) +
    labs(title = "Non-parametric sequential changepoint detection in windspeeds"
         ,subtitle = paste("Statistical test = ", statTest, "\n",
                           "ARL0 = ",avgrunLength, "\n",
                           "Changepoints = 'CP'")
         ,y = "Average wind speed (meters/second)"
         ,x = "") +
    theme_few() +
    theme(legend.position = "none",
          axis.text = element_text(size = 10, face = "bold"), 
          axis.title = element_text(size = 10, face = "bold"),
          plot.title = element_text(colour = "blue", hjust = 0.5,
                                    size = 15, face = "bold"),
          plot.subtitle = element_text(colour = "red", hjust = 0.5,
                                       size = 12, face = "bold"),
          strip.text = element_text(size = 10, face = "bold")) +
    scale_color_viridis(discrete = TRUE) +
    scale_x_datetime(breaks = date_breaks("2 days"),
                     labels = date_format("%b-%d\n%H:%M"))
  
  print(plotCP)
 # ggsave(here("outputs", "plots", sprintf("Changepoints_%s.png", 
 #                                         gsub(" ", "_", statTest))))
  
  Sys.sleep(0)
}
```
![changepoints](/outputs/plots/Changepoints_Mann-Whitney.png)

The above plot shows the `Mann-Whitney` test statistic to find location changes in the dataset. I also played around with other statistical methods while varying the average run length values. 

# `Conclusion`

Changepoint detection is a very effective technique in identifying small and subtle changes in the dataset. 

## `Results`

Here are my observations 
- `Cramer-von-Misses (CvM)` vs `Kolmogorov-Smirnov (KS)` 
  - Best choices for all change types
  - Unaffected by the choice of  
  - `CvM` performs slightly better than `KS` and is superior for detecting location shifts 
  - `KS` detects scale changes faster than the `CvM` 
- `Lepage (LP)` vs `Mann-Whitney (MW)` vs `Mood`
  - `MW` > `LP` when change magnitude is small
    - `LP` is looking at both scale and location
    - `LP` = (`MW`)^2 + (`Mood`)^2
  - `LP` > `MW` when change magnitude is large
    - Large change has effect on mean & std dev
  - `Mood` > `LP` regardless of scale shift magnitude


## `Key Takeaways for changepoint detection`

- `Not a “silver bullet” for all anomaly detection problems`
  - Powerful detecting small as well as sustained changes over a long period of time
  - Takes long time to detect if the drift occurs after very few observations
  - Cannot detect isolated abnormal points
- `Scalability`
  - Online methods scale well because of constant memory & computation time
- `Value`
  - Simple, easily interpretable and has the ability to automate difficult processes 
  - Specifies the exact point where a change occurred whereas visual inspection can only capture the existence of such change
  - Take less time to implement and add value faster than Machine Learning projects
