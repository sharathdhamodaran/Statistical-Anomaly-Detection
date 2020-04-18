#-------------------------------------------------------------------------------
# Script Name: Statistical Anomaly Detection in wind speed data
# Author: Sharath Kumar Dhamodaran
#-------------------------------------------------------------------------------

# setup
# load the libraries
source("scripts/libraries.R")
source("scripts/functions/functions.R")

# inputs to the script ---------------------------------------------------------
constantVal <- 10
windowsizeMA <- 3

# Data extraction --------------------------------------------------------------

#  make a file path by specifying the location
filesLocation <- here("data")

# look for all the .csv files and makes a list of them called "files"
files <- dir(filesLocation, pattern = "*.csv")
files

# bind all the .csv's into one dataframe
windspeedData <- files %>%
  # read in all the files, appending the path before the filename
  map(~ read_csv(file.path(filesLocation, .))) %>%
  # reduce with rbind into one dataframe
  reduce(rbind)

# peak into the windspeed data
head(windspeedData)

# Data Manipulation ------------------------------------------------------------

windspeedData <- windspeedData %>%
  # multiply the windspeed and target by a constant
  mutate(AVERAGE = AVERAGE * constantVal, TARGET = TARGET * constantVal) %>%
  # change the datetime format for analysis
  mutate(DATETIME = as.POSIXct(DATETIME, format = "%m/%d/%Y %H:%M"))

# visualize windspeeds at different locations
windspeedData %>%
  ggplot(aes(x = DATETIME, y = AVERAGE, color = LOCATION)) +
  geom_line(size = 0.8) +
  labs(
    title = "Wind speeds by location",
    y = "Average wind speed (meters/second)",
    x = ""
  ) +
  facet_wrap(~LOCATION, ncol = 2, scale = "free") +
  theme_few() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(
      colour = "blue", hjust = 0.5,
      size = 15, face = "bold"
    ),
    strip.text = element_text(size = 10, face = "bold")
  ) +
  scale_color_viridis(discrete = TRUE) +
  scale_x_datetime(
    breaks = date_breaks("2 days"),
    labels = date_format("%b-%d\n%H:%M")
  )
ggsave(here("outputs", "plots", "windspeeds_data.png"))

# Statistical Checks -----------------------------------------------------------

# checking for trends in the dataset using moving average for every 30 minutes
windspeedData %>%
  tq_mutate(
    select = AVERAGE,
    mutate_fun = rollapply,
    width = windowsizeMA,
    align = "right",
    FUN = mean,
    na.rm = TRUE,
    col_rename = "rollavg"
  ) %>%
  ggplot(aes(x = DATETIME, color = LOCATION)) +
  geom_line(aes(y = rollavg), size = 1) +
  facet_wrap(~LOCATION, ncol = 2, scale = "free") +
  labs(
    title = "Trend analysis of wind speeds using moving average",
    y = "Average wind speed (meters/second)",
    x = ""
  ) +
  theme_few() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(
      colour = "blue", hjust = 0.5,
      size = 15, face = "bold"
    ),
    strip.text = element_text(size = 10, face = "bold")
  ) +
  scale_color_viridis(discrete = TRUE) +
  scale_x_datetime(
    breaks = date_breaks("2 days"),
    labels = date_format("%b-%d\n%H:%M")
  )
ggsave(here("outputs", "plots", "windspeeds_trends.png"))

# assessing stationarity of data using augmented dickey-fuller (adf) test
windspeedData <- windspeedData %>%
  group_by(LOCATION) %>%
  # add the p-values of wind speeds at different locations to the dataset
  mutate(PVALUE = adf.test(AVERAGE)$p.value)

# Outlier Detection ------------------------------------------------------------

# visualization of outliers using boxplots
windspeedData %>%
  ggplot(aes(x = LOCATION, y = AVERAGE, color = LOCATION)) +
  geom_boxplot(
    outlier.colour = "red", outlier.shape = 8,
    outlier.size = 4
  ) +
  labs(
    title = "Boxplot of wind speeds by location",
    x = "",
    y = "Average wind speed (meters/second)"
  ) +
  theme_few() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(
      colour = "blue", hjust = 0.5,
      size = 15, face = "bold"
    ),
    strip.text = element_text(size = 10, face = "bold")
  ) +
  scale_color_viridis(discrete = TRUE)
ggsave(here("outputs", "plots", "windspeeds_boxplots.png"))

# visualization of outliers using statistical process control (spc)
windspeedData %>%
  group_by(LOCATION) %>%
  # avoid analysis on dates in random order
  arrange(DATETIME) %>%
  # compute the upper control limit (UCL) using (mean + 3 SD)
  mutate(UCL = mean(AVERAGE, na.rm = TRUE) + 3 * sd(AVERAGE, na.rm = TRUE)) %>%
  ggplot(aes(x = DATETIME, y = AVERAGE)) +
  geom_point(
    alpha = 0.6, position = position_jitter(w = 0.05, h = 0.0),
    aes(colour = (AVERAGE < UCL)), size = 2
  ) +
  geom_hline(aes(yintercept = UCL, linetype = "UCL"),
    color = "red", size = 1
  ) +
  geom_text(aes(
    y = UCL, x = windspeedData$DATETIME[30],
    label = paste("UCL = ", round(UCL, 3)),
    hjust = 0, vjust = 1.5
  )) +
  facet_wrap(~LOCATION, ncol = 2, scale = "free") +
  labs(
    title = "Outlier Detection in wind speeds using SPC",
    y = "Average wind speed (meters/second)",
    x = ""
  ) +
  theme_few() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(
      colour = "blue", hjust = 0.5,
      size = 15, face = "bold"
    ),
    strip.text = element_text(size = 10, face = "bold")
  ) +
  scale_color_viridis(discrete = TRUE) +
  scale_x_datetime(
    breaks = date_breaks("2 days"),
    labels = date_format("%b-%d\n%H:%M")
  )
ggsave(here("outputs", "plots", "windspeeds_spc.png"))

# Modeling & Visualization------------------------------------------------------

# set a random number to obtain reproducible results
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
  if (nrow(windspeeddataSubset) == 0) {
    next
  }
  # identify the changepoint location indices
  changepointLocations <- changepointDetection(
    windspeeddataSubset$AVERAGE,
    statTest,
    avgrunLength,
    startObs
  )
  # exit out of the loop if no changepoints are found
  if (!length(changepointLocations)) {
    next
  }
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
    geom_point(
      data = windspeeddataSubset[
        which(windspeeddataSubset$IS_CHANGEPOINT %in% "CP"),
      ],
      aes(x = DATETIME, y = AVERAGE), size = 4, show.legend = FALSE
    ) +
    geom_vline(
      data = windspeeddataSubset[
        which(windspeeddataSubset$IS_CHANGEPOINT %in% "CP"),
      ],
      aes(xintercept = DATETIME),
      linetype = 2, col = "brown", size = 1
    ) +
    geom_label_repel(aes(DATETIME, AVERAGE,
      label = IS_CHANGEPOINT,
    ),
    size = 4,
    fontface = "bold",
    box.padding = 0.5,
    point.padding = 0.5,
    segment.color = "red",
    segment.size = 0.5,
    arrow = arrow(length = unit(0.01, "npc"))
    ) +
    labs(
      title = "Non-parametric sequential changepoint detection in windspeeds",
      subtitle = paste(
        "Statistical test = ", statTest, "\n",
        "ARL0 = ", avgrunLength, "\n",
        "Changepoints = 'CP'"
      ),
      y = "Average wind speed (meters/second)",
      x = ""
    ) +
    theme_few() +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 10, face = "bold"),
      plot.title = element_text(
        colour = "blue", hjust = 0.5,
        size = 15, face = "bold"
      ),
      plot.subtitle = element_text(
        colour = "red", hjust = 0.5,
        size = 12, face = "bold"
      ),
      strip.text = element_text(size = 10, face = "bold")
    ) +
    scale_color_viridis(discrete = TRUE) +
    scale_x_datetime(
      breaks = date_breaks("2 days"),
      labels = date_format("%b-%d\n%H:%M")
    )

  print(plotCP)
  ggsave(here("outputs", "plots", sprintf(
    "Changepoints_%s.png",
    gsub(" ", "_", statTest)
  )))

  Sys.sleep(0)
}

# combine all the changepoint dataframes into a final table
changepointFinal <- bind_rows(changepointsData)
# clear all workspace objects and retain only what we want
rm(list = setdiff(ls(), "changepointFinal"))