### Try out DTW with a 1 dimensional signal


## Setup
################################################################################
setwd('C:/Users/lchco/OneDrive/Documents/School/CSU/Spring_2024/STAT472/physical+therapy+exercises+dataset/')

library(dtw)
library(tidyverse)
library(ggplot2)

## Functions
plottingFormat <- function(template_session) {
  # Takes a template_session.txt or test.txt and turns it into a longer format
  acc <- template_session %>%
    select(time.index, acc_x, acc_y, acc_z) %>%
    pivot_longer(cols = starts_with('acc_'), 
                 names_to = 'coord', 
                 names_prefix = 'acc_', 
                 values_to = 'acc') 
  gyr <- template_session %>%
    select(time.index, gyr_x, gyr_y, gyr_z) %>%
    pivot_longer(cols = starts_with('gyr_'), 
                 names_to = 'coord', 
                 names_prefix = 'gyr_', 
                 values_to = 'gyr') 
  mag <- template_session %>%
    select(time.index, mag_x, mag_y, mag_z) %>%
    pivot_longer(cols = starts_with('mag_'), 
                 names_to = 'coord', 
                 names_prefix = 'mag_', 
                 values_to = 'mag') 
  list(acc, gyr, mag) %>% reduce(full_join, by=c('time.index', 'coord'))
}

getMetric <- function(exerciseType, sensorUnit, metric) {
  # Extracts a specific metric and exercise type from all sessions
  e <- exerciseType 
  u <- sensorUnit
  
  # Read data and join together
  tTime1 <- read.table(file=paste('s',1,'/e',e,'/template_times.txt', sep=''), header=TRUE, sep=';')
  tSesh1 <- read.table(file=paste('s',1,'/e',e,'/u',u,'/template_session.txt', sep=''), header=TRUE, sep=';') %>%
    plottingFormat() %>%
    select(time.index, coord, metric) %>%
    mutate(exercise1 = 
             ifelse(tTime1$start[1] < time.index & time.index < tTime1$end[1], 1, 0) +
             ifelse(tTime1$start[2] < time.index & time.index < tTime1$end[2], 2, 0) +
             ifelse(tTime1$start[3] < time.index & time.index < tTime1$end[3], 3, 0)) %>%
    rename(acc1 = metric)
  tTime2 <- read.table(file=paste('s',2,'/e',e,'/template_times.txt', sep=''), header=TRUE, sep=';')
  tSesh2 <- read.table(file=paste('s',2,'/e',e,'/u',u,'/template_session.txt', sep=''), header=TRUE, sep=';') %>%
    plottingFormat() %>%
    select(time.index, coord, metric) %>%
    mutate(exercise2 = 
             ifelse(tTime2$start[1] < time.index & time.index < tTime2$end[1], 1, 0) +
             ifelse(tTime2$start[2] < time.index & time.index < tTime2$end[2], 2, 0) +
             ifelse(tTime2$start[3] < time.index & time.index < tTime2$end[3], 3, 0)) %>%
    rename(acc2 = metric)
  tTime3 <- read.table(file=paste('s',3,'/e',e,'/template_times.txt', sep=''), header=TRUE, sep=';')
  tSesh3 <- read.table(file=paste('s',3,'/e',e,'/u',u,'/template_session.txt', sep=''), header=TRUE, sep=';') %>%
    plottingFormat() %>%
    select(time.index, coord, metric) %>%
    mutate(exercise3 = 
             ifelse(tTime3$start[1] < time.index & time.index < tTime3$end[1], 1, 0) +
             ifelse(tTime3$start[2] < time.index & time.index < tTime3$end[2], 2, 0) +
             ifelse(tTime3$start[3] < time.index & time.index < tTime3$end[3], 3, 0)) %>%
    rename(acc3 = metric)
  tTime4 <- read.table(file=paste('s',4,'/e',e,'/template_times.txt', sep=''), header=TRUE, sep=';')
  tSesh4 <- read.table(file=paste('s',4,'/e',e,'/u',u,'/template_session.txt', sep=''), header=TRUE, sep=';') %>%
    plottingFormat() %>%
    select(time.index, coord, metric) %>%
    mutate(exercise4 = 
             ifelse(tTime4$start[1] < time.index & time.index < tTime4$end[1], 1, 0) +
             ifelse(tTime4$start[2] < time.index & time.index < tTime4$end[2], 2, 0) +
             ifelse(tTime4$start[3] < time.index & time.index < tTime4$end[3], 3, 0)) %>%
    rename(acc4 = metric)
  tTime5 <- read.table(file=paste('s',5,'/e',e,'/template_times.txt', sep=''), header=TRUE, sep=';')
  tSesh5 <- read.table(file=paste('s',5,'/e',e,'/u',u,'/template_session.txt', sep=''), header=TRUE, sep=';') %>%
    plottingFormat() %>%
    select(time.index, coord, metric) %>%
    mutate(exercise5 = 
             ifelse(tTime5$start[1] < time.index & time.index < tTime5$end[1], 1, 0) +
             ifelse(tTime5$start[2] < time.index & time.index < tTime5$end[2], 2, 0) +
             ifelse(tTime5$start[3] < time.index & time.index < tTime5$end[3], 3, 0)) %>%
    rename(acc5 = metric)
  tSesh_all_subjects <- list(tSesh1, tSesh2, tSesh3, tSesh4, tSesh5) %>%
    reduce(full_join, by=c('time.index', 'coord'))
  tSesh_all_subjects
}

get1D_from_metric <- function(metricdf, axis, style) {
  # Extracts a single coordinate from a single exercise performed in a specific style from a metric data frame
  c <- axis
  s <- style
  
  tSesh_all_subjects %>%
    filter(coord == c, 
           exercise1==s |
             exercise2==s |
             exercise3==s |
             exercise4==s |
             exercise5==s) -> filtered
  filtered %>%
    mutate(acc1 = ifelse(exercise1==s, acc1, NA),
           acc2 = ifelse(exercise2==s, acc2, NA),
           acc3 = ifelse(exercise3==s, acc3, NA),
           acc4 = ifelse(exercise4==s, acc4, NA),
           acc5 = ifelse(exercise5==s, acc5, NA)) %>%
    mutate(acc1 = c(na.omit(acc1), rep(NA, sum(is.na(acc1)))),
           acc2 = c(na.omit(acc2), rep(NA, sum(is.na(acc2)))),
           acc3 = c(na.omit(acc3), rep(NA, sum(is.na(acc3)))),
           acc4 = c(na.omit(acc4), rep(NA, sum(is.na(acc4)))),
           acc5 = c(na.omit(acc5), rep(NA, sum(is.na(acc5))))) %>%
    select(-c(exercise1, exercise2, exercise3, exercise4, exercise5)) %>% 
    filter(!is.na(acc1) | !is.na(acc2) | !is.na(acc3) | !is.na(acc4) | !is.na(acc5)) -> filtered
  filtered
}

# Get data
exerciseType <- 1 # Which exercise they are doing, 1:8
sensorUnit <- 1 # Which sensor, 1:5
metric <- 'acc' # Choose from acc, gyr, mag
axis <- 'x' # Choose from x, y, z
style <- 1 # Way the exercise is performed, 1:3

df <- get1D_from_metric(getMetric(exerciseType, sensorUnit, metric), axis, style) 
################################################################################



## Doing DTW
################################################################################
# Plot data
df %>%
  ggplot() +
  geom_line(aes(x=time.index, y=acc1), col='orange', size=1.5) +
  geom_line(aes(x=time.index, y=acc2), col='dodgerblue', size=1.5) +
#  geom_line(aes(x=time.index, y=acc3), col='red2', size=1.5) +
#  geom_line(aes(x=time.index, y=acc4), col='green2', size=1.5) +
#  geom_line(aes(x=time.index, y=acc5), size=1.5) +
  labs(title=paste('Exercise type ',exerciseType,
                   ', Exercise style ',style,
                   ', Unit ',sensorUnit,
                   ', ',axis,' coordinate, Accelerometer', sep=''))

alignment <- dtw(na.omit(df$acc1), na.omit(df$acc2), k=T)

alignment %>%
  plot(type='threeway', off=1, match.lty=2)

dtw(na.omit(df$acc1), na.omit(df$acc2), k=T, step=typeIIIc) %>%
  dtwPlotDensity()

################################################################################






