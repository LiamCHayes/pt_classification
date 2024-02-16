### Compute the average signal over all sessions for each exercise and sensor 

## Setup
################################################################################
setwd('C:/Users/lchco/OneDrive/Documents/School/CSU/Spring_2024/STAT472/physical+therapy+exercises+dataset/')

library(dtw)
library(tidyverse)
library(ggplot2)

# Functions
getRep <- function(template_session, template_time, type) {
  t <- template_time %>%
    filter(execution.type == type)
  template_session %>%
    filter(time.index >= t$start & time.index <= t$end)
}

plottingFormat <- function(template_session) {
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
################################################################################


## Get data in the right format
################################################################################
e <- 7 # Exercise type, 1:8
u <- 1 # Sensor unit, 1:5

# Get all sessions given an exercise and a sensor
session_list <- list()
for (i in 1:5) {
  df <- read.table(file=paste('s',i,'/e',e,'/u',u,'/template_session.txt', sep=''), header=TRUE, sep=';') %>%
    getRep(read.table(file=paste('s',i,'/e',e,'/template_times.txt', sep=''), header=TRUE, sep=';'), 2) %>%
    select(-time.index)
  colnames(df) <- paste(colnames(df), i, sep='_')
  session_list <- c(session_list, 
                    df)
}

# Stretch two acceleration x vectors to be the same length with dtw
alignment <- dtw(session_list$acc_x_1, session_list$acc_x_2, k=T, step=typeIIIc)
alignment %>% plot(type='two')

new_acc_x_2 <- rep(0, length(alignment$index2))
new_acc_x_2[alignment$index1] <- session_list$acc_x_2[alignment$index2]

data.frame(i=seq(1,length(new_acc_x_2)), x1=session_list$acc_x_1, x2=new_acc_x_2) %>%
  ggplot() +
  geom_line(aes(x=i, y=x1)) +
  geom_line(aes(x=i, y=x2), col='red')

# Align all acceleration x vectors and take average
acc_x <- session_list[seq(1,45,by=9)]
m <- which(lengths(acc_x)==max(lengths(acc_x)))[1]
vec1 <- acc_x[[m]]
acc_x <- acc_x[-m]
while (length(acc_x) >= 1) {
  m <- which(lengths(acc_x)==max(lengths(acc_x)))[1]
  vec2 <- acc_x[[m]]
  acc_x <- acc_x[-m]
  
  alignment <- dtw(vec1, vec2, k=T, step=typeIIIc)
  new_vec2 <- rep(0, length(alignment$index2))
  new_vec2[alignment$index1] <- vec2[alignment$index2]
  
  vec1 <- (vec1 + new_vec2)/2
}
rm(acc_x)

dtw(vec1, session_list$acc_x_1)$distance
dtw(vec1, session_list$acc_x_2)$distance
dtw(vec1, session_list$acc_x_3)$distance
dtw(vec1, session_list$acc_x_4)$distance
dtw(vec1, session_list$acc_x_5)$distance

# Create a function to average over all sessions for a specific exercise, exercise type, and sensor
avgExercise <- function(exercise, sensor, exerciseStyle) {
  e <- exercise
  u <- sensor
  session_list <- list()
  for (i in 1:5) {
    df <- read.table(file=paste('s',i,'/e',e,'/u',u,'/template_session.txt', sep=''), header=TRUE, sep=';') %>%
      getRep(read.table(file=paste('s',i,'/e',e,'/template_times.txt', sep=''), header=TRUE, sep=';'), exerciseStyle) %>%
      select(-time.index)
    colnames(df) <- paste(colnames(df), i, sep='_')
    session_list <- c(session_list, df)
  }
  
  df <- list()
  for (i in 1:9) {
    metrics <- session_list[seq(i,45,by=9)]
    m <- which(lengths(metrics)==max(lengths(metrics)))[1]
    vec1 <- metrics[[m]]
    metrics <- metrics[-m]
    while (length(metrics) >= 1) {
      m <- which(lengths(metrics)==max(lengths(metrics)))[1]
      vec2 <- metrics[[m]]
      metrics <- metrics[-m]
      
      alignment <- tryCatch(
        {
          dtw(vec1, vec2, k=T, step=typeIIIc)
        },
        error = function(e) {
          vec2 <- c(0,vec2,0)
          dtw(vec1, vec2, k=T, step=typeIIIc)
        }
      ) 
      new_vec2 <- rep(0, length(alignment$index2))
      new_vec2[alignment$index1] <- vec2[alignment$index2]
      
      vec1 <- (vec1 + new_vec2)/2
    }
    df <- c(df, list(vec1))
  }
  names(df) <- c(paste('acc_x_e',e,'_u',u,'_es',exerciseStyle,sep=''), 
                 paste('acc_y_e',e,'_u',u,'_es',exerciseStyle,sep=''), 
                 paste('acc_z_e',e,'_u',u,'_es',exerciseStyle,sep=''), 
                 paste('gyr_x_e',e,'_u',u,'_es',exerciseStyle,sep=''), 
                 paste('gyr_y_e',e,'_u',u,'_es',exerciseStyle,sep=''), 
                 paste('gyr_z_e',e,'_u',u,'_es',exerciseStyle,sep=''), 
                 paste('mag_x_e',e,'_u',u,'_es',exerciseStyle,sep=''), 
                 paste('mag_y_e',e,'_u',u,'_es',exerciseStyle,sep=''), 
                 paste('mag_z_e',e,'_u',u,'_es',exerciseStyle,sep=''))
  df %>% as.data.frame() %>%
    mutate(time.index = 1:length(vec1))
}

# Plot average exercises
avgExercise(exercise=3, sensor=1, exerciseStyle=2) %>% 
  plottingFormat() %>%
  ggplot() +
  geom_line(aes(x=time.index, y=acc), size=1.5) +
  geom_line(aes(x=time.index, y=gyr), size=1.5, col='red1') +
  geom_line(aes(x=time.index, y=mag), size=1.5, col='blue1') +
  facet_grid(coord~., scales='free')

# Collect average metrics for all exercises and sensors
es <- 1 # Exercise type
exercises <- list()
for (e in 1:8) {
  sensors <- list()
  for (s in 1:5) {
    sen <- avgExercise(e, s, es) 
    sensors <- c(sensors, sen)
  }
  sensors <- sensors %>% 
    as.data.frame() %>%
    select(-c(time.index.1, time.index.2, time.index.3, time.index.4)) %>%
    relocate(time.index)
  exercises <- c(exercises, list(sensors))
}








