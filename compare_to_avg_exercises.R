### Comparing exercises to average signal

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
  names(df) <- c(paste('acc_x_e',e,'_u',u,sep=''), 
                 paste('acc_y_e',e,'_u',u,sep=''), 
                 paste('acc_z_e',e,'_u',u,sep=''), 
                 paste('gyr_x_e',e,'_u',u,sep=''), 
                 paste('gyr_y_e',e,'_u',u,sep=''), 
                 paste('gyr_z_e',e,'_u',u,sep=''), 
                 paste('mag_x_e',e,'_u',u,sep=''), 
                 paste('mag_y_e',e,'_u',u,sep=''), 
                 paste('mag_z_e',e,'_u',u,sep=''))
  df %>% as.data.frame() %>%
    mutate(time.index = 1:length(vec1))
}

getAllAvgExercises <- function(exerciseType) {
  es <- exerciseType
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
  exercises
}

getExercise <- function(exercise, session, startTime, endTime) {
  s <- session
  e <- exercise
  sensors <- list()
  for (u in 1:5) {
    tSesh <- read.table(file=paste('s',s,'/e',e,'/u',u,'/template_session.txt', sep=''), header=TRUE, sep=';')
    index <- tSesh$time.index
    tSesh <- tSesh %>%
      select(-time.index)
    names(tSesh) <- paste(names(tSesh),'_e',e,'_u',u,sep='')
    sensors <- c(sensors, tSesh)
  }
  
  sensors <- sensors %>% 
    as.data.frame() %>%
    mutate(time.index=index) %>%
    relocate(time.index)
  
  sensors %>%
    filter(time.index >= startTime & time.index <= endTime)
}

# Get data
avgExercise_type1 <- getAllAvgExercises(1)
avgExercise_type2 <- getAllAvgExercises(2)
avgExercise_type3 <- getAllAvgExercises(3)
################################################################################


# Compare an exercise to an average exercise
################################################################################
tTime <- read.table(file=paste('s',1,'/e',1,'/template_times.txt', sep=''), header=TRUE, sep=';')

# Exercise 1 from session 1, type 1 (performed correctly)
e1s1t1 <- getExercise(exercise=1, session=1, startTime=tTime$start[1], endTime=tTime$end[1])

# Compare with dtw
dtw(e1s1t1$acc_y_e1_u1, avgExercise_type1[[1]]$acc_y_e1_u1, k=T, step=typeIIIc) %>%
  plot(type='two')

dtw(e1s1t1$acc_y_e1_u1, avgExercise_type1[[2]]$acc_y_e2_u1, k=T, step=typeIIIc) %>%
  plot(type='two')

# Compare all with dtw and get average difference
e1s1t1_avg <- avgExercise_type1[[1]] 

compareExercises <- function(avgExercise, exercise) {
  distances <- rep(0,45)
  for (i in 2:46) {
    distances[i-1] <- dtw(avgExercise[,i], exercise[,i], k=T, step=typeIIIc)$distance
  }
  mean(distances)
}

# Compare exercise 1 (e1s1t1) to all other exercises of type 1 from session 1
for (e in 1:8) {
  compareExercises(avgExercise_type1[[e]], e1s1t1) %>% print()
}

# General compare exercise to all other exercises from session 1
compare_exercise <- 4
for (e in 1:8) {
  d <- compareExercises(avgExercise_type1[[e]], 
                   getExercise(exercise=compare_exercise, 
                               session=1, 
                               startTime=tTime$start[1], 
                               endTime=tTime$end[1]))
  if (e == compare_exercise) print(paste("Correct match: ", d))
  else print(d)
}








