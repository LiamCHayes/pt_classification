### Identify when an exercise is happening

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

getSession <- function(session, exercise) {
  s <- session
  e <- exercise
  sesh <- list()
  for (u in 1:5) {
    df <- read.table(file=paste('s',s,'/e',e,'/u',u,'/template_session.txt', sep=''), header=TRUE, sep=';') %>%
      select(-time.index)
    colnames(df) <- paste(colnames(df), '_u',u,sep='')
    sesh <- c(sesh, df)
  }
  sesh %>% 
    as.data.frame() %>%
    mutate(time.index = 1:nrow(df)) %>%
    relocate(time.index)
}

compareExercises <- function(avgExercise, exercise) {
  distances <- rep(0,45)
  for (i in 2:46) {
    distances[i-1] <- dtw(avgExercise[,i], exercise[,i], k=T, step=typeIIIc)$distance
  }
  mean(distances)
}

# Get data
avgEx <- getAllAvgExercises(1)
s1e1 <- getSession(1, 1) # session 1 exercise 1
################################################################################



## Identify when an exercise is being done
################################################################################
n_dtw_val <- nrow(s1e1) - nrow(avgEx[[1]])
dtw_val <- rep(0, n_dtw_val+1)
for (i in 0:n_dtw_val) {
  window <- s1e1[avgEx[[1]]$time.index + i,]
  dtw_val[i+1] <- compareExercises(avgEx[[1]], window)
  if (i %% 100 == 0) print(paste(round(i/n_dtw_val*100, 2),'% done', sep=''))
}


s1e1 %>%
  add_column(dtw_val = c(rep(0, 107),dtw_val,rep(0, 107))) %>%
  ggplot() +
  geom_line(aes(x=time.index, y=acc_x_u2)) +
  geom_line(aes(x=time.index, y=dtw_val))

plot(dtw_val)







