setwd("/Users/ethanstraub/Desktop/472 Proj/PT Data")

library(tidyverse)
library(ggplot2)
library(tidyr)

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

getExercise <- function(exercise, session, startTime, endTime, sessionNum = FALSE, timeSeconds = FALSE) {
  s <- session
  e <- exercise
  sensors <- list()
  if(timeSeconds == TRUE){
    startTime = (startTime-1)/25
    endTime = (endTime-1)/25
  }
  for (u in 1:5) {
    tSesh <- read.table(file=paste('s',s,'/e',e,'/u',u,'/template_session.txt', sep=''), header=TRUE, sep=';')
    index <- tSesh$time.index
    tSesh <- tSesh %>%
      select(-time.index)
    if(sessionNum == FALSE){
      names(tSesh) <- paste(names(tSesh),'_e',e,'_u',u,sep='')
    }
    else{
      names(tSesh) <- paste(names(tSesh),'_s',s,'_e',e,'_u',u,sep='')
    }
    sensors <- c(sensors, tSesh)
  }
  
  if(timeSeconds == FALSE){
    sensors <- sensors %>% 
      as.data.frame() %>%
      mutate(time.index=index) %>%
      relocate(time.index)
  }
  else{
    sensors <- sensors %>% 
      as.data.frame() %>%
      mutate(time.index=(index-1)/25) %>%
      relocate(time.index)
  }
  
  
  if(endTime > 0){
    sensors %>%
      filter(time.index >= startTime & time.index <= endTime)
  }
  else{
    sensors %>%
      filter(time.index >= startTime)
  }
}

tTime <- read.table(file=paste('s',1,'/e',1,'/template_times.txt', sep=''), header=TRUE, sep=';')
tTimeSeconds = tTime
tTimeSeconds$start = tTime$start/25
tTimeSeconds$end = tTime$end/25
tTimeSeconds$execution.type = c("correct", "fast", "low-amplitude")

e1s1 = getExercise(exercise=1, session=1, startTime=1, endTime=-97, sessionNum = TRUE, timeSeconds = TRUE)
e1s2 <- getExercise(exercise=1, session=2, startTime=1, endTime=-1, sessionNum = TRUE, timeSeconds = TRUE)
e6s4 = getExercise(exercise=6, session=4, startTime=1, endTime=-97, sessionNum = TRUE, timeSeconds = TRUE)
e5s1t1 = getExercise(exercise=5, session=1, startTime=tTime$start[1], endTime=tTime$end[1], sessionNum = TRUE, timeSeconds = TRUE)
e5s1t2 = getExercise(exercise=5, session=1, startTime=tTime$start[2], endTime=tTime$end[2], sessionNum = TRUE, timeSeconds = TRUE)
e5s1t3 = getExercise(exercise=5, session=1, startTime=tTime$start[3], endTime=tTime$end[3], sessionNum = TRUE, timeSeconds = TRUE)

p = ggplot(e1s2, aes(x = time.index, y = acc_z_s2_e1_u5)) + geom_line() + 
  labs(title = "Session 2. Sitting Leg Raises. Sensor on the Back", x = "Time(Seconds)", y = "Acceleration in the Z direction (meters/second^2)") +
  geom_vline(xintercept = 313.5/25, color = "red") +
  geom_vline(xintercept = 528.5/25, color = "red") +
  geom_vline(xintercept = 1011/25, color = "blue") +
  geom_vline(xintercept = 1116/25, color = "blue") +
  scale_x_continuous(breaks = seq(0, 100, by = 5)) 
p
h = ggplot(e1s1, aes(x = time.index, y = acc_z_s1_e1_u2)) + geom_line() + 
  labs(title = "Session 1. Sitting Leg Raises. Ankle Sensor", x = "Time(Seconds)", y = "Acceleration in the X direction (meters/second^2)") +
  geom_vline(xintercept = 313.5/25, color = "red") +
  geom_vline(xintercept = 528.5/25, color = "red") +
  geom_vline(xintercept = 1011/25, color = "blue") +
  geom_vline(xintercept = 1116/25, color = "blue") +
  geom_vline(xintercept = 67.54, color = "darkgreen") +
  geom_vline(xintercept = 75.64, color = "darkgreen") +
  scale_x_continuous(breaks = seq(0, 100, by = 5)) 
h

e6s4_longA = gather(e6s4, key = "Direction", value = "value", acc_x_s4_e6_u2, acc_y_s4_e6_u2,acc_z_s4_e6_u2)
a = ggplot(e6s4_longA, aes(x = time.index, y = value, color = Direction)) + geom_line() + 
  labs(title = "Session 4 Bicep Curls Forearm Sensor", x = "Time(Seconds)", y = "Acceleration (m/s^2)") +
  scale_color_manual(values = c("acc_x_s4_e6_u2" = "red", "acc_y_s4_e6_u2" = "darkgreen", "acc_z_s4_e6_u2" = "blue"), labels = c("X", "Y", "Z"))
e6s4_longG = gather(e6s4, key = "Direction", value = "value", gyr_x_s4_e6_u2, gyr_y_s4_e6_u2,gyr_z_s4_e6_u2)
b = ggplot(e6s4_longG, aes(x = time.index, y = value, color = Direction)) + geom_line() + 
  labs(title = "Session 4 Bicep Curls Forearm Sensor", x = "Time(Seconds)", y = "Angular Rate (Radians/Second)") +
  scale_color_manual(values = c("gyr_x_s4_e6_u2" = "red", "gyr_y_s4_e6_u2" = "darkgreen", "gyr_z_s4_e6_u2" = "blue"), labels = c("X", "Y", "Z"))
e6s4_longM = gather(e6s4, key = "Direction", value = "value", mag_x_s4_e6_u2, mag_y_s4_e6_u2,mag_z_s4_e6_u2)
c = ggplot(e6s4_longM, aes(x = time.index, y = value, color = Direction)) + geom_line() + 
  labs(title = "Session 4 Bicep Curls Forearm Sensor", x = "Time(Seconds)", y = "Magnetic Field (Relative)") +
  scale_color_manual(values = c("mag_x_s4_e6_u2" = "red", "mag_y_s4_e6_u2" = "darkgreen", "mag_z_s4_e6_u2" = "blue"), labels = c("X", "Y", "Z"))

