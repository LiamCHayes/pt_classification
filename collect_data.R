

## Setup
setwd('C:/Users/lchco/OneDrive/Documents/School/CSU/Spring_2024/STAT472/physical+therapy+exercises+dataset/')

library(tidyverse)
library(ggplot2)

## Functions
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

## Method 1
s <- 5 # Session number, 1:5
e <- 1 # Exercise type, 1:8
u <- 2 # Sensor unit, 1:5

tSesh <- read.table(file=paste('s',s,'/e',e,'/u',u,'/template_session.txt', sep=''), header=TRUE, sep=';')
tTest <- read.table(file=paste('s',s,'/e',e,'/u',u,'/test.txt', sep=''), header=TRUE, sep=';')
tTime <- read.table(file=paste('s',s,'/e',e,'/template_times.txt', sep=''), header=TRUE, sep=';')

plottingFormat(tSesh) %>%
  ggplot() +
  geom_point(aes(x=time.index, y=acc)) +
  facet_grid(coord ~., scales = "free") +
  geom_vline(xintercept = tTime$start, col='green', size=2) +
  geom_vline(xintercept = tTime$end, col='red', size=2) +
  labs(title=paste('Session ',s,', Exercise type ',e,', Sensor ', u, sep=''))

## Get all sensors for a session and exercise
s <- 1 # Session number, 1:5
e <- 1 # Exercise type, 1:8

tSesh1 <- read.table(file=paste('s',s,'/e',e,'/u',1,'/template_session.txt', sep=''), header=TRUE, sep=';') %>%
  plottingFormat() %>%
  rename(acc1=acc, gyr1=gyr, mag1=mag)
tSesh2 <- read.table(file=paste('s',s,'/e',e,'/u',2,'/template_session.txt', sep=''), header=TRUE, sep=';') %>%
  plottingFormat() %>%
  rename(acc2=acc, gyr2=gyr, mag2=mag)
tSesh3 <- read.table(file=paste('s',s,'/e',e,'/u',3,'/template_session.txt', sep=''), header=TRUE, sep=';') %>%
  plottingFormat() %>%
  rename(acc3=acc, gyr3=gyr, mag3=mag)
tSesh4 <- read.table(file=paste('s',s,'/e',e,'/u',4,'/template_session.txt', sep=''), header=TRUE, sep=';') %>%
  plottingFormat() %>%
  rename(acc4=acc, gyr4=gyr, mag4=mag)
tSesh5 <- read.table(file=paste('s',s,'/e',e,'/u',5,'/template_session.txt', sep=''), header=TRUE, sep=';') %>%
  plottingFormat() %>%
  rename(acc5=acc, gyr5=gyr, mag5=mag)
tSesh_all_sensors <- list(tSesh1, tSesh2, tSesh3, tSesh4, tSesh5) %>%
  reduce(full_join, by=c('time.index', 'coord'))

tTime <- read.table(file=paste('s',s,'/e',e,'/template_times.txt', sep=''), header=TRUE, sep=';')

tSesh_all_sensors %>%
  ggplot() +
  geom_line(aes(x=time.index, y=acc1), col='orange', size=2) +
  geom_line(aes(x=time.index, y=acc2), col='dodgerblue', size=2) +
  geom_line(aes(x=time.index, y=acc3), col='red2', size=2) +
  geom_line(aes(x=time.index, y=acc4), col='green2', size=2) +
  geom_line(aes(x=time.index, y=acc5), size=2) +
  facet_grid(coord~., scales='free') +
  labs(title=paste('Subject ',s,', Exercise type ',e,', Accelerometer', sep=''))

tSesh_all_sensors %>%
  ggplot() +
  geom_line(aes(x=time.index, y=gyr1), col='orange', size=2) +
  geom_line(aes(x=time.index, y=gyr2), col='dodgerblue', size=2) +
  geom_line(aes(x=time.index, y=gyr3), col='red2', size=2) +
  geom_line(aes(x=time.index, y=gyr4), col='green2', size=2) +
  geom_line(aes(x=time.index, y=gyr5), size=2) +
  facet_grid(coord~., scales='free') +
  labs(title=paste('Subject ',s,', Exercise type ',e,', Gyroscope', sep=''))
  
tSesh_all_sensors %>%
  ggplot() +
  geom_line(aes(x=time.index, y=mag1), col='orange', size=2) +
  geom_line(aes(x=time.index, y=mag2), col='dodgerblue', size=2) +
  geom_line(aes(x=time.index, y=mag3), col='red2', size=2) +
  geom_line(aes(x=time.index, y=mag4), col='green2', size=2) +
  geom_line(aes(x=time.index, y=mag5), size=2) +
  facet_grid(coord~., scales='free') +
  labs(title=paste('Subject ',s,', Exercise type ',e,', Magnetometer', sep=''))

## Get all data for an exercise type
e <- 1 # exercise type, 1:8
u <- 1 # sensor unit, 1:5
metric <- 'acc' # Choose from acc, gyr, mag

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

# Get a specific coordinate signal and exercise type from tSesh_all_subjects
c <- 'x' # coordinate we want to look at, choose from x, y, z
exerciseType <- 1 # Way of performing the exercise. Regular, fast, low amplitude. Choose from 1:3
tSesh_all_subjects %>%
  filter(coord == c, 
         exercise1==exerciseType |
           exercise2==exerciseType |
           exercise3==exerciseType |
           exercise4==exerciseType |
           exercise5==exerciseType) -> filtered
filtered %>%
  mutate(acc1 = ifelse(exercise1==exerciseType, acc1, NA),
         acc2 = ifelse(exercise2==exerciseType, acc2, NA),
         acc3 = ifelse(exercise3==exerciseType, acc3, NA),
         acc4 = ifelse(exercise4==exerciseType, acc4, NA),
         acc5 = ifelse(exercise5==exerciseType, acc5, NA)) %>%
  mutate(acc1 = c(na.omit(acc1), rep(NA, sum(is.na(acc1)))),
         acc2 = c(na.omit(acc2), rep(NA, sum(is.na(acc2)))),
         acc3 = c(na.omit(acc3), rep(NA, sum(is.na(acc3)))),
         acc4 = c(na.omit(acc4), rep(NA, sum(is.na(acc4)))),
         acc5 = c(na.omit(acc5), rep(NA, sum(is.na(acc5))))) %>%
  select(-c(exercise1, exercise2, exercise3, exercise4, exercise5)) %>% 
  filter(!is.na(acc1) | !is.na(acc2) | !is.na(acc3) | !is.na(acc4) | !is.na(acc5)) -> filtered

filtered %>%
  ggplot() +
  geom_line(aes(x=time.index, y=acc1), col='orange', size=1.5) +
  geom_line(aes(x=time.index, y=acc2), col='dodgerblue', size=1.5) +
  geom_line(aes(x=time.index, y=acc3), col='red2', size=1.5) +
  geom_line(aes(x=time.index, y=acc4), col='green2', size=1.5) +
  geom_line(aes(x=time.index, y=acc5), size=1.5) +
  labs(title=paste('Exercise type ',e,', Exercise style ',exerciseType,', Unit ',u,', ',c,' coordinate, Accelerometer', sep=''))


