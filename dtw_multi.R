### Multivariate dtw


## Setup
################################################################################
setwd('C:/Users/lchco/OneDrive/Documents/School/CSU/Spring_2024/STAT472/physical+therapy+exercises+dataset/')

library(dtw)
library(tidyverse)
library(ggplot2)

## Functions
getRep <- function(template_session, template_time, type) {
  t <- template_time %>%
    filter(execution.type == type)
  template_session %>%
    filter(time.index >= t$start & time.index <= t$end)
}
plot_acc <- function(session, exerciseType) {
  s <- session
  e <- exerciseType
  
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
}
plot_gyr <- function(session, exerciseType) {
  s <- session
  e <- exerciseType
  
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
    geom_line(aes(x=time.index, y=gyr1), col='orange', size=2) +
    geom_line(aes(x=time.index, y=gyr2), col='dodgerblue', size=2) +
    geom_line(aes(x=time.index, y=gyr3), col='red2', size=2) +
    geom_line(aes(x=time.index, y=gyr4), col='green2', size=2) +
    geom_line(aes(x=time.index, y=gyr5), size=2) +
    facet_grid(coord~., scales='free') +
    labs(title=paste('Subject ',s,', Exercise type ',e,', Accelerometer', sep=''))
}
plot_mag <- function(session, exerciseType) {
  s <- session
  e <- exerciseType
  
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
    geom_line(aes(x=time.index, y=mag1), col='orange', size=2) +
    geom_line(aes(x=time.index, y=mag2), col='dodgerblue', size=2) +
    geom_line(aes(x=time.index, y=mag3), col='red2', size=2) +
    geom_line(aes(x=time.index, y=mag4), col='green2', size=2) +
    geom_line(aes(x=time.index, y=mag5), size=2) +
    facet_grid(coord~., scales='free') +
    labs(title=paste('Subject ',s,', Exercise type ',e,', Accelerometer', sep=''))
}
################################################################################


## Compare 1 sensor - acceleration, gyroscope, and magnemometer
################################################################################
e <- 1
u <- 4
time11 <- read.table(file=paste('s1/e',e,'/template_times.txt', sep=''), header=TRUE, sep=';')
s1e1u1 <- read.table(file=paste('s1/e',e,'/u',u,'/template_session.txt', sep=''), header=TRUE, sep=';')
time21 <- read.table(file=paste('s2/e',e,'/template_times.txt', sep=''), header=TRUE, sep=';')
s2e1u1 <- read.table(file=paste('s2/e',e,'/u',u,'/template_session.txt', sep=''), header=TRUE, sep=';')
time31 <- read.table(file=paste('s3/e',e,'/template_times.txt', sep=''), header=TRUE, sep=';')
s3e1u1 <- read.table(file=paste('s3/e',e,'/u',u,'/template_session.txt', sep=''), header=TRUE, sep=';')
time41 <- read.table(file=paste('s4/e',e,'/template_times.txt', sep=''), header=TRUE, sep=';')
s4e1u1 <- read.table(file=paste('s4/e',e,'/u',u,'/template_session.txt', sep=''), header=TRUE, sep=';')
time51 <- read.table(file=paste('s5/e',e,'/template_times.txt', sep=''), header=TRUE, sep=';')
s5e1u1 <- read.table(file=paste('s5/e',e,'/u',u,'/template_session.txt', sep=''), header=TRUE, sep=';')

rep1 <- getRep(s1e1u1, time11, 1) %>% select(-time.index)
rep2 <- getRep(s2e1u1, time21, 1) %>% select(-time.index)
rep3 <- getRep(s3e1u1, time31, 1) %>% select(-time.index)
rep4 <- getRep(s4e1u1, time41, 1) %>% select(-time.index)
rep5 <- getRep(s5e1u1, time51, 1) %>% select(-time.index)

for (i in 1:9) {
  dtw(rep1[,i], rep3[,i], keep=T, step=typeIa)$distance %>% print()
}




