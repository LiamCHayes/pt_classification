### Classifying exercises algorithms


## Setup
################################################################################
setwd('C:/Users/lchco/OneDrive/Documents/School/CSU/Spring_2024/STAT472/physical+therapy+exercises+dataset/')
library(dtw)
library(tidyverse)
library(ggplot2)
library(yardstick)

# Gets an entire session from the data 
getSession <- function(session, exercise, test=F) {
  test <- ifelse(test, 'test', 'template_session')
  s <- session
  e <- exercise
  sesh <- list()
  for (u in 1:5) {
    df <- read.table(file=paste('s',s,'/e',e,'/u',u,'/',test,'.txt', sep=''), header=TRUE, sep=';') %>%
      select(-time.index)
    colnames(df) <- paste(colnames(df), '_u',u,sep='')
    sesh <- c(sesh, df)
  }
  sesh %>% 
    as.data.frame() %>%
    mutate(time.index = 1:nrow(df)) %>%
    relocate(time.index)
}

# Extracts a single rep of a certain type from the data
getRep <- function(template_session, template_time, type) {
  t <- template_time %>%
    filter(execution.type == type)
  template_session %>%
    filter(time.index >= t$start & time.index <= t$end)
}

# Average over all subjects for a specific exercise, sensor, and exercise style
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

# Combine the averages in a dataframe
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

# Extract an exercise to compare to the averages
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

# Compare an exercise to an average (all sensors, all axes)
compareExercises <- function(avgExercise, exercise) {
  distances <- rep(0,45)
  for (i in 2:46) {
    distances[i-1] <- dtw(avgExercise[,i], exercise[,i], k=T, step=typeIIIc)$distance
  }
  mean(distances)
}

# Classify an exercise (all sensors, all axes)
classifyExercise <- function(subject_exercise, avg_exercise) {
  distances <- rep(0, 8)
  for (e in 1:8) {
    if (nrow(subject_exercise) < nrow(avg_exercise[[e]])/2) {
      subject_exercise <- lapply(subject_exercise, rep, each=2) %>%
        as.data.frame()
    }
    d <- compareExercises(avg_exercise[[e]],
                          subject_exercise)
    distances[e] <- d
  }
  which.min(distances)
} 

# Compare an exercise to an average (subset of sensors and axes)
compareExercises_subset <- function(avgExercise, exercise, indexes) {
  distances <- rep(0, length(indexes))
  idx <- 1
  for (i in indexes) {
    distances[idx] <- dtw(avgExercise[,i], exercise[,i], k=T, step=typeIIIc)$distance
    idx <- idx + 1
  }
  mean(distances)
}

# Classify an exercise using a subset of signals (highest variance)
classifyExercise_subset <- function(subject_exercise, avg_exercise, nSignals) {
  maxVarIdx <- sapply(subject_exercise, var)[2:46] %>% 
    order(decreasing = T) + 1
  maxVarIdx <- maxVarIdx[1:nSignals]
  
  distances <- rep(0, 8)
  for (e in 1:8) {
    if (nrow(subject_exercise) < nrow(avg_exercise[[e]])/2) {
      subject_exercise <- lapply(subject_exercise, rep, each=2) %>%
        as.data.frame()
    }
    d <- compareExercises_subset(avg_exercise[[e]],
                                 subject_exercise,
                                 maxVarIdx)
    distances[e] <- d
  }
  which.min(distances)
}
################################################################################




## Method 1: compare all signals
################################################################################
# Get the average signals for all exercises
avgExercise_type1 <- getAllAvgExercises(1)

# Get exercise we want to classify
e <- 4 # exercise
s <- 1 # session
tTime <- read.table(file=paste('s',s,'/e',e,'/template_times.txt', sep=''), 
                    header=TRUE, sep=';')
subject_exercise <- getExercise(exercise=e,
                                session=s,
                                startTime=tTime$start[1],
                                endTime=tTime$end[1])

# Classify exercise
classifyExercise(subject_exercise, avgExercise_type1)

# Get training set error
truth <- rep(0, 8*5)
pred <- rep(0, 8*5)
i <- 1
for (e in 1:8) {
  for (s in 1:5) {
    tTime <- read.table(file=paste('s',s,'/e',e,'/template_times.txt', sep=''), 
                        header=TRUE, sep=';')
    subject_exercise <- getExercise(exercise=e,
                                    session=s,
                                    startTime=tTime$start[1],
                                    endTime=tTime$end[1])
    truth[i] <- e
    pred[i] <- classifyExercise(subject_exercise, avgExercise_type1)
    i <- i+1
  }
}

print(paste(sum(truth == pred) / length(truth) *100,'% accuracy',sep=''))

################################################################################



## Method 2: highest variance comparison
################################################################################
# Get the average signals for all exercises
avgExercise_type1 <- getAllAvgExercises(1)

# Get exercise we want to classify
e <- 2 # exercise
s <- 1 # session
tTime <- read.table(file=paste('s',s,'/e',e,'/template_times.txt', sep=''), 
                    header=TRUE, sep=';')
subject_exercise <- getExercise(exercise=e,
                                session=s,
                                startTime=tTime$start[1],
                                endTime=tTime$end[1])

# Classify exercise for different number of signals
for (nSignals in 1:45) {
  paste(nSignals,
        ' signals: exercise ', 
        classifyExercise_subset(subject_exercise, avgExercise_type1, nSignals),
        sep='')%>%
    print()
}

# Find optimal number of signals to classify with
nSigs <- rep(0, 8*5)
pred <- rep(0, 8*5)
truth <- rep(0, 8*5)
i <- 1
for (e in 1:8) {
  for (s in 1:5) {
    tTime <- read.table(file=paste('s',s,'/e',e,'/template_times.txt', sep=''), 
                        header=TRUE, sep=';')
    subject_exercise <- getExercise(exercise=e,
                                    session=s,
                                    startTime=tTime$start[1],
                                    endTime=tTime$end[1])
    nCorrect <- 0
    for (nSignals in 1:45) {
      class <- classifyExercise_subset(subject_exercise, 
                                        avgExercise_type1, 
                                        nSignals)
      if (class == e) {nCorrect <- nCorrect + 1}
      else (nCorrect <- 0)
      if (nCorrect == 3) {
        nSigs[i] <- nSignals - 2
        break
      }
    }
    pred[i] <- classifyExercise_subset(subject_exercise, 
                            avgExercise_type1, 
                            nSigs[i])
    truth[i] <- e
    i <- i+1
  }
}

preds <- data.frame(truth = truth, pred = pred,
                    nSigs = nSigs, pred_correct = c(truth==pred))
# For computationally efficient, 4-5 signals should be good enough
# For maximum accuracy, >13 signals will work
  
  
  


