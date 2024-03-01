### Comparing and classifying exercises with multiDSignals

## Setup
################################################################################
setwd('C:/Users/lchco/OneDrive/Documents/School/CSU/Spring_2024/STAT472/physical+therapy+exercises+dataset/')
library(multiDSignals)
library(tidyverse)

## Functions

# Extract a signle rep from the data
getExercise <- function(exercise, session, startTime, endTime, test=F) {
  t <- ifelse(test == T, 'test', 'template_session')
  s <- session
  e <- exercise
  sensors <- list()
  for (u in 1:5) {
    tSesh <- read.table(file=paste('s',s,'/e',e,'/u',u,'/',t,'.txt', sep=''), header=TRUE, sep=';')
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

# Get all average exercises for type 1
average_exercises <- list()
for (e in 1:8) {
  exercise_list <- list()
  for (s in 1:5) {
    tTime <- read.table(file=paste('s',s,'/e',e,'/template_times.txt', sep=''), 
                        header=TRUE, sep=';')
    exercise <- getExercise(exercise=e,
                            session=s,
                            startTime=tTime$start[1],
                            endTime=tTime$end[1])
    exercise_list <- c(exercise_list, list(exercise))
  }
  average_exercises <- c(average_exercises, list(avgSignals(exercise_list, index=TRUE)))
}
################################################################################




## Method 1: compare all signals
################################################################################
# Get test set exercise we want to classify
e <- 1 # exercise
s <- 1 # session
tTime <- read.csv(file=paste('s',s,'/e',e,'/test_times.csv', sep=''), 
                  header=TRUE)
subject_exercise <- getExercise(exercise=e,
                                session=s,
                                startTime=tTime$start[1],
                                endTime=tTime$end[1], 
                                test=T)

# Classify exercise on test set
for (f in 1:30) {
  subject_exercise <- getExercise(exercise=e,
                                  session=s,
                                  startTime=tTime$start[f],
                                  endTime=tTime$end[f], 
                                  test=T)
  paste('Exercise ',e,
        ' type ',tTime$execution.type[f],
        ' classification: ', classifySignals(average_exercises, subject_exercise, index=TRUE)) %>% 
    print()
}

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
    pred[i] <- classifySignals(average_exercises, subject_exercise, index=TRUE)
    i <- i+1
  }
}

print(paste(sum(truth == pred) / length(truth) *100,'% accuracy',sep=''))
################################################################################




## Method 2: highest variance comparison
################################################################################
# Get exercise we want to classify
e <- 1 # exercise
s <- 3 # session
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
        classifySignals_highVar(average_exercises, subject_exercise, nSigs=nSignals, index=TRUE),
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
      class <- classifySignals_highVar(average_exercises, subject_exercise, nSigs=nSignals, index=TRUE)
      if (class == e) {nCorrect <- nCorrect + 1}
      else (nCorrect <- 0)
      if (nCorrect == 3) {
        nSigs[i] <- nSignals - 2
        break
      }
    }
    pred[i] <- classifySignals_highVar(average_exercises, subject_exercise, nSigs=nSignals, index=TRUE)
    truth[i] <- e
    i <- i+1
  }
}

preds <- data.frame(truth = truth, pred = pred,
                    nSigs = nSigs, pred_correct = c(truth==pred))
# For computationally efficient, 4-5 signals should be good enough
# For maximum accuracy, >18 signals will work






