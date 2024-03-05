### Comparing and classifying exercises with multiDSignals

## Setup
################################################################################
setwd('C:/Users/lchco/OneDrive/Documents/School/CSU/Spring_2024/STAT472/physical+therapy+exercises+dataset/')
library(multiDSignals)
library(tidyverse)
library(ggplot2)

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


# Find optimal number of signals to classify with (smoothed averages tuning for window as well)
# Get smoothed average exercises for type 1
avg_nsigs <- rep(0, 20)
pred_dfs <- list()
for (w in 1:20) {
  avg_ex_smoothed <- list()
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
    avg_ex_smoothed <- c(avg_ex_smoothed, list(avgSignalsSmoothed(exercise_list, window=w, index=TRUE)))
  }
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
                                      endTime=tTime$end[1]) %>%
        movingAverage(window=w, index=TRUE)
      nCorrect <- 0
      for (nSignals in 1:45) {
        class <- classifySignals_highVar(avg_ex_smoothed, subject_exercise, nSigs=nSignals, index=TRUE)
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
  avg_nsigs[w] <- preds$nSigs %>% mean()
  pred_dfs <- c(pred_dfs, list(preds))
}
################################################################################




## LOOCV for tuning number of signals and window for smoothed averages
################################################################################

# Find optimal number of signals to classify with (smoothed averages tuning for window as well)
# Using LOOCV
# Window tuned on 1-20
# Number of signals tuned on 1-45
results <- data.frame(window = c(0),
                      truth = c(0),
                      pred = c(0),
                      nSignals = c(0))
loocv_sessions <- c(1,2,3,4,5)
for (testSesh in loocv_sessions) {
  # Pick testing session
  train <- loocv_sessions[-testSesh]
  
  for (w in 1:20) {
    # Fit model with w window size on 4 of the 5 sessions
    avg_ex_smoothed <- list()
    for (e in 1:8) {
      exercise_list <- list()
      for (s in train) {
        tTime <- read.table(file=paste('s',s,'/e',e,'/template_times.txt', sep=''),
                            header=TRUE, sep=';')
        exercise <- getExercise(exercise=e,
                                session=s,
                                startTime=tTime$start[1],
                                endTime=tTime$end[1])
        exercise_list <- c(exercise_list, list(exercise))
      }
      avg_ex_smoothed <- c(avg_ex_smoothed, list(avgSignalsSmoothed(exercise_list, window=w, index=TRUE)))
    }
    
    # Find what number of highest variance signals are best
    truth <- rep(0, 8*45) 
    pred <- rep(0, 8*45)
    nSignal <- rep(0, 8*45)
    i <- 1
    for (e in 1:8) {
      tTime <- read.table(file=paste('s',testSesh,'/e',e,'/template_times.txt', sep=''),
                          header=TRUE, sep=';')
      subject_exercise <- getExercise(exercise=e,
                                      session=testSesh,
                                      startTime=tTime$start[1],
                                      endTime=tTime$end[1]) %>%
        movingAverage(window=w, index=TRUE)
      for (nSigs in 1:45) {
        class <- classifySignals_highVar(avg_ex_smoothed, 
                                         subject_exercise, 
                                         nSigs=nSigs, 
                                         index=TRUE)
        truth[i] <- e
        pred[i] <- class
        nSignal[i] <- nSigs
        i <- i+1
      }
    }
    preds <- data.frame(window = rep(w, 8*45),
                        truth = truth,
                        pred = pred,
                        nSignals = nSignal)
    results <- rbind(results, preds)
    print(paste("Fold ", testSesh, " ", w/20*100, "% done", sep=""))
  }
  print(paste("Fold ", testSesh, "/5 done", sep=""))
}
results <- results %>%
  filter(window != 0)


accuracy <- data.frame(Window = c(0),
                       nSignals = c(0),
                       Accuracy = c(0))
for (w in 1:20) {
  for (s in 1:45) {
    accuracy <- results %>%
      filter(window == w & nSignals == s) %>%
      summarise(Window = unique(window), 
                nSignals = unique(nSignals), 
                Accuracy = sum(truth==pred)/n()) %>%
      rbind(accuracy)
  }
}

accuracy <- accuracy %>% 
  filter(Accuracy > 0.95)
accuracy[which.min(accuracy$nSignals),]

# Results: 
# For 100% accuracy was window of 10 and nSignals of 28
# For 97.5% accuracy was window of 17 and nSignals of 12
# For 90% accuracy was window of 15 and nSignals of 4







