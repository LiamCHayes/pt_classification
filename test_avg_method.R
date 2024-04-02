### Fit model on sessions 2-5, do test set on session 1

## Setup
################################################################################
setwd('C:/Users/lchco/OneDrive/Documents/School/CSU/Spring_2024/STAT472/physical+therapy+exercises+dataset/')
library(multiDSignals)
library(tidyverse)
library(ggplot2)
library(ggpmisc)

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

# Get a whole session
getSession <- function(session, exercise, test=TRUE) {
  s <- session
  e <- exercise
  f <- ifelse(test, "test", "template_session")
  sesh <- list()
  for (u in 1:5) {
    df <- read.table(file=paste('s',s,'/e',e,'/u',u,'/',f,'.txt', sep=''), header=TRUE, sep=';') %>%
      select(-time.index)
    colnames(df) <- paste(colnames(df), '_u',u,sep='')
    sesh <- c(sesh, df)
  }
  sesh %>%
    as.data.frame() %>%
    mutate(time.index = 1:nrow(df)) %>%
    relocate(time.index)
}

## Get data frames necessary for testing

# Not smoothed

# Get all average exercises for type 1
avgex <- list()
for (e in 1:8) {
  exercise_list <- list()
  for (s in 2:5) {
    tTime <- read.table(file=paste('s',s,'/e',e,'/template_times.txt', sep=''),
                        header=TRUE, sep=';')
    exercise <- getExercise(exercise=e,
                            session=s,
                            startTime=tTime$start[1],
                            endTime=tTime$end[1])
    exercise_list <- c(exercise_list, list(exercise))
  }
  avgex <- c(avgex, list(avgSignals(exercise_list, index=TRUE)))
}

# Get all average exercises for type 2 - too fast
avgex_too_fast <- list()
for (e in 1:8) {
  exercise_list <- list()
  for (s in 2:5) {
    tTime <- read.table(file=paste('s',s,'/e',e,'/template_times.txt', sep=''),
                        header=TRUE, sep=';')
    exercise <- getExercise(exercise=e,
                            session=s,
                            startTime=tTime$start[2],
                            endTime=tTime$end[2])
    exercise_list <- c(exercise_list, list(exercise))
  }
  avgex_too_fast <- c(avgex_too_fast, list(avgSignals(exercise_list, index=TRUE)))
}

# Get all average exercises for type 3 - low amplitude
avgex_low_amplitude <- list()
for (e in 1:8) {
  exercise_list <- list()
  for (s in 2:5) {
    tTime <- read.table(file=paste('s',s,'/e',e,'/template_times.txt', sep=''),
                        header=TRUE, sep=';')
    exercise <- getExercise(exercise=e,
                            session=s,
                            startTime=tTime$start[3],
                            endTime=tTime$end[3])
    exercise_list <- c(exercise_list, list(exercise))
  }
  avgex_low_amplitude <- c(avgex_low_amplitude, list(avgSignals(exercise_list, index=TRUE)))
}


# Smoothed window=10

# Get all average exercises for type 1
avgex_w10 <- list()
for (e in 1:8) {
  exercise_list <- list()
  for (s in 2:5) {
    tTime <- read.table(file=paste('s',s,'/e',e,'/template_times.txt', sep=''),
                        header=TRUE, sep=';')
    exercise <- getExercise(exercise=e,
                            session=s,
                            startTime=tTime$start[1],
                            endTime=tTime$end[1])
    exercise_list <- c(exercise_list, list(exercise))
  }
  avgex_w10 <- c(avgex_w10, list(avgSignalsSmoothed(exercise_list, window=10, index=TRUE)))
}


# Smoothed window=15

# Get all average exercises for type 1
avgex_w15 <- list()
for (e in 1:8) {
  exercise_list <- list()
  for (s in 2:5) {
    tTime <- read.table(file=paste('s',s,'/e',e,'/template_times.txt', sep=''),
                        header=TRUE, sep=';')
    exercise <- getExercise(exercise=e,
                            session=s,
                            startTime=tTime$start[1],
                            endTime=tTime$end[1])
    exercise_list <- c(exercise_list, list(exercise))
  }
  avgex_w15 <- c(avgex_w15, list(avgSignalsSmoothed(exercise_list, window=15, index=TRUE)))
}


# Smoothed window=17

# Get all average exercises for type 1
avgex_w17 <- list()
for (e in 1:8) {
  exercise_list <- list()
  for (s in 2:5) {
    tTime <- read.table(file=paste('s',s,'/e',e,'/template_times.txt', sep=''),
                        header=TRUE, sep=';')
    exercise <- getExercise(exercise=e,
                            session=s,
                            startTime=tTime$start[1],
                            endTime=tTime$end[1])
    exercise_list <- c(exercise_list, list(exercise))
  }
  avgex_w17 <- c(avgex_w17, list(avgSignalsSmoothed(exercise_list, window=17, index=TRUE)))
}


# Get test dataset
testDf <- list()
testDfTimes <- data.frame()
for (i in 1:8) {
  testDf <- c(testDf, list(getSession(1, i)))

  df <- read.csv(paste("s1/e",i,"/test_times.csv",sep=""))
  df <- df %>%
    add_column(exercise = rep(i, nrow(df)))
  testDfTimes <- rbind(testDfTimes, df)
}

# Remove unecessary variables
rm(exercise, exercise_list, tTime, df, e, s, i)

################################################################################



## Classify Exercise (all 3 types of exercise executions)
################################################################################
# LOOCV Results:
# For 100% accuracy was window of 10 and nSignals of 28
# For 97.5% accuracy was window of 17 and nSignals of 12
# For 90% accuracy was window of 15 and nSignals of 4

getTestSetPreds <- function() {
  # 100% accuracy testing
  window <- rep(10, nrow(testDfTimes))
  nSignals <- rep(28, nrow(testDfTimes))
  pred <- rep(0, nrow(testDfTimes))
  truth <- rep(0, nrow(testDfTimes))
  for (i in 1:nrow(testDfTimes)) {
    # Get exercise to compare
    ex_metadata <- testDfTimes[i,]
    ex <- testDf[[ex_metadata$exercise]] %>%
      filter(time.index >= ex_metadata$start & time.index <= ex_metadata$end)

    # Classify
    pred[i] <- classifySignals_highVar(avgex_w10, ex, nSigs=28, index=TRUE)[1]
    truth[i] <- ex_metadata$exercise

    # Print progress
    if (i %% 24 == 0) print(paste(i/240*100,"% done", sep=""))
  }
  results100 <- data.frame(window = window, nSignals = nSignals, pred = pred, truth = truth)

  # 97.5% accuracy testing
  window <- rep(17, nrow(testDfTimes))
  nSignals <- rep(12, nrow(testDfTimes))
  pred <- rep(0, nrow(testDfTimes))
  truth <- rep(0, nrow(testDfTimes))
  for (i in 1:nrow(testDfTimes)) {
    # Get exercise to compare
    ex_metadata <- testDfTimes[i,]
    ex <- testDf[[ex_metadata$exercise]] %>%
      filter(time.index >= ex_metadata$start & time.index <= ex_metadata$end)

    # Classify
    pred[i] <- classifySignals_highVar(avgex_w17, ex, nSigs=12, index=TRUE)[1]
    truth[i] <- ex_metadata$exercise

    # Print progress
    if (i %% 24 == 0) print(paste(i/240*100,"% done", sep=""))
  }
  results975 <- data.frame(window = window, nSignals = nSignals, pred = pred, truth = truth)

  # 90% accuracy testing
  window <- rep(15, nrow(testDfTimes))
  nSignals <- rep(4, nrow(testDfTimes))
  pred <- rep(0, nrow(testDfTimes))
  truth <- rep(0, nrow(testDfTimes))
  for (i in 1:nrow(testDfTimes)) {
    # Get exercise to compare
    ex_metadata <- testDfTimes[i,]
    ex <- testDf[[ex_metadata$exercise]] %>%
      filter(time.index >= ex_metadata$start & time.index <= ex_metadata$end)

    # Classify
    pred[i] <- classifySignals_highVar(avgex_w15, ex, nSigs=4, index=TRUE)[1]
    truth[i] <- ex_metadata$exercise

    # Print progress
    if (i %% 24 == 0) print(paste(i/240*100,"% done", sep=""))
  }
  results90 <- data.frame(window = window, nSignals = nSignals, pred = pred, truth = truth)

  rbind(results100, results975, results90)
}

results <- getTestSetPreds()

r100 <- results %>%
  filter(window==10)

r97.5 <- results %>%
  filter(window==17)

r90 <- results %>%
  filter(window==15)

# Accuracy
sum(r100$pred == r100$truth) / nrow(r100) # 100%
sum(r97.5$pred == r97.5$truth) / nrow(r97.5) # 84.58%
sum(r90$pred == r90$truth) / nrow(r90) # 53.33%

################################################################################



## Find When and Guess the Interval (only correctly executed exercises)
################################################################################
preds <- data.frame(pred.exercise = c(0), 
                    pred.center = c(0), 
                    pred.start = c(0), 
                    pred.end = c(0))
truth <- data.frame(truth.exercise = c(0),
                    truth.start = c(0),
                    truth.end = c(0))
for (e in 1:8) { # Takes about 3 1/3 hours
  print(paste("Exercise",e))
  
  # Get data
  sesh <- getSession(e, 1)
  times <- read.csv(paste("s1/e",e,"/test_times.csv",sep="")) %>%
    filter(rep.number <= 10)
  sesh <- sesh %>% filter(time.index < times$end[10] + 100)
  
  # get dtw distances for a sliding window for all exercises
  dtw_distances <- list()
  for (j in 1:8) {
    print(paste('Comparing exercise ',j,'/8...',sep=''))
    
    n_dtw_val <- nrow(sesh) - nrow(avgex_w10[[j]])
    dtw_val <- rep(0, nrow(sesh))
    for (i in 0:n_dtw_val) {
      w <- sesh[avgex[[j]]$time.index + i,]
      dtw_val[i+floor(nrow(avgex_w10[[j]])/2)] <- compareSignals_highVar(avgex_w10[[j]], w, nSigs=28, index=TRUE)
    }
    dtw_val[dtw_val==0] <- dtw_val[dtw_val>0] %>% mean()
    dtw_distances <- c(dtw_distances, list(dtw_val))
  }
  dtw_distances <- dtw_distances %>% as.data.frame()
  colnames(dtw_distances) <- c("e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8")
  
  # Get the exercise prediction
  ex_pred <- dtw_distances %>% sapply(mean) %>% which.min()
  ex_pred_distances <- dtw_distances[,ex_pred]
  
  # Get the local minima (predictions of when the exercise is happening)
  pred <- seq(1,nrow(dtw_distances))[find_peaks(-dtw_distances[,ex_pred], span=101)]
  pred <- pred[which(ex_pred_distances[pred] < mean(ex_pred_distances))]
  
  # Get the interval guess for each exercise 
  start <- pred - ceiling(nrow(avgex[[ex_pred]]) / 2)
  end <- pred + ceiling(nrow(avgex[[ex_pred]]) / 2)
  
  # Record results
  r <- data.frame(pred.exercise = rep(ex_pred, length(pred)),
                  pred.center = pred,
                  pred.start = start,
                  pred.end = end)
  t <- data.frame(truth.exercise = rep(e, 10),
                  truth.start = times$start,
                  truth.end = times$end)
  preds <- rbind(preds, r)
  truth <- rbind(truth, r)
}
preds <- preds %>% filter(pred.exercise != 0)
truth <- truth %>% filter(truth.exercise != 0)

################################################################################



## Find When (looking for center of the interval, all exercise executions)
################################################################################
preds <- data.frame(pred.exercise = c(0), 
                    pred.center = c(0))
truth <- data.frame(truth.exercise = c(0),
                    truth.start = c(0),
                    truth.end = c(0))
for (e in 1:8) { # Takes about 3 1/3 hours
  print(paste("Exercise",e))
  
  # Get data
  sesh <- getSession(e, 1)
  times <- read.csv(paste("s1/e",e,"/test_times.csv",sep=""))
  
  # get dtw distances for a sliding window for all exercises
  dtw_distances <- list()
  for (j in 1:8) {
    print(paste('Comparing exercise ',j,'/8...',sep=''))
    
    n_dtw_val <- nrow(sesh) - nrow(avgex_w10[[j]])
    dtw_val <- rep(0, nrow(sesh))
    for (i in 0:n_dtw_val) {
      w <- sesh[avgex[[j]]$time.index + i,]
      dtw_val[i+floor(nrow(avgex_w10[[j]])/2)] <- compareSignals_highVar(avgex_w10[[j]], w, nSigs=28, index=TRUE)
    }
    dtw_val[dtw_val==0] <- dtw_val[dtw_val>0] %>% mean()
    dtw_distances <- c(dtw_distances, list(dtw_val))
  }
  dtw_distances <- dtw_distances %>% as.data.frame()
  colnames(dtw_distances) <- c("e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8")
  
  # Get the exercise prediction
  ex_pred <- dtw_distances %>% sapply(mean) %>% which.min()
  ex_pred_distances <- dtw_distances[,ex_pred]
  
  # Get the local minima (predictions of when the exercise is happening)
  pred <- seq(1,nrow(dtw_distances))[find_peaks(-dtw_distances[,ex_pred], span=101)]
  pred <- pred[which(ex_pred_distances[pred] < mean(ex_pred_distances))]
  
  # Record results
  r <- data.frame(pred.exercise = rep(ex_pred, length(pred)),
                  pred.center = pred)
  t <- data.frame(truth.exercise = rep(e, 10),
                  truth.start = times$start,
                  truth.end = times$end)
  preds <- rbind(preds, r)
  truth <- rbind(truth, r)
}
preds <- preds %>% filter(pred.exercise != 0)
truth <- truth %>% filter(truth.exercise != 0)

################################################################################



## Classify Exercise Type
################################################################################
pred.exercise <- rep(0, nrow(testDfTimes))
pred.exercise_type <- rep(0, nrow(testDfTimes))
truth.exercise <- rep(0, nrow(testDfTimes))
truth.exercise_type <- rep(0, nrow(testDfTimes))
for (i in 1:nrow(testDfTimes)) {
  # Get exercise to compare
  ex_metadata <- testDfTimes[i,]
  ex <- testDf[[ex_metadata$exercise]] %>%
    filter(time.index >= ex_metadata$start & time.index <= ex_metadata$end)
  
  # Classify exercise using the 100% accuracy model
  pred.exercise[i] <- classifySignals_highVar(avgex_w10, ex, nSigs=28, index=TRUE)[1]
  truth.exercise[i] <- ex_metadata$exercise
  
  # Classify exercise type
  ex_type_list <- list(avgex[[pred.exercise[i]]],
                       avgex_low_amplitude[[pred.exercise[i]]],
                       avgex_too_fast[[pred.exercise[i]]])
  pred.exercise_type[i] <- classifySignals(ex_type_list, ex, index=TRUE)
  truth.exercise_type[i] <- ex_metadata$execution.type
  
  # Print progress
  if (i %% 24 == 0) print(paste(i/240*100,"% done", sep=""))
}
results <- data.frame(pred.exercise = pred.exercise,
                      pred.exercise_type = pred.exercise_type,
                      truth.exercise = truth.exercise,
                      truth.exercise_type = truth.exercise_type) %>%
  add_column(correct = pred.exercise_type == truth.exercise_type)

sum(results$pred.exercise_type == results$truth.exercise_type) / nrow(results) * 100
# Bad algorithm, 5.42% accuracy

results %>%
  ggplot() +
  geom_jitter(aes(x=truth.exercise_type, y=pred.exercise_type, col=correct),
              width=.1, 
              height=.1) +
  scale_x_continuous(breaks=c(1,2,3)) +
  scale_y_continuous(breaks=c(1,2,3)) +
  labs(title="Confusion Matrix for Exercise Type",
       x="Truth",
       y="Prediction",
       col="Correct")
ggsave("plots/exercise_type_confusion_matrix.png", height=7, width=14)

################################################################################





