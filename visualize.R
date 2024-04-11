### Script to create visualizations

## Setup
################################################################################
setwd('C:/Users/lchco/OneDrive/Documents/School/CSU/Spring_2024/STAT472/physical+therapy+exercises+dataset/')
library(multiDSignals)
library(tidyverse)
library(ggplot2)
library(dtw)


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


################################################################################
## Visualize averaging method
e <- 1
signals <- list()
for (s in seq(1, 5)) {
  tTime <- read.table(file=paste('s',s,'/e',e,'/template_times.txt', sep=''),
                      header=TRUE, sep=';')

  df <- getExercise(e, s, tTime$start[1], tTime$end[1])

  maxVarIdx <- sapply(df, var)[2:46] %>%
    order(decreasing = T) + 1
  maxVarIdx <- maxVarIdx[1]

  signals <- c(signals, list(df[,maxVarIdx]))
}

longestSig <- sapply(signals, length) %>% max()

aligned <- list(signals[[1]])
for (i in 2:5) {
  dtwObj <- dtw(signals[[1]], signals[[i]], step.pattern = typeIIIc)
  s <- signals[[i]][dtwObj$index2]
  aligned <- c(aligned, list(s))
}

unaligned <- list()
for (i in 1:5) {
  unaligned <- c(unaligned, list(c(signals[[i]], rep(100, longestSig - length(signals[[i]])))))
}

unaligned <- as.data.frame(unaligned)
aligned <- as.data.frame(aligned)

colnames(unaligned) <- c("s1", "s2", "s3", "s4", "s5")
colnames(aligned) <- c("s1", "s2", "s3", "s4", "s5")

unaligned %>%
  add_column(index = 1:longestSig) %>%
  pivot_longer(cols = starts_with("s"),
               names_to = "Session",
               values_to = "value") %>%
  ggplot() +
  geom_line(aes(x=index, y=value, col=Session)) +
  ylim(-17, 15) +
  labs(title="Unaligned Signals - Exercise 1 Sensor 2 Acceleration in the x Direction",
       x="Time Index", y="Acceleration (m/s)") +
  theme(axis.title = element_text(size=13),
        title = element_text(size=15))
ggsave("plots/unaligned.png", width = 14, height = 7)

aligned %>%
  add_column(index = 1:longestSig) %>%
  pivot_longer(cols = starts_with("s"),
               names_to = "Session",
               values_to = "value") %>%
  ggplot() +
  geom_line(aes(x=index, y=value, col=Session)) +
  ylim(-17, 15) +
  labs(title="Aligned Signals - Exercise 1 Sensor 2 Acceleration in the x Direction",
       x="Time Index", y="Acceleration (m/s)") +
  theme(axis.title = element_text(size=13),
        title = element_text(size=15))
ggsave("plots/aligned.png", width = 14, height = 7)

# Get average signals
exercise_list <- list()
for (s in 1:5) {
  tTime <- read.table(file=paste('s',s,'/e1/template_times.txt', sep=''),
                      header=TRUE, sep=';')
  exercise <- getExercise(exercise=e,
                          session=s,
                          startTime=tTime$start[1],
                          endTime=tTime$end[1])
  exercise_list <- c(exercise_list, list(exercise))
}
avgSig <- avgSignals(exercise_list, index=TRUE)

aligned %>%
  add_column(index = 1:longestSig) %>%
  add_column(avg = avgSig[[maxVarIdx]]) %>%
  pivot_longer(cols = starts_with("s"),
               names_to = "Session",
               values_to = "value") %>%
  ggplot() +
  geom_line(aes(x=index, y=value, col=Session), alpha=0.4) +
  geom_line(aes(x=index, y=avg), size=1.5) +
  ylim(-17, 15) +
  labs(title="Average Signal - Exercise 1 Sensor 2 Acceleration in the x Direction",
       x="Time Index", y="Acceleration (m/s)") +
  theme(axis.title = element_text(size=13),
        title = element_text(size=15))
ggsave("plots/average.png", width = 14, height = 7)

################################################################################
# Visualize finding when an exercise is happening
e <- 1
sesh <- getSession(e, 1) %>%
  filter(time.index <= 2100)
times <- read.csv(paste("s1/e",e,"/test_times.csv",sep="")) %>%
  filter(end <= 2100)

maxVarIdx <- sapply(sesh, var)[2:46] %>%
  order(decreasing = T) + 1
maxVarIdx <- maxVarIdx[1]

# Plot highlighted test set
sesh %>%
  ggplot() +
  geom_line(aes(x=time.index, y=sesh[,maxVarIdx])) +
  annotate('rect', fill='green', alpha=0.3, xmin=times$start, xmax=times$end, ymin=-Inf, ymax=Inf) +
  labs(title="Test Set - Exercise 1 Sensor 2 Acceleration in the x Direction",
       x="Time Index", y="Acceleration (m/s)") +
  theme(axis.title = element_text(size=13),
        title = element_text(size=15))
ggsave("plots/s1e1_test_set_highlighted.png", width = 14, height = 7)

# Get average signals for all 8 exercises (sessions 2:5 because session 1 is the test set)
avg_sigs <- list()
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
  avg_sigs <- c(avg_sigs, list(avgSignals(exercise_list, index=TRUE)))
}

# get dtw distances for a sliding window for all exercises
dtw_distances <- list()
for (j in 1:8) {
  print(paste('Comparing exercise ',j,'/8...',sep=''))

  n_dtw_val <- nrow(sesh) - nrow(avg_sigs[[j]])
  dtw_val <- rep(0, nrow(sesh))
  for (i in 0:n_dtw_val) {
    window <- sesh[avg_sigs[[j]]$time.index + i,]
    dtw_val[i+floor(nrow(avg_sigs[[j]])/2)] <- compareSignals(avg_sigs[[j]], window, index=TRUE)
  }
  dtw_val[dtw_val==0] <- dtw_val[dtw_val>0] %>% mean()
  dtw_distances <- c(dtw_distances, list(dtw_val))
}
dtw_distances <- dtw_distances %>% as.data.frame()
colnames(dtw_distances) <- c("e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8")

# Plot  dtw_distances
dtw_distances %>%
  add_column(index = seq(1, nrow(dtw_distances))) %>%
  pivot_longer(cols = starts_with("e")) %>%
  ggplot() +
  geom_line(aes(x=index, y=value, col=name)) +
  annotate('rect', fill='green', alpha=0.3, xmin=times$start, xmax=times$end, ymin=-Inf, ymax=Inf) +
  labs(title="DTW Distances - Exercise 1",
       x="Time Index", y="DTW Distance",
       col="Exercise") +
  theme(axis.title = element_text(size=13),
        title = element_text(size=15))
ggsave("plots/s1e1_dtw_distances.png", width = 14, height = 7)

# Plot lowest average dtw distance against session signal
sesh %>%
  add_column(dtw_dist = dtw_distances[,which.min(sapply(dtw_distances, mean))]-70) %>%
  ggplot() +
  geom_line(aes(x=time.index, y=sesh[,maxVarIdx])) +
  geom_line(aes(x=time.index, y=dtw_dist, col="DTW distance (adjusted)"), size=1.5) +
  annotate('rect', fill='green', alpha=0.3, xmin=times$start, xmax=times$end, ymin=-Inf, ymax=Inf) +
  labs(title="Test Set With DTW Distance - Exercise 1 Sensor 2 Acceleration in the x Direction",
       x="Time Index", y="Acceleration (m/s)",
       col="") +
  theme(axis.title = element_text(size=13),
        title = element_text(size=15))
ggsave("plots/s1e1_test_set_dtw_distances.png", width = 14, height = 7)








################################################################################
## See a whole session

df <- getSession(1,1,test=FALSE) %>%
  pivot_longer(!time.index, 
               names_to = c("type", "direction", "sensor"),
               names_pattern = "(.*)_(.*)_u(.*)")
df$type[which(df$type == "acc")] <- "Accelerometer"
df$type[which(df$type == "gyr")] <- "Gyroscope"
df$type[which(df$type == "mag")] <- "Magnemometer"

a <-  df %>%
  filter(type == "Accelerometer") %>%
  ggplot() +
  geom_line(aes(x=time.index, y=value, col=sensor)) +
  facet_grid(direction ~ .) +
  labs(title = "Accelerometer",
       y = "Value",
       x = "Time Index",
       col = "Sensor") +
  theme(legend.position = "none")

g <- df %>%
  filter(type == "Gyroscope") %>%
  ggplot() +
  geom_line(aes(x=time.index, y=value, col=sensor)) +
  facet_grid(direction ~ .) +
  labs(title = "Gyroscope",
       y = "",
       x = "Time Index",
       col = "Sensor") +
  theme(legend.position = "none")

m <- df %>%
  filter(type == "Magnemometer") %>%
  ggplot() +
  geom_line(aes(x=time.index, y=value, col=sensor)) +
  facet_grid(direction ~ .) +
  labs(title = "Magnemometer",
       y = "",
       x = "Time Index",
       col = "Sensor")


a + g + m + plot_layout(widths = c(10,10,10))
ggsave("plots/e1_s1_all_signals.png", width=14, height=7)

