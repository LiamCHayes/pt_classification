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
    distances[i-1] <- tryCatch(
      {
        dtw(avgExercise[,i], exercise[,i], k=T, step=typeIIIc)$distance
      },
      error = function(e) {
        0
      }
    ) 
  }
  mean(distances)
}

# Get data
avgEx1 <- getAllAvgExercises(1)
avgEx2 <- getAllAvgExercises(2)
avgEx3 <- getAllAvgExercises(3)
avgEx <- list(avgEx1, avgEx2, avgEx3)
s1e1 <- getSession(1, 1) # session 1 exercise 1
################################################################################



## Identify when an exercise is being done
################################################################################
# Compare session to exercise 1 of all types
n_dtw_val <- nrow(s1e1) - nrow(avgEx1[[1]])
dtw_val <- rep(0, nrow(s1e1))
for (i in 0:n_dtw_val) {
  window <- s1e1[avgEx1[[1]]$time.index + i,]
  dtw_val[i+floor(nrow(avgEx1[[1]])/2)] <- compareExercises(avgEx1[[1]], window)
  if (i %% 100 == 0) print(paste(round(i/n_dtw_val*100, 2),'% done', sep=''))
}
dtw_val[dtw_val==0] <- dtw_val[dtw_val>0] %>% mean()

n_dtw_val_type2 <- nrow(s1e1) - nrow(avgEx2[[1]])
dtw_val_type2 <- rep(0, nrow(s1e1))
for (i in 0:n_dtw_val_type2) {
  window <- s1e1[avgEx2[[1]]$time.index + i,]
  dtw_val_type2[i+floor(nrow(avgEx2[[1]])/2)] <- compareExercises(avgEx2[[1]], window)
  if (i %% 100 == 0) print(paste(round(i/n_dtw_val_type2*100, 2),'% done', sep=''))
}
dtw_val_type2[dtw_val_type2==0] <- dtw_val_type2[dtw_val_type2>0] %>% mean()

n_dtw_val_type3 <- nrow(s1e1) - nrow(avgEx3[[1]])
dtw_val_type3 <- rep(0, nrow(s1e1))
for (i in 0:n_dtw_val_type3) {
  window <- s1e1[avgEx3[[1]]$time.index + i,]
  dtw_val_type3[i+floor(nrow(avgEx3[[1]])/2)] <- compareExercises(avgEx3[[1]], window)
  if (i %% 100 == 0) print(paste(round(i/n_dtw_val_type3*100, 2),'% done', sep=''))
}
dtw_val_type3[dtw_val_type3==0] <- dtw_val_type3[dtw_val_type3>0] %>% mean()


s1e1 %>%
  add_column(dtw_val,
             dtw_val_type2,
             dtw_val_type3) %>%
  ggplot() +
  geom_line(aes(x=time.index, y=acc_x_u2)) +
  geom_line(aes(x=time.index, y=dtw_val), col='blue') +
  geom_line(aes(x=time.index, y=dtw_val_type2), col='red') +
  geom_line(aes(x=time.index, y=dtw_val_type3), col='green2') 


# Compare to all exercises
dtw_distances <- list()
for (j in 1:8) {
  print(paste('Comparing exercise ',j,'/8...', sep=''))
  
  # First type
  n_dtw_val <- nrow(s1e1) - nrow(avgEx1[[j]])
  dtw_val <- rep(0, nrow(s1e1))
  for (i in 0:n_dtw_val) {
    window <- s1e1[avgEx1[[j]]$time.index + i,]
    dtw_val[i+floor(nrow(avgEx1[[j]])/2)] <- compareExercises(avgEx1[[j]], window)
  }
  dtw_val[dtw_val==0] <- dtw_val[dtw_val>0] %>% mean()
  print('33% done')
  
  # Second type
  n_dtw_val_type2 <- nrow(s1e1) - nrow(avgEx2[[j]])
  dtw_val_type2 <- rep(0, nrow(s1e1))
  for (i in 0:n_dtw_val_type2) {
    window <- s1e1[avgEx2[[j]]$time.index + i,]
    dtw_val_type2[i+floor(nrow(avgEx2[[j]])/2)] <- compareExercises(avgEx2[[j]], window)
  }
  dtw_val_type2[dtw_val_type2==0] <- dtw_val_type2[dtw_val_type2>0] %>% mean()
  print('66% done')
  
  # Third type
  n_dtw_val_type3 <- nrow(s1e1) - nrow(avgEx3[[j]])
  dtw_val_type3 <- rep(0, nrow(s1e1))
  for (i in 0:n_dtw_val_type3) {
    window <- s1e1[avgEx3[[j]]$time.index + i,]
    dtw_val_type3[i+floor(nrow(avgEx3[[j]])/2)] <- compareExercises(avgEx3[[j]], window)
  }
  dtw_val_type3[dtw_val_type3==0] <- dtw_val_type3[dtw_val_type3>0] %>% mean()
  print('100% done')
  
  df <- data.frame(dtw_val, dtw_val_type2, dtw_val_type3)
  colnames(df) <- c(paste('dtw_e',j,'_t1',sep=''), 
                    paste('dtw_e',j,'_t2',sep=''),
                    paste('dtw_e',j,'_t3',sep=''))
  dtw_distances <- c(dtw_distances, df)
}
dtw_distances <- dtw_distances %>% 
  as.data.frame() 
dtw_distances <- dtw_distances %>%
  add_column(time.index = 1:nrow(dtw_distances)) %>%
  relocate(time.index) %>%
  mutate(dtw_1 = select(., starts_with('dtw_e1_')) %>% rowSums(na.rm=T),
         dtw_2 = select(., starts_with('dtw_e2_')) %>% rowSums(na.rm=T),
         dtw_3 = select(., starts_with('dtw_e3_')) %>% rowSums(na.rm=T),
         dtw_4 = select(., starts_with('dtw_e4_')) %>% rowSums(na.rm=T),
         dtw_5 = select(., starts_with('dtw_e5_')) %>% rowSums(na.rm=T),
         dtw_6 = select(., starts_with('dtw_e6_')) %>% rowSums(na.rm=T),
         dtw_7 = select(., starts_with('dtw_e7_')) %>% rowSums(na.rm=T),
         dtw_8 = select(., starts_with('dtw_e8_')) %>% rowSums(na.rm=T),)
  

# Plot all dtw_distances
dtw_distances %>% 
  ggplot() +
  geom_line(aes(x=time.index, y=dtw_e1_t1)) +
  geom_line(aes(x=time.index, y=dtw_e2_t1), col='red') +
  geom_line(aes(x=time.index, y=dtw_e3_t1), col='red') +
  geom_line(aes(x=time.index, y=dtw_e4_t1), col='red') +
  geom_line(aes(x=time.index, y=dtw_e5_t1), col='red') +
  geom_line(aes(x=time.index, y=dtw_e6_t1), col='red') +
  geom_line(aes(x=time.index, y=dtw_e7_t1), col='red') +
  geom_line(aes(x=time.index, y=dtw_e8_t1), col='red') +
  geom_line(aes(x=time.index, y=dtw_e1_t2)) +
  geom_line(aes(x=time.index, y=dtw_e2_t2), col='red') +
  geom_line(aes(x=time.index, y=dtw_e3_t2), col='red') +
  geom_line(aes(x=time.index, y=dtw_e4_t2), col='red') +
  geom_line(aes(x=time.index, y=dtw_e5_t2), col='red') +
  geom_line(aes(x=time.index, y=dtw_e6_t2), col='red') +
  geom_line(aes(x=time.index, y=dtw_e7_t2), col='red') +
  geom_line(aes(x=time.index, y=dtw_e8_t2), col='red') +
  geom_line(aes(x=time.index, y=dtw_e1_t3)) +
  geom_line(aes(x=time.index, y=dtw_e2_t3), col='red') +
  geom_line(aes(x=time.index, y=dtw_e3_t3), col='red') +
  geom_line(aes(x=time.index, y=dtw_e4_t3), col='red') +
  geom_line(aes(x=time.index, y=dtw_e5_t3), col='red') +
  geom_line(aes(x=time.index, y=dtw_e6_t3), col='red') +
  geom_line(aes(x=time.index, y=dtw_e7_t3), col='red') +
  geom_line(aes(x=time.index, y=dtw_e8_t3), col='red')  +
  geom_rect(data=data.frame(start=c(90, 330, 550, 900, 1025, 1160, 1470, 1700, 1920), 
                            end=c(260, 510, 710, 960, 1100, 1230, 1650, 1880, 2100)),
            aes(xmin=start, xmax=end, ymin=0, ymax=600),
            col='transparent', fill='green', alpha=0.3) +
  labs(title = 'All DTW Distances (not summed)')


# plot sums of dtw values by exercise
doing_exercise <- ifelse(dtw_distances$dtw_1 < mean(dtw_distances$dtw_1), 1, 0)
dtw_distances %>%
  select(-starts_with('dtw_e')) %>%
  pivot_longer(cols = starts_with('dtw_'),
               names_to = 'exercise',
               names_prefix = 'dtw_', 
               values_to = 'dtw') %>%
  ggplot() +
  geom_line(aes(x=time.index, y=dtw, col=exercise)) +
  geom_rect(data=data.frame(start=c(90, 330, 550, 900, 1025, 1160, 1470, 1700, 1920), 
                            end=c(260, 510, 710, 960, 1100, 1230, 1650, 1880, 2100)),
            aes(xmin=start, xmax=end, ymin=25, ymax=1050),
            col='transparent', fill='green', alpha=0.2) +
  geom_line(data=s1e1, aes(x=time.index, y=acc_x_u2), col=doing_exercise+1) +
  labs(title = 'All DTW Distances (summed)')


## Create Functions
################################################################################
get_dtw_distances <- function(session, avgEx1, avgEx2, avgEx3) {
  dtw_distances <- list()
  for (j in 1:8) {
    print(paste('Comparing exercise ',j,'/8...', sep=''))
    
    # First type
    n_dtw_val <- nrow(session) - nrow(avgEx1[[j]])
    dtw_val <- rep(0, nrow(session))
    for (i in 0:n_dtw_val) {
      window <- session[avgEx1[[j]]$time.index + i,]
      dtw_val[i+floor(nrow(avgEx1[[j]])/2)] <- compareExercises(avgEx1[[j]], window)
    }
    dtw_val[dtw_val==0] <- dtw_val[dtw_val>0] %>% mean()
    print('33% done')
    
    # Second type
    n_dtw_val_type2 <- nrow(session) - nrow(avgEx2[[j]])
    dtw_val_type2 <- rep(0, nrow(session))
    for (i in 0:n_dtw_val_type2) {
      window <- session[avgEx2[[j]]$time.index + i,]
      dtw_val_type2[i+floor(nrow(avgEx2[[j]])/2)] <- compareExercises(avgEx2[[j]], window)
    }
    dtw_val_type2[dtw_val_type2==0] <- dtw_val_type2[dtw_val_type2>0] %>% mean()
    print('66% done')
    
    # Third type
    n_dtw_val_type3 <- nrow(session) - nrow(avgEx3[[j]])
    dtw_val_type3 <- rep(0, nrow(session))
    for (i in 0:n_dtw_val_type3) {
      window <- session[avgEx3[[j]]$time.index + i,]
      dtw_val_type3[i+floor(nrow(avgEx3[[j]])/2)] <- compareExercises(avgEx3[[j]], window)
    }
    dtw_val_type3[dtw_val_type3==0] <- dtw_val_type3[dtw_val_type3>0] %>% mean()
    print('100% done')
    
    df <- data.frame(dtw_val, dtw_val_type2, dtw_val_type3)
    colnames(df) <- c(paste('dtw_e',j,'_t1',sep=''), 
                      paste('dtw_e',j,'_t2',sep=''),
                      paste('dtw_e',j,'_t3',sep=''))
    dtw_distances <- c(dtw_distances, df)
  }
  dtw_distances <- dtw_distances %>% 
    as.data.frame() 
  dtw_distances <- dtw_distances %>%
    add_column(time.index = 1:nrow(dtw_distances)) %>%
    relocate(time.index) %>%
    mutate(dtw_1 = select(., starts_with('dtw_e1_')) %>% rowSums(na.rm=T),
           dtw_2 = select(., starts_with('dtw_e2_')) %>% rowSums(na.rm=T),
           dtw_3 = select(., starts_with('dtw_e3_')) %>% rowSums(na.rm=T),
           dtw_4 = select(., starts_with('dtw_e4_')) %>% rowSums(na.rm=T),
           dtw_5 = select(., starts_with('dtw_e5_')) %>% rowSums(na.rm=T),
           dtw_6 = select(., starts_with('dtw_e6_')) %>% rowSums(na.rm=T),
           dtw_7 = select(., starts_with('dtw_e7_')) %>% rowSums(na.rm=T),
           dtw_8 = select(., starts_with('dtw_e8_')) %>% rowSums(na.rm=T),)
  dtw_distances
}

get_exercise_pred <- function(dtw_distances) {
  dtw_distances %>%
    select(-starts_with('dtw_e')) %>%
    sapply(mean) %>%
    sort(decreasing = F) -> selection
  selection <- selection[1]
  
  dtw_distances %>%
    select(time.index, 
           names(selection), 
           starts_with(paste(strtrim(names(selection), 4),
                             'e',
                             substr(names(selection),start=5,stop=5), 
                             sep=''))) %>%
    mutate(less_avg = ifelse(.[[2]] < selection, 1, 0)) %>%
    select(less_avg)
}

# test functions
dtw_distances <- get_dtw_distances(s1e1, avgEx1, avgEx2, avgEx3)
get_exercise_pred(dtw_distances) %>% plot()

