### Labelling Reps in test set


## Setup
################################################################################
setwd('C:/Users/lchco/OneDrive/Documents/School/CSU/Spring_2024/STAT472/physical+therapy+exercises+dataset/')
library(tidyverse)
library(ggplot2)

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
################################################################################



## Get sessions, plot, add start and end times to dataframe
################################################################################
# Get session
s <- 1
e <- 8
df <- getSession(s, e, T)

# Find max variance index
maxVarIdx <- sapply(df, var)[2:46] %>% 
  order(decreasing = T) + 1
maxVarIdx <- maxVarIdx[1]

# Plot max variance index
xmin <- 4000
xmax <- 6000
df %>%
  select(1, sig = maxVarIdx) %>%
  filter(time.index > xmin & time.index < xmax) %>%
  ggplot() +
  geom_line(aes(x=time.index, y=sig)) +
  geom_vline(xintercept = 5790, col='blue') +
  scale_x_continuous(breaks=seq(xmin, xmax, by=100)) 

# create test_times.txt that corresponds to test session
start <- c(100, 250, 415, 615, 790, 950, 1125, 1280, 1450, 1605, 
           2590, 2655, 2720, 2790, 2865, 2940, 3030, 3100, 3185, 3270, 
           4020, 4180, 4325, 4510, 4690, 4860, 5055, 5250, 5450, 5640)
end <- c(250, 415, 600, 770, 950, 1120, 1275, 1450, 1605, 1765, 
         2650, 2720, 2790, 2860, 2940, 3020, 3100, 3180, 3265, 3345, 
         4180, 4325, 4500, 4670, 4860, 5050, 5250, 5430, 5610, 5790)

repNum <- seq(1,30)
type <- rep(c(1,2,3), each=10)
test_times <- data.frame(rep.number = repNum,
                         execution.type = type,
                         start=start,
                         end=end)
write.csv(test_times, 
          paste('s',s,'/e',e,'/test_times.csv', sep=''), 
          row.names = F)
    
    
    
    
    
    
    
    

    
    
    
    
    
    
    
    
    
    
    
    
    
