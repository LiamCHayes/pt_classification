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
e <- 6
df <- getSession(s, e, T)

# Find max variance index
maxVarIdx <- sapply(df, var)[2:46] %>% 
  order(decreasing = T) + 1
maxVarIdx <- maxVarIdx[1]

# Plot max variance index
xmin <- 0
xmax <- 6000
df %>%
  select(1, sig = maxVarIdx) %>%
  filter(time.index > xmin & time.index < xmax) %>%
  ggplot() +
  geom_line(aes(x=time.index, y=sig)) +
  geom_vline(xintercept = 5840, col='blue') +
  scale_x_continuous(breaks=seq(xmin, xmax, by=100)) 

# create test_times.txt that corresponds to test session
start <- c()
end <- c()

repNum <- seq(1,30)
type <- rep(c(1,2,3), each=10)
test_times <- data.frame(rep.number = repNum,
                         execution.type = type,
                         start=start,
                         end=end)
write.csv(test_times, 
          paste('s',s,'/e',e,'/test_times.csv', sep=''), 
          row.names = F)
    
    
    
    
    
    
    
    

    
    
    
    
    
    
    
    
    
    
    
    
    
