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
xmin <- 4000
xmax <- 6000
df %>%
  select(1, sig = maxVarIdx) %>%
  filter(time.index > xmin & time.index < xmax) %>%
  ggplot() +
  geom_line(aes(x=time.index, y=sig)) +
  geom_vline(xintercept = 5865, col='blue') +
  scale_x_continuous(breaks=seq(xmin, xmax, by=100)) 

# create test_times.txt that corresponds to test session
start <- c(40, 260, 470, 720, 950, 1210, 1425, 1670, 1900, 2140, 
           2530, 2625, 2710, 2800, 2895, 3000, 3120, 3230, 3340, 3460, 
           4040, 4220, 4410, 4585, 4755, 4940, 5125, 5305, 5510, 5695)
end <- c(250, 440, 685, 920, 1190, 1400, 1645, 1885, 2100, 2350, 
         2610, 2700, 2800, 2890, 2990, 3100, 3210, 3320, 3435, 3540, 
         4200, 4380, 4570, 4740, 4920, 5105, 5290, 5480, 5665, 5865)

repNum <- seq(1,30)
type <- rep(c(1,2,3), each=10)
test_times <- data.frame(rep.number = repNum,
                         execution.type = type,
                         start=start,
                         end=end)
write.csv(test_times, 
          paste('s',s,'/e',e,'/test_times.csv', sep=''), 
          row.names = F)
    
    
    
    
    
    
    
    

    
    
    
    
    
    
    
    
    
    
    
    
    
