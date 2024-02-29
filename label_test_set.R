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
e <- 1
df <- getSession(s, e, T)

# Find max variance index
maxVarIdx <- sapply(df, var)[2:46] %>% 
  order(decreasing = T) + 1
maxVarIdx <- maxVarIdx[1]

# Plot max variance index
df %>%
  select(1, sig = maxVarIdx) %>%
  ggplot() +
  geom_line(aes(x=time.index, y=sig)) +
  xlim(4000, 5900) +
  geom_vline(xintercept = 5880, col='blue')

# create test_times.txt that corresponds to test session
start <- c(51, 240, 428, 600, 775, 993, 1220, 1415, 1605, 1805, 
           2600, 2670, 2740, 2800, 2890, 2955, 3040, 3130, 3215, 3300, 
           4060, 4265, 4460, 4660, 4835, 5035, 5190, 5365, 5530, 5690)
end <- c(215, 403, 567, 750, 945, 1165, 1377, 1571, 1770, 1990, 
         2650, 2720, 2785, 2850, 2930, 3010, 3100, 3185, 3270, 3350,
         4245, 4425, 4620, 4815, 5000, 5175, 5350, 5510, 5665, 5880)

repNum <- seq(1,30)
type <- rep(c(1,2,3), each=10)
test_times <- data.frame(rep.number = repNum,
                         execution.type = type,
                         start=start,
                         end=end)
write.csv(test_times, 
          paste('s',s,'/e',e,'/test_times.csv', sep=''), 
          row.names = F)
    
    
    
    
    
    
    
    

    
    
    
    
    
    
    
    
    
    
    
    
    
