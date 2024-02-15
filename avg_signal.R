### Compute the average signal over all sessions for each exercise and sensor 

## Setup
################################################################################
setwd('C:/Users/lchco/OneDrive/Documents/School/CSU/Spring_2024/STAT472/physical+therapy+exercises+dataset/')

library(ggplot2)

# Functions
getRep <- function(template_session, template_time, type) {
  t <- template_time %>%
    filter(execution.type == type)
  template_session %>%
    filter(time.index >= t$start & time.index <= t$end)
}
################################################################################


## Get data in the right format
################################################################################
s <- 1 # Session number, 1:5
e <- 1 # Exercise type, 1:8
u <- 1 # Sensor unit, 1:5

# For each session, get all 5 sensors for a specific exercise
session_list <- list()
for (i in 1:5) {
  df <- read.table(file=paste('s',i,'/e',e,'/u',u,'/template_session.txt', sep=''), header=TRUE, sep=';') %>%
    getRep(read.table(file=paste('s',i,'/e',e,'/template_times.txt', sep=''), header=TRUE, sep=';'), 1) %>%
    select(-time.index)
  colnames(df) <- paste(colnames(df), i, sep='_')
  session_list <- c(session_list, 
                    df)
}

# Stretch two acceleration x vectors to be the same length with dtw
alignment <- dtw(session_list$acc_x_1, session_list$acc_x_2, k=T, step=typeIIIc)
alignment %>% plot(type='two')

new_acc_x_2 <- rep(0, length(alignment$index2))
new_acc_x_2[alignment$index1] <- session_list$acc_x_2[alignment$index2]

data.frame(i=seq(1,length(new_acc_x_2)), x1=session_list$acc_x_1, x2=new_acc_x_2) %>%
  ggplot() +
  geom_line(aes(x=i, y=x1)) +
  geom_line(aes(x=i, y=x2), col='red')
