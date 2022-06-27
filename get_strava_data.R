
library(httr) 


# get stream data for two activities in vector 'activities'
# note cookie is not valid and needs to update for current session

activities = c(7352923995,7349180819)

df_activity<-data.frame()

for (i in (1:length(activities))){
  activity = activities[i]
  url = sprintf("https://www.strava.com/activities/ %s /streams?stream_types%%5B%%5D=heartrate&stream_types%%5B%%5D=distance&stream_types%%5B%%5D=grade_smooth&stream_types%%5B%%5D=time&stream_types%%5B%%5D=grade_adjusted_distance", activity)
  c <- set_cookies('_strava4_session' = 'b847jbofrua0k6ke4q43uc1b48ncr7no')
  jsonResponse<-GET(url,c)
  jsonResponsestream <- content(jsonResponse) 
  df<-as.data.frame(do.call(cbind, jsonResponsestream))
  df<-cbind(activity,df)
  colnames(df)[1] <- 'activity'
  df_activity<-rbind(df_activity,df)
}

# Get a summary list of last 200 actvities for an athlete and save as dataframe

library(httr) 
headers = c('Authorization' = 'Bearer 929f8ad9bfd6e4941671ab20625aff18e0f860c6')
res <- VERB("GET", url = "https://www.strava.com/api/v3/athlete/activities?per_page=200", add_headers(headers))
activities <- content(res)
df_athleteactivities<-as.data.frame(do.call(rbind, activities))


#function to calculate trimp
Trimp <- function(duration, hr, rest, max,sex){
  if (sex == "M"){v = 1.92} else {v = 1.67}
  hrr = (hr - rest) / (max - rest)
  tr = duration*hrr*0.64*exp(v*hrr)
  return(tr)
}

#calculate TRIMP over a list 
pts <- length(activities)
duration_list <- c()
hr_list <- c()
for (i in 1:pts) duration_list[i] <- activities[[i]]$moving_time
for (i in 1:pts) hr_list[i] <- activities[[i]]$average_heartrate
hrtrimps <- c()
for (i in 1:length(duration_list)){
  trimps[i] <- (Trimp(duration_list[i]/60,hr_list[i],50,180,"M"))
  }


# calculate performance

fitness_values <- c()
fatigue_values <- c()
performance_values <- c()
fitness = 0
fatigue = 0
r1 = 49
r2 = 11
k1 = 1.0
k2 = 1.8
for (i in 1:length(trimps)) {
  fitness = fitness * exp(-1 / r1) + trimps[i]
  fatigue = fatigue * exp(-1 / r2) + trimps[i]
  performance = fitness*k1 - fatigue*k2
  fitness_values[i] <- fitness
  fatigue_values[i] <- fatigue
  performance_values[i] <- performance
}
results <- data.frame(fitness_values,fatigue_values, performance_values)
colnames(results) <- c("fitness", "fatigue","performance")


library(ggplot2)
ggplot(results, aes(x=1:nrow(results))) + labs(x="days", y = "", title = "Banister: performance = fitness - fatigue") +
  geom_line(aes(y=performance, colour = "performance")) +
  geom_line(aes(y=fitness, colour = "fitness")) +
  geom_line(aes(y=fatigue, colour = "fatigue")) +
  scale_colour_manual("Legend", 
                      values = c("performance"="blue", "fitness"="green", "fatigue"="red")) 

