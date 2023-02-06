#Team project script for regression models

#Install packages

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("stats")
library(ggplot2)
library(dplyr)
library(stats)

#This data set is from SullyGnome, Top 200 Watched games channels from 6/21/20 - 6/21/21.
#I've added the game genre into each game.
#I've also removed channels which are not for a video game.

raw_data = read.csv(file.choose())
Twitch = raw_data
as.factor(Twitch$Genre)

#Here I reduce the watch and stream times to millions of hours

Twitch$Watch.time = Twitch$Watch.time / 1000000
Twitch$Stream.time = Twitch$Stream.time / 1000000

#I added in a ratio that is Watch / Stream time just to see what is the highest

Twitch = mutate(Twitch, Watch_to_Stream_Ratio = Watch.time / Stream.time)

#Here I split out the data set into the different genres

Action = filter(Twitch, Genre == "Action")
Action_Adventure = filter(Twitch, Genre == "Action-Adventure")
Adventure = filter(Twitch, Genre == "Adventure")
Fighting = filter(Twitch, Genre == "Fighting")
Misc = filter(Twitch, Genre == "Misc")
MMO = filter(Twitch, Genre == "MMO")
Platform = filter(Twitch, Genre == "Platform")
Puzzle = filter(Twitch, Genre == "Puzzle")
Racing = filter(Twitch, Genre == "Racing")
RPG = filter(Twitch, Genre == "RPG")
Shooter = filter(Twitch, Genre == "Shooter")
Simulation = filter(Twitch, Genre == "Simulation")
Sports = filter(Twitch, Genre == "Sports")
Strategy = filter(Twitch, Genre == "Strategy")

#Plot watch time versus stream time for channels with more than 10 games excluding Misc.

ggplot(Action, aes(x = Stream.time, y = Watch.time)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Stream Time (Millions of Hours)", y = "Watch Time (Millions of Hours)", title = "Watch Time vs. Stream Time for Action Games on Twitch 06/20 - 06/21") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(Action_Adventure, aes(x = Stream.time, y = Watch.time)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Stream Time (Millions of Hours)", y = "Watch Time (Millions of Hours)", title = "Watch Time vs. Stream Time for Action-Adventure Games on Twitch 06/20 - 06/21") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(RPG, aes(x = Stream.time, y = Watch.time)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Stream Time (Millions of Hours)", y = "Watch Time (Millions of Hours)", title = "Watch Time vs. Stream Time for RPG Games on Twitch 06/20 - 06/21") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(Shooter, aes(x = Stream.time, y = Watch.time)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Stream Time (Millions of Hours)", y = "Watch Time (Millions of Hours)", title = "Watch Time vs. Stream Time for Shooter Games on Twitch 06/20 - 06/21") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(Strategy, aes(x = Stream.time, y = Watch.time)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Stream Time (Millions of Hours)", y = "Watch Time (Millions of Hours)", title = "Watch Time vs. Stream Time for Strategy Games on Twitch 06/20 - 06/21") +
  theme(plot.title = element_text(hjust = 0.5))

#Run the linear models for the above plots

Action_model = lm(Watch.time~Stream.time, Action)
Action_Adventure_model = lm(Watch.time~Stream.time, Action_Adventure)
RPG_model = lm(Watch.time~Stream.time, RPG)
Shooter_model = lm(Watch.time~Stream.time, Shooter)
Strategy_model = lm(Watch.time~Stream.time, Strategy)

#Summarize the results
summary(Action_model)
summary(Action_Adventure_model)
summary(RPG_model)
summary(Shooter_model)
summary(Strategy_model)

#Place the stream time coefficients into a data frame to compare steepness of the lines

Game_Genres = c("Action", "Action-Adventure", "RPG", "Shooter", "Strategy")

Action_slope = Action_model$coefficients[2]
Action_Adventure_Slope = Action_Adventure_model$coefficients[2]
RPG_Slope = RPG_model$coefficients[2]
Shooter_Slope = Shooter_model$coefficients[2]
Strategy_Slope = Strategy_model$coefficients[2]

Slopes = c(Action_slope, Action_Adventure_Slope, RPG_Slope, Shooter_Slope, Strategy_Slope)

result = data.frame(cbind(Game_Genres, Slopes))
rownames(result) = c()
result = arrange(result,Slopes)

#The below result shows the relationship between the amount of time streamed versus watched for
#the genres Action, Action-Adventure, RPG, Shooter, and Strategy

result

# I interpret the results as "For every 1 million hours increase in time streamed for an Action game the watch time increases by 17.4 million hours")
# From the results we can see that RPG has the highest slope and Shooter as the lowest.

#Interpretation of the results could be that if a new twitch streamer wanted to choose which
#game genre to stream, they should choose RPG as it has the highest multiple of stream to watch time.
#Since we are assuming revenue is correlated to watch time this has the highest chance of generating
#a large audience during a stream on average for that genre. Certain games fall above and below the trend line
#indicating that specific games are inherently more or less popular to watch.





               