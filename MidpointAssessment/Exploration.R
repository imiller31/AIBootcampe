library(tidyverse)
events <- read_csv("data/events.csv") %>% mutate_if(is.character, factor)
matchdetails <- read_csv("data/matchdetails.csv") %>% mutate_if(is.character, factor)
teams <- read_csv("data/teams.csv") %>% mutate_if(is.character, factor)

matchdetails.columns

foulDetails <- select(matchdetails,"winning_alliance", 
                            "alliances.red.score", 
                            "alliances.blue.score", 
                            "score_breakdown.red.techFoulCount",
                            "score_breakdown.blue.techFoulCount",
                            "score_breakdown.red.foulPoints",
                            "score_breakdown.blue.foulPoints")

blueWinnersFoulDetails <- select(subset(foulDetails, foulDetails$winning_alliance == "blue"), 
                                 "alliances.blue.score",
                                 "score_breakdown.blue.techFoulCount",
                                 "score_breakdown.blue.foulPoints")

redWinnersFoulDetails <- select(subset(foulDetails, foulDetails$winning_alliance == "red"), 
                                "alliances.red.score",
                                "score_breakdown.red.techFoulCount",
                                "score_breakdown.red.foulPoints")

blueWinnersFoulDetails <- rename(blueWinnersFoulDetails, WinningScore = alliances.blue.score, 
       TechFoulCount= score_breakdown.blue.techFoulCount,
       FoulPoints = score_breakdown.blue.foulPoints)

redWinnersFoulDetails <- rename(redWinnersFoulDetails, WinningScore = alliances.red.score, 
       TechFoulCount= score_breakdown.red.techFoulCount,
       FoulPoints = score_breakdown.red.foulPoints)

foulDetails <- rbind(blueWinnersFoulDetails, redWinnersFoulDetails)

blueWinnerDetails <- rename(select(subset(matchdetails, matchdetails$winning_alliance == "blue"), 
                               "winning_alliance",
                               "alliances.blue.score"),
                            WinningScore = alliances.blue.score)
redWinnerDetails <- rename(select(subset(matchdetails, matchdetails$winning_alliance == "red"), 
                                  "winning_alliance",
                                  "alliances.red.score"),
                           WinningScore = alliances.red.score)

winnerDetails <- rbind(redWinnerDetails, blueWinnerDetails)

g <- ggplot(winnerDetails, aes(x = winning_alliance, y = WinningScore, color = winning_alliance))
g + geom_boxplot()

blueWinnerDetails <- matchdetails %>% 
  subset(., .$winning_alliance == "blue") %>% 
  select(., "winning_alliance", "alliances.blue.score") %>%
  rename(., WinningScore = alliances.blue.score)

blueData <- matchdetails %>% select(., contains("blue"))
redData <- matchdetails %>% select(., contains("red"))

ggplot(data = blueData, aes(x = alliances.blue.score)) + 
  geom_histogram(aes(y=..density..), bins = 20,
                                     color = "seagreen",
                                     fill = "lightblue") + 
  geom_density(color = "blue") +
  labs(title = "Blue")

ggplot(data = winnerDetails, aes(x = WinningScore)) + 
  geom_histogram(position = "dodge", aes(y = ..density.., fill = winnerDetails$winning_alliance)) +
  geom_density(aes(color = winnerDetails$winning_alliance))

  