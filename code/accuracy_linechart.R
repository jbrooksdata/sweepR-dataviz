library(sweepR) # data
library(dplyr) # various functions
library(ggplot2) # visualization output
library(ggthemes)

# load gamelog dataframe, specify playoff teams, create rolling avg attribute
mens_games <- load_team_games("mens") %>%
  filter(team %in% c("GBR","USA","CAN","SWE")) %>%
  summarise(team,opp,team_game_number,stage,throws,throw_accuracy) %>%
  group_by(team) %>% 
  mutate(rec = 1) %>%
  mutate(rollavg = cumsum(throw_accuracy)/cumsum(rec)) %>%
  select(-rec)

# create viz
mens_games %>%
  ggplot(aes(x = team_game_number, y = rollavg, color = team)) +
  geom_line(size = 1.25) + # increase line weight
  ylim(80,90) + # y axis range
  labs(title = "Men's Finalists - Overall Throwing Accuracy",
       subtitle = "How does accuracy change as the tournament progresses?",
       caption = "@jbrooksdata | data: sweepR",
       x = "Game Number",
       y = "Running Throw Accuracy %",
       color = "Nation:") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text()) + # show x and y labels
  scale_color_manual(values = c("#CD7F32", #line colors: gold, silver, bronze, black
                                "#C0C0C0",
                                "#FFCC33",
                                "#363636")) +
  ggsave("accuracy_linechart.png")
