library(sweepR) # data
library(dplyr) # various functions
library(ggplot2) # visualization output
library(ggthemes)

# load player gamelog, specify USA games, filter inactive players
womens_games <- load_player_games("womens")%>%
  filter(team == "USA")%>%
  filter(throws != "0")

team_avg <- womens_games %>% # calculate team average
  pull(throw_accuracy) %>%
  mean() %>%
  signif(3)

# create viz
womens_games %>%
  ggplot(aes(x = throw_accuracy,fill = team)) +
  geom_density(show.legend = FALSE) +
  labs(title = "Tabitha Peterson's Final Three Games",
       subtitle = "How did the United States captain perform down the stretch?",
       caption = "@jbrooksdata | data: sweepR",
       x = "Throw Accuracy %",
       y = "Density") +
  geom_vline(xintercept = womens_games$throw_accuracy[c(28,32,36)], # Peterson's final 3 games
             alpha = 0.4,
             size = 1.5,
             color = "red") +
  geom_vline(xintercept = team_avg, # team average - dashed line
             alpha = 0.75,
             size = 0.5,
             linetype = "dashed") +
  scale_fill_manual(values = "#c9dbff") +
  ylim(0,0.04) +
  theme_fivethirtyeight() +
  theme(axis.title.y.left = element_text(),
        axis.title.x.bottom = element_text(face = "bold"))

ggsave("accuracy_densityplot.png")
