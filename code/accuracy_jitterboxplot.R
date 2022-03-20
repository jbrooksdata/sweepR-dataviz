library(sweepR) # data
library(dplyr) # various functions
library(ggplot2) # visualization output
library(ggthemes)

# load player gamelog, join for nation names, specify playoff teams
games <- load_player_games("mens")%>%
  left_join(load_standings("mens"),by = c("team" = "team_abbr")) %>%
  filter(team %in% c("USA","SWE","GBR","CAN"))

# order medal winners on x-axis manually
games$team.y <- factor(games$team.y,levels = c("Sweden","Great Britain","Canada","United States")) # manual reorder
  
# create viz
games %>%
  ggplot(aes(x = team.y, y = throw_accuracy, fill = team.y)) +
  geom_boxplot(show.legend = FALSE, outlier.shape = NA, alpha = 0.25, width = 0.6) +
  geom_jitter(show.legend = FALSE, width = 0.25, shape = 21, size = 3, alpha = 0.5, color = "black") +
  labs(title = "Overall Throw Accuracy of Men's Playoff Teams",
       subtitle = "How did individual accuracy percentage compare among the medal candidates?",
       caption = "@jbrooksdata | data: sweepR",
       x = " ", # no x axis label; spaces out caption
       y = "Throw Accuracy %") +
  theme_fivethirtyeight() +
  theme(axis.title.y.left = element_text(),
        axis.title.x.bottom = element_text(),
        axis.text.x = element_text(face="bold")) +
  scale_fill_manual(values = c("#FFCC33", # medal colors
                               "#C0C0C0",
                               "#CD7F32",
                               "#362436")) +
  ggsave("accuracy_jitterboxplot.png")
