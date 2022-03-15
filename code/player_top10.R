library(sweepR) # data
library(dplyr) # various functions
library(gt) # graphic table output

# load player log dataframe, join team logos and squad info
player_gamelog <- load_player_games("mixed")%>%
  left_join(load_standings("mixed"),by = c("team" = "team_abbr"))%>%
  left_join(load_rosters("mixed"),by = c("player_id" = "player_id"))

# create table
player_gamelog%>%
  filter(stage != "Round-Robin")%>%
  summarise(Player=paste0(first," ",last), #combine player's first and last name
            Team=logo,
            Stage=stage,
            Throws=throws,
            Accuracy=throw_accuracy)%>%
  arrange(desc(Accuracy))%>%
  slice_head(n=10)%>% # return top 10 values
  gt()%>% # initiate table
  text_transform(
    locations = cells_body(Team),
    fn = function(x) {
      web_image(
        url = x)})%>%
  data_color(
    columns = c(Accuracy),
    colors = scales::col_numeric(
      palette = c("#E54343","#F5F5F5","#4395E5"),
      domain = c(50,100)))%>%
  cols_align(
    align = "center",
    columns = c(Team:Accuracy)
  )%>%
  tab_header(
    title = md("**Mixed Doubles Player Comparison**"),
    subtitle = ("Overall Throw Accuracy - Playoff Performances")
  )%>%
  tab_source_note(
    source_note = "@jbrooksdata | data: nbc via sweepR"
  )%>%
  tab_style(style = cell_text(align = "right"), locations = cells_source_notes())%>%
  gtsave(filename = "player_top10.png")
