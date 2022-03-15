library(sweepR) # data
library(dplyr) # various functions
library(gt) # graphic table output

# load team log dataframe and join team logos
team_gamelog <- load_team_games("mens")%>%
  left_join(load_standings("mens"),by = c("team" = "team_abbr"))

# create table
team_gamelog%>%
  summarise(Team=logo,Stage=stage,Opponent=opp,Throws=throws,TO_Rate=round(takeouts/throws,2))%>% # select desired columns
  arrange(desc(TO_Rate))%>%
  slice_head(n=10)%>% # return top 10 values
  gt()%>% # initiate table
  text_transform(
    locations = cells_body(Team),
    fn = function(x) {
      web_image(
        url = x)})%>%
  data_color(
    columns = c(TO_Rate),
    colors = scales::col_numeric(
      palette = c("#E54343","#F5F5F5","#4395E5"),
      domain = c(.25,.75)))%>%
  cols_align(
    align = "center",
    columns = c(Team:TO_Rate)
  )%>%
  tab_header(
    title = md("**Men's Tournament Team Comparison**"),
    subtitle = ("Takeout Rate - Top 10 Performances")
  )%>%
  tab_source_note(
    source_note = "@jbrooksdata | data: nbc via sweepR"
  )%>%
  tab_style(style = cell_text(align = "right"), locations = cells_source_notes())%>%
  gtsave(filename = "team_top10.png")
