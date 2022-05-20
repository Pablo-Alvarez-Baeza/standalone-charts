library(pacman)
p_load(tidyverse, janitor, worldfootballR, camcorder, showtext)


font_add_google("Lato")
font_add(family = "Lato black",
         regular = "/Users/pabloalvarezbaeza/Library/Fonts/Lato-Black.ttf")

showtext_auto()
gg_record(dir = "temp_benzema_cristiano", device = "png", width = 3.375, height = 1.768, units = "in", dpi = 320)


# Data --------------------------------------------------------------------
benzema <- fb_player_season_stats("https://fbref.com/en/players/70d74ece/Karim-Benzema", stat_type = 'shooting') |> 
  clean_names() |> 
  filter(squad == "Real Madrid")

cristiano <- fb_player_season_stats("https://fbref.com/en/players/dea698d9/Cristiano-Ronaldo", stat_type = 'shooting') |> 
  clean_names() |> 
  filter(!season %in% c("2002-2003", "2003-2004", "2004-2005", "2005-2006",
                        "2006-2007", "2007-2008", "2008-2009"))

# Combined and filtered players data
df <- bind_rows(benzema, cristiano) |> 
  select(player_name, season, squad, comp, gls_standard) 

goals_by_year <- df |> 
  group_by(player_name, season) |> 
  summarize(total_goals = sum(gls_standard)) |> 
  ungroup() 

cristiano_goals <- goals_by_year |> 
  filter(player_name == "Cristiano Ronaldo") |> 
  summarize(min_goals = min(season),
            max_goals = max(total_goals))

# Plot --------------------------------------------------------------------
goals_by_year |> 
  ggplot(aes(season, total_goals, group = player_name)) +
  geom_line(data = goals_by_year |> filter(player_name == "Cristiano Ronaldo")) +
  geom_line(data = goals_by_year |> filter(player_name == "Karim Benzema")) +
  geom_segment(x = c(10:13, xend = "2021-2022", y = 0, yend = 60, color = "red")

