library(pacman)
p_load(tidyverse, janitor, worldfootballR, camcorder, showtext)


font_add_google("Lato")
font_add(family = "Lato black",
         regular = "/Users/pabloalvarezbaeza/Library/Fonts/Lato-Black.ttf")

showtext_auto()
gg_record(dir = "temp_worldfootballR", device = "png", width = 3.375, height = 3.375, units = "in", dpi = 320)

# Vignette extracting data from fotmob
# https://jaseziv.github.io/worldfootballR/articles/extract-fotmob-data.html


# Data --------------------------------------------------------------------
cl_players <- fotmob_get_season_stats(
  country = "INT",
  league_name = "Champions League", 
  season = c("2016/2017", "2017/2018", "2018/2019", "2019/2020","2020/2021", "2021/2022"),
  stat_name = "Goals + Assists",
  team_or_player = "player"
) |> 
  mutate(group = factor(if_else(participant_name == "Karim Benzema", 1, 2)))

player_color <- cl_players |> 
  mutate(row_number()) |> filter(participant_name == "Karim Benzema") |> select(team_color) |> pull()

# Plot --------------------------------------------------------------------
cl_players_stats <- cl_players |> 
  filter(stat_value >= 1) |> 
  group_by(participant_name, season_name) |> 
  mutate(goals_assists_per_game = round(stat_value / matches_played, 1), .after = stat_value) |> 
  ungroup()

cl_players_stats |> 
  distinct(participant_name)

cl_players_stats |> 
  filter(season_name == "2019/2020") |> 
  slice_max(goals_assists_per_game, n = 5) |> View()
 
  
cl_players_stats |> 
  ggplot(aes(season_name, goals_assists_per_game, group = participant_name)) +
  geom_line(data = cl_players_stats |> filter(!participant_name %in% c("Cristiano Ronaldo", "Karim Benzema")), alpha = .15, size = .25, color = "grey50") +
  geom_line(data = cl_players_stats |> filter(participant_name == "Cristiano Ronaldo",
                                              season_name %in% c("2016/2017", "2017/2018")), size = 1, color = "black") +
  geom_line(data = cl_players_stats |> filter(participant_name == "Cristiano Ronaldo",
                                              !season_name %in% c("2016/2017")), alpha = .15, size = 1, color = "grey50") +
  
  geom_line(data = cl_players_stats |> filter(group == 1), size = 1, color = "#0072B8") +
  geom_point(data = cl_players_stats |> filter(group == 1, season_name == "2021/2022"), size = 2, color = "#0072B8") +
  scale_x_discrete(labels = c("2016-17", "'17-'18", "'18-'19", "'19-'20", "'20-'21", "'21-'22")) +
  scale_y_continuous(limits = c(0, 2)) +
  coord_cartesian(expand = c(0, 0),
                  clip = "off") +
  theme_minimal(base_family = "Lato") +
  labs(x = "Season",
       y = "Goals plus assists per game",
       title = "The Rise of Karim Benzema",
       subtitle = "Offensive production of players in the UEFA Champions League",
       caption = "Visualization by Pablo Alvarez | Data from Fotmob\nOnly including data from players with at least 1 goal or assistance") +
  theme(
    plot.margin = margin(rep(10, 4)),
    panel.grid = element_blank(),
    panel.grid.major = element_line(size = .15, color = "grey75"),
    plot.background = element_rect(fill = "#D2D2EB", color = "#D2D2EB"),
    panel.background = element_rect(fill = "#D2D2EB", color = "#D2D2EB"),
    axis.title.y = element_text(angle = 90, hjust = .99, size = 20, color = "grey25"),
    axis.title.x = element_text(hjust = 0, size = 20, color = "grey15"),
    axis.text = element_text(size = 16, color = "grey50"), 
    axis.line = element_line(color = "grey75", size = .15),
    plot.title = element_text(size = 48,
                              color = "#0072B8",
                              family = "Lato black",
                              hjust = 0),
    plot.subtitle = element_text(size = 22,
                                 color = "#0072B8",
                                 family = "Lato",
                                 hjust = 0,
                                 margin = margin(b = 20, t = -3.5)),
    plot.caption = element_text(size = 16,
                                color = "grey50",
                                hjust = 0,
                                margin = margin(t = 20),
                                lineheight = .3)
  )

ggsave("the_rise_of_karim_benzema.png", width = 1080, height = 1080, units = "px", dpi = 320)


# Plot 2 ------------------------------------------------------------------
xg_players <- fotmob_get_season_stats(
  country = "INT",
  league_name = "Champions League", 
  season = "2021/2022",
  stat_name = "Expected goals (xG)",
  team_or_player = "player"
) |> 
  mutate(group = factor(if_else(participant_name == "Karim Benzema", 1, 2)))
topten <- xg_players |> slice_max(stat_value, n = 10) 

xg_players |> 
  filter(minutes_played >= 90) |>
  mutate(group_color = factor(case_when(participant_name == "Karim Benzema" ~ 1,
                                        participant_name %in% c("Robert Lewandowski", "Lionel Messi", "Mohamed Salah", "Cristiano Ronaldo") ~ 2,
                                        TRUE ~ 3))) |> 
  ggplot(aes(stat_value, sub_stat_value, fill = group, color = group_color, alpha = group)) +
  geom_point(shape = 21, stroke = .25) +
  geom_smooth(method = "lm", se = FALSE, size = .25, color = "grey25") +
  scale_fill_manual(values = c("#FF3EB5", "grey50")) +
  scale_color_manual(values = c("black", "black", NA)) +
  scale_alpha_manual(values = c(1, .35)) +
  scale_x_continuous(limits = c(0, 12),
                     breaks = seq(0, 12, 2)) + 
  scale_y_continuous(limits = c(0, 16)) +
  coord_cartesian(expand = c(0, 0),
                  clip = "off") +
  theme_minimal(base_family = "Lato") +
  labs(x = "Expected Goals (xG)",
       y = "Goals",
       title = "Benzema nearly Doubles his Expected Goals",
       subtitle = "Players' expected goals vs. goals scored in the UEFA Champions League (2021/22)",
       caption = "Visualization by Pablo Alvarez | Data from Fotmob\nOnly including data from players who have played at least 90 minutes") +
  theme(
    plot.margin = margin(rep(10, 4)),
    panel.grid = element_blank(),
    panel.grid.major = element_line(size = .15, color = "grey75"),
    plot.background = element_rect(fill = "grey90", color = "grey90"),
    panel.background = element_rect(fill = "grey90", color = "grey90"),
    axis.title.y = element_text(angle = 90, hjust = .99, size = 16, color = "grey50"),
    axis.title.x = element_text(hjust = 0, size = 16, color = "grey50"),
    axis.text = element_text(size = 16, color = "grey50"), 
    axis.line = element_line(color = "grey75", size = .15),
    plot.title = element_text(size = 33,
                              color = "#FF3EB5",
                              family = "Lato black",
                              hjust = 0),
    plot.subtitle = element_text(size = 18,
                                 color = "grey25",
                                 family = "Lato",
                                 hjust = 0,
                                 margin = margin(b = 20, t = -3.5)),
    plot.caption = element_text(size = 16,
                                color = "grey50",
                                hjust = 0,
                                margin = margin(t = 20),
                                lineheight = .3),
    legend.position = "none"
  )

ggsave("Benzema xG.png", width = 1080, height = 1080, units = "px", dpi = 320)


