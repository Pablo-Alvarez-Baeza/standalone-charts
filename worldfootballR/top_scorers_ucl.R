library(pacman)
p_load(tidyverse, janitor, readxl, camcorder, showtext, qdapRegex, stringi)


font_add_google("Lato")
font_add(family = "Lato black",
         regular = "/Users/pabloalvarezbaeza/Library/Fonts/Lato-Black.ttf")

showtext_auto()
gg_record(dir = "temp_ucl_scorers", device = "png", width = 3.375, height = 1.908, units = "in", dpi = 320)


# Data --------------------------------------------------------------------
# All time except 2021/22
# https://www.uefa.com/uefachampionsleague/news/0257-0e910cf2494a-5185150de9d4-1000--all-time-top-scorers/
# 2021-22
# https://www.uefa.com/uefachampionsleague/news/026d-13405646aa7e-10d35549dfdf-1000--champions-league-top-scorers-robert-lewandowski-leads-the-way/

df_raw <- read_excel("UEFA Champions League top scorers by season.xlsx") 

df <- df_raw |> 
  mutate(data = stringi::stri_trans_general(str = data, id = "Latin-ASCII"), # Remove accents from the column
         season = str_extract(data, "[^:]+"),
         player = str_trim(str_extract(data, "[ a-zA-Z]+")),
         team = qdapRegex::rm_round(data, extract = TRUE),
         goals = as.double(str_extract(data, "\\b\\w+$")))


# Plot --------------------------------------------------------------------
df |> View()
  filter(pla) |> 
  arrange(season)
  
goals <- df |> 
  arrange(season) |> 
  distinct(season, goals) |> 
  select(goals) |>
  pull()

df |> 
  mutate(group_color = factor(case_when(player == "Lionel Messi" & season != "2014/15" ~ 1,
                                 player == "Cristiano Ronaldo" & season != "2014/15" ~ 2,
                                 player == "Karim Benzema" ~ 3,
                                 TRUE ~ 4)),
         season = factor(season)) |>
  distinct(season, goals, group_color) |>
  ggplot(aes(season, goals, fill = group_color, alpha = group_color)) +
  geom_col(width = .75) +
  scale_x_discrete(labels = c("1992/93", rep("", 4), "97/98", rep("", 9), "07/08", rep("", 10), "18/19", rep("", 2), "21/22\n(*)")) +
  scale_fill_manual(values = c("#CCADD3", "#C9E5E0", "#FFE900", "grey75")) +
  scale_alpha_manual(values = c(rep(1, 3), .5)) +
  coord_cartesian(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 19)) +
  geom_text(aes(label  = goals, color = group_color), family = "Lato", size = 4, vjust = -.5) +
  scale_color_manual(values = c("#CCADD3", "#C9E5E0", "#FFE900", "#003170")) +
  annotate("text", x = 1.5, y = 16.8, label = "UEFA Champions League\ntop scorers by season", lineheight = .3, hjust = 0, family = "Lato bold", size = 8, color = "white") +
  labs(x = "Season", y = "Goals (group stage to final)",
       caption = "Visualization by Pablo Alvarez | Data from UEFA.com\n(*) The UEFA Champions League Final remains to be played") +
  theme_minimal() +
  theme(
    plot.margin = margin(c(20, 10, 5, 10)),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(size = .05, color = "grey90"),
    plot.background = element_rect(fill = "#003170", color = "#003170"),
    panel.background = element_rect(fill = "#003170", color = "#003170"),
    axis.title.y = element_text(angle = 90, hjust = .7, size = 12, color = "white"),
    axis.title.x = element_text(hjust = 0, size = 12, color = "white", margin = margin(t = -.5)),
    axis.text = element_text(size = 10, color = "white"), 
    axis.text.x = element_text(margin = margin(t = 0), lineheight = .25),
    axis.line.x = element_line(color = "white", size = .25),
    plot.caption = element_text(size = 10,
                                color = "white",
                                hjust = 0,
                                lineheight = .3),
    legend.position = "none"
  )

ggsave("top_scorers_ucl.png", width = 1080, height = 566, units = "px", dpi = 320)

