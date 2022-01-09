library(pacman)
p_load(tidyverse, janitor, readxl, lubridate, stringi, showtext, cowplot)

font_add_google("Lato")
showtext_auto()

data_raw <- read_excel("laliga_all-time_winners.xlsx")

data_raw |> glimpse()

df <- data_raw |> 
  clean_names() |> 
  mutate(season_starts = as.numeric(str_extract(season, "[0-9]+")),
         season_ends = season_starts + 1)


df <- df |> 
  select(season_ends, winner) |> 
  mutate(value = 1) |> 
  pivot_wider(names_from = "winner", values_from = "value", values_fill = 0) |> 
  pivot_longer(cols = -season_ends, names_to = "winner") 

winner_details <- df |> 
  group_by(winner) |> 
  summarize(seasons_won = sum(value)) |> 
  ungroup()

winner_last_league <- df |> 
  filter(value == 1,
         !winner == "No se disputó por la Guerra Civil") |> 
  group_by(winner) |> 
  slice_max(season_ends, n = 1) |> 
  rename(last_league = season_ends)

winner_details <- winner_details |> 
  left_join(winner_last_league) |> 
  arrange(desc(seasons_won), desc(last_league)) 

df <- df |> 
  mutate(season_ends = as.Date(as.character(season_ends), format = "%Y"),
         season_ends = year(season_ends)) |>
  group_by(winner) |> 
  mutate(seasons_won = sum(value)) |> 
  ungroup() |> 
  filter(value == 1)

label_years <- data.frame(years = as.double(c(1929:2021))) |> 
  mutate(labels = ifelse(years %in% c(1929, 2021), years, ""))

plot <- df |>   
  ggplot() +
  geom_vline(aes(xintercept = season_ends - .4), size = 2, color = "black") +
  facet_wrap(~ winner, ncol = 1, strip.position = 'left') +
  geom_text(x = 2025, y = .5, aes(label = seasons_won), data = df,
            size = 4, hjust = .5) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(1928, 2021),
                     breaks = label_years$years,
                     expand = c(0, 0),
                     labels = label_years$labels) +
  theme_minimal(base_family = "Lato") +
  theme(plot.margin = margin(c(85, 200, 85, 50)),
        panel.background = element_rect(fill = "grey75", color = "grey75"),
        plot.background = element_rect(fill = "#f0f0f0", color = "#f0f0f0"),
        panel.border = element_rect(color = "black", fill = NA, size = .5),
        panel.grid = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        strip.text = element_text(hjust = 1, size = 12,
                                  face = "bold",
                                  margin = margin(r = 10)),
        axis.text = element_text(size = 10),
        axis.title.x = element_blank())

ggdraw(plot) +
  draw_image("laliga.png",
             vjust = -4.7, hjust = -2.5,
             width = .18, height = .18)         


ggsave("laliga_all-time_winners.png", width = 10, height = 10, units = "in", dpi = 320)



df <- df |> 
  mutate(value = case_when(winner == "No se disputó por la Guerra Civil" ~ 0,
                                    TRUE ~ value),
         winner = case_when(winner == "No se disputó por la Guerra Civil" ~ "Real Madrid",
                            TRUE ~ winner),
         season_ends_fct = factor(season_ends)) |> 
  group_by(winner) |> 
  mutate(seasons_won = sum(value)) |> 
  ungroup() |> 
  mutate(winner_relevel = fct_reorder(winner, seasons_won, .desc = TRUE),
         value = factor(value))  
  
df |> 
  ggplot() +
  geom_tile(aes(x = season_ends_fct, y = 0, fill = value), width = 1,,
            show.legend = FALSE) +
  scale_fill_manual(values = c("white", "#1b8dcc")) + 
  facet_wrap(~ winner_relevel, ncol = 1, strip.position = 'left') +
  geom_text(x = 2025, y = .5, aes(label = seasons_won), data = df,
            size = 4, hjust = .5) +
  coord_cartesian(clip = "off") +
  scale_x_discrete(labels = c("1929", rep("", 91), "2021"),
                   expand = c(0, 0)) +
  theme_minimal(base_family = "Lato") +
  theme(plot.margin = margin(c(20, 40, 20, 40)),
        panel.background = element_rect(fill = "grey75", color = "grey75"),
        plot.background = element_rect(fill = "#f0f0f0", color = "#f0f0f0"),
        panel.border = element_rect(color = NA, fill = NA, size = .1),
        panel.grid = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        strip.text = element_text(hjust = 1, size = 12,
                                  face = "bold",
                                  margin = margin(r = 10)),
        axis.text = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.title = element_blank())
