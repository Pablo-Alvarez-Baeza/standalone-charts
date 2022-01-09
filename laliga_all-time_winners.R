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
  filter(value == 1) |> 
  group_by(winner) |> 
  slice_max(season_ends, n = 1) |> 
  rename(last_league = season_ends)

winner_details <- winner_details |> 
  left_join(winner_last_league) |> 
  arrange(desc(seasons_won), desc(last_league)) |> 
  filter(!winner == "No se disputó por la Guerra Civil")

df <- df |> 
  mutate(season_ends = as.Date(as.character(season_ends), format = "%Y"),
         season_ends = year(season_ends),
         winner = factor(winner, levels = winner_details$winner)) |>
  group_by(winner) |> 
  mutate(seasons_won = sum(value)) |> 
  ungroup() |> 
  filter(value == 1,
         !winner == "No se disputó por la Guerra Civil")

label_years <- data.frame(years = as.double(c(1928:2022))) |> 
  mutate(labels = ifelse(years %in% c(1929, 2021), years, ""))

plot <- df |>   
  filter() |> 
  ggplot() +
  geom_vline(aes(xintercept = season_ends - .5), size = 1.6, color = "black") +
  facet_wrap(~ winner, ncol = 1, strip.position = 'left') +
  geom_text(x = 2023, y = .5, aes(label = seasons_won), data = df,
            size = 2) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(1928, 2021),
                     breaks = label_years$years,
                     expand = c(0, 0),
                     labels = label_years$labels) +
  theme_minimal(base_family = "Lato") +
  theme(plot.margin = margin(c(50, 100, 20, 40)),
        panel.background = element_rect(fill = "grey75", color = "grey75"),
        plot.background = element_rect(fill = "#f0f0f0", color = "#f0f0f0"),
        panel.grid = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        strip.text = element_text(hjust = 1, size = 5, face = "bold", family = "Lato Black (900)"),
        axis.text = element_text(size = 5),
        axis.title.x = element_blank())

ggdraw(plot) +
  draw_image("laliga.png",
             vjust = -4.7, hjust = -2.2,
             width = .18, height = .18)         


ggsave("laliga_all-time_winners.png", width = 8.5, height = 10, units = "in", dpi = 320)
