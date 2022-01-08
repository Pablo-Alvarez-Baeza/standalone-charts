library(pacman)
p_load(tidyverse, janitor, readxl, lubridate, stringi)

data_raw <- read_excel("laliga_all-time_winners.xlsx")

data_raw |> glimpse()

df <- data_raw |> 
  clean_names() |> 
  mutate(season_starts = as.numeric(str_extract(season, "[0-9]+")),
         season_ends = season_starts + 1)

Tdate = c("2020-04-20", "2020-04-22","2020-05-16","2020-05-29", "2020-06-20", "2020-07-02", "2020-07-18", "2020-07-19", "2020-07-22", "2020-09-14", "2020-10-10", "2020-10-15", "2020-11-22", "2020-12-22", "2020-12-24", "2020-12-25")
Tevents = data.frame(station1=c(1,0,0,1,1,1,1,0,0,0,1,1,0,1,0,1),station2=c(1,0,1,1,0,1,1,0,1,1,1,1,0,1,1,1), station3=c(0,1,1,1,1,0,1,1,1,1,1,1,1,1,1,0) )
Zevents<-zoo(Tevents,as.Date(Tdate))

Tevents |> View()

df <- df |> 
  select(season_ends, winners) |> 
  mutate(value = 1) |> 
  pivot_wider(names_from = "winners", values_from = "value", values_fill = 0) |> 
  pivot_longer(cols = -season_ends, names_to = "winners") 

winners_levels <- df |> 
  group_by(winners) |> 
  summarize(seasons_won = sum(value)) |> 
  arrange(desc(seasons_won)) 

winners_titles <- df |> 
  group_by(winners) |> 
  summarize(seasons_won = sum(value)) |> 
  arrange(desc(seasons_won)) 

dat_text <- data.frame(
  label = winners_titles$seasons_won,
  winners   = winners_titles$winners
)

df <- df |> 
  mutate(season_ends = as.Date(as.character(season_ends), format = "%Y"),
         season_ends = year(season_ends),
         winners = factor(winners, levels = winners_levels$winners)) |>
  group_by(winners) |> 
  mutate(seasons_won = sum(value)) |> 
  ungroup() |> 
  filter(value == 1)

label_years <- data.frame(years = as.double(c(1928:2022))) |> 
  mutate(labels = ifelse(years %in% c(1929, 1975, 2021), years, ""))

df |>   
  ggplot() +
  geom_vline(aes(xintercept = season_ends - .5), size = 2, color = "purple") +
  facet_wrap(~ winners, ncol = 1, strip.position = 'left') +
  geom_text(x = 2023, y = .5, aes(label = label), data = dat_text, size = 2) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(1928, 2021),
                     breaks = label_years$years,
                     expand = c(0, 0),
                     labels = label_years$labels) +
  theme(plot.margin = margin(c(20, 40, 20, 40)),
        panel.background = element_rect(fill = "grey95", color = "grey95"),
        panel.grid = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        strip.text = element_text(size = 3))
        
        axis.title.y       = element_blank(),
        axis.text.y        = element_blank(),
        axis.ticks.y       = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        )
  

ggplot(data) + geom_point(aes(x = dat, y = 2))+
  geom_vline(aes(xintercept  = dat), data = filter(data,value == 1)) +
  coord_cartesian(ylim = c(0,1))
