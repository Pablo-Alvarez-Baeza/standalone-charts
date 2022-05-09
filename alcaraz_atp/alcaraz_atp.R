library(pacman)
p_load(tidyverse, janitor, readxl, camcorder, showtext, lubridate, MESS)

gg_record(dir = "temp_worldfootballR", device = "png", width = 3.375, height = 3.375, units = "in", dpi = 320)

font_add_google("Lato")
font_add(family = "Lato black",
         regular = "/Users/pabloalvarezbaeza/Library/Fonts/Lato-Black.ttf")

showtext_auto()

# Data --------------------------------------------------------------------
# ATP
# https://www.atptour.com/en/players/carlos-alcaraz/a0e2/rankings-history

# ATP Calendar by year
# https://www.eurosport.com/tennis/atp/calendar-result_sea336.shtml

today <- today()
birthday <- "2003-05-05"

df <- read_excel("alcaraz_atp.xlsx") |> 
  clean_names() |> 
  rename(date_ranking = fecha) |> 
  mutate(date_ranking = str_replace_all(date_ranking, "\\.", "/"),
         age_years = MESS::age(birthday, date_ranking),
         date_ranking = ymd(date_ranking)) |> 
  filter(age_years >= 16)

dates_old <- df |> 
  group_by(age_years) |> 
  slice_min(date_ranking, n = 1)

dates_titles <- ymd(c("2021-07-26",
                      "2022-02-21",
                      "2022-04-04",
                      "2022-04-25",
                      "2022-05-09"))

df |> 
  filter(date_ranking %in% dates_titles) |> View()


df |> 
  ggplot(aes(date_ranking, singles)) +
  geom_line(color = "#234797") +
  geom_point(data = df |> filter(date_ranking %in% dates_titles), shape = 21, color = "black", fill = "#F68D2E", stroke = .25, size = 1) +
  #geom_point(data = df |> filter(date_ranking %in% dates_old$date_ranking), color = "green") +
  scale_y_reverse(limits = c(600, 1),
                  breaks = seq(1, 600, 51),
                  expand = c(0, 0))  +
  theme_minimal(base_family = "Lato") +
  labs(x = NULL,
       y = "ATP Ranking",
       title = "Carlos Alcaraz: A Rising Star",
       subtitle = "ATP ranking position by week since he was 16 years old",
       caption = "Visualization by Pablo Alvarez | Data from ATP"
       ) +
    theme(
      plot.margin = margin(rep(10, 4)),
      panel.grid = element_blank(),
      panel.grid.major = element_line(size = .15, color = "grey75"),
      plot.background = element_rect(fill = "#C6D6BE", color = "#C6D6BE"),
      panel.background = element_rect(fill = "#C6D6BE", color = "#C6D6BE"),
      axis.title.y = element_text(angle = 90, hjust = .99, size = 20, color = "grey25"),
      axis.title.x = element_text(hjust = 0, size = 20, color = "grey15"),
      axis.text = element_text(size = 16, color = "grey50"), 
      axis.line = element_line(color = "grey75", size = .15),
      plot.title = element_text(size = 48,
                                color = "#234797",
                                family = "Lato black",
                                hjust = 0),
      plot.subtitle = element_text(size = 22,
                                   color = "#234797",
                                   family = "Lato",
                                   hjust = 0,
                                   margin = margin(b = 20, t = -3.5)),
      plot.caption = element_text(size = 16,
                                  color = "grey50",
                                  hjust = 0,
                                  margin = margin(t = 20),
                                  lineheight = .3)
    )

ggsave("Carlos Alcaraz: A Rising Star.png", width = 1080, height = 1080, units = "px", dpi = 320)

  
