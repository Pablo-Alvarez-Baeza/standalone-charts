library(pacman)
p_load(tidyverse, readxl, ggtext, patchwork)

font_add_google("Lato")
showtext_auto()

# Inspiration and credits to
# https://www.kickstarter.com/projects/szabohaslam/waveform

df <- read_excel("lyrics.xlsx") |> 
  mutate(n_bruno_lines = factor(if_else(str_detect(line, "Bruno"), 1, 0))) 

# What is the number of lines 'Bruno' appears in?
df |> 
  group_by(n_bruno_lines) |> 
  count() |> 
  ungroup() |> 
  mutate(sum = sum(n),
         perc = round(n / sum, 2))

# How many times is 'Bruno' mentioned?
df |> 
  mutate(n_bruno_times = stri_count(line, fixed = "Bruno")) |> 
  summarize(sum(n_bruno_times))


df |> 
  ggplot(aes(as.factor(value), n_characters, fill = n_bruno_lines, color = n_bruno_lines)) +
  geom_bar(stat = "identity", show.legend = FALSE, size = .1, width = .5) +
  ylim(-35,100) +
  coord_polar(start = 0,
              clip = "off") +
  scale_fill_manual(values = c("black", "#49FF00")) +
  scale_color_manual(values = c("white", "black")) +
  annotate(geom = "text", label = df$line[1], x = .9, y = 80, angle = 90, size = 3, color = "white", family = "Lato") +
  annotate(geom = "text", label = df$line[46], x = 45.9, y = 93, angle = 0, size = 3, color = "white", family = "Lato") +
  theme_void() +
  labs(title = "We Do Actually Talk About <span style='color:#49FF00'>**Bruno**</span>",
       caption = "Visualization by Pablo Alvarez") +
  theme(
    plot.margin = margin(10, 10, 10, 10),
      panel.background = element_rect(fill = "black", color = "black"),
      plot.background = element_rect(fill = "black", color = "black"),
    plot.title = element_markdown(
      color = "white",
      face = "bold",
      size = 24,
      margin = margin(t = 20, b = 60),
      hjust = .5
    ),
    plot.caption = element_text(
      color = "white",
      size = 8,
      hjust = .5,
      margin = margin(b = 10)
  )
  )

ggsave("plot.png", width = 10, height = 10, dpi = 320)
