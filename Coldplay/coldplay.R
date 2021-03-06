library(pacman)
p_load(tidyverse, spotifyr, ggridges, patchwork, showtext)


# Fonts -------------------------------------------------------------------
font_add_google("Lato")
font_add(family = "Lato Bold",
         regular = "Lato-Bold.ttf")
font_add(family = "Coldplay",
         regular = "COLDPLAY.ttf")

showtext_auto()


# Spotify data ------------------------------------------------------------
# You need to enter your credentials. Read how here
# https://www.rcharlie.com/spotifyr/
Sys.setenv(SPOTIFY_CLIENT_ID = 'xxx')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxx')


access_token <- get_spotify_access_token()

coldplay_raw <- get_artist_audio_features('coldplay')


# Final data set ----------------------------------------------------------
studio_albums <- c("Parachutes",
                   "A Rush of Blood to the Head",
                   "X&Y",
                   "Viva La Vida or Death and All His Friends",
                   "Mylo Xyloto",
                   "Ghost Stories",
                   "A Head Full of Dreams",
                   "Everyday Life",
                   "Music Of The Spheres")

coldplay <- coldplay_raw |> 
  select(album_name, album_release_year, track_name, track_number, danceability:tempo) |> 
  filter(album_name %in% studio_albums) |>
  as_tibble() |> 
  distinct()


# Valence -----------------------------------------------------------------
coldplay_val <- coldplay |> 
  group_by(album_name, album_release_year) |> 
  summarize(median = round(median(valence), 2)) |> 
  ungroup() |> 
  mutate(color_font = case_when(median == max(median) ~ "#f2ad51",
                                median == min(median) ~ "#58389e",
                                TRUE ~ "white")) |> 
  arrange(-album_release_year) 

median_val <- coldplay_val |> 
  select(median, color_font) |> 
  mutate(median = paste0(median, "\n")) 

plot1 <- coldplay |> 
  mutate(album_name = paste(album_name, "|", album_release_year),
         album_name = fct_reorder(album_name, -album_release_year)) |> 
  ggplot(aes(x = valence, y = album_name, fill = stat(x))) +
  geom_density_ridges_gradient(rel_min_height = 0.01,
                               color = "white",
                               linetype = 1,
                               lwd = 0.1,
                               show.legend = FALSE) +
  scale_fill_viridis_c(option = "plasma") +
  coord_cartesian(expand = c(0, 0),
                  clip = "off") +
  annotate(geom = "segment", x = .5, xend = .5, y = 1, yend = 11, size = .05, color = "white") +
  annotate(geom = "segment", x = .5, xend = .5, y = 11, yend = 12, size = .05, color = "grey25") +
  annotate(geom = "text", x = 1.4, y = 0.2:8.2, label = median_val$median, color = median_val$color_font, size = 3, family = "Lato Bold") +
  annotate("text", x = .55, y = 12.1, label = "UPBEAT TUNES", color = "white", family = "Lato", size = 3, hjust = 0) +
  annotate("text", x = .45, y = 11.6, label = "SAD SONGS", color = "white", family = "Lato", size = 3, hjust = 1) +
  annotate("text", x = 1, y = 11.7, label = "1", color = "white", family = "Lato", size = 3, hjust = .5) +
  annotate("text", x = 0, y = 11.2, label = "0", color = "white", family = "Lato", size = 3, hjust = .5) +
  annotate(geom = "segment", x = .5, xend = .75, y = 11.7, yend = 11.7, 
           size = .1, color = "white", arrow = arrow(length = unit(.5, "mm"))) +
  annotate(geom = "segment", x = .5, xend = .25, y = 11.2, yend = 11.2, 
             size = .1, color = "white", arrow = arrow(length = unit(.5, "mm"))) +
  labs(title = "Valence",
       subtitle = "Musical positiveness conveyed by a track") +
  theme_minimal() +
  theme(
    plot.margin = margin(5,5,0,5),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_rect(fill = "black", color = "black"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(vjust = -.1,
                               hjust = 1,
                               margin = margin(r = 0),
                               color = "white",
                               size = 10,
                               family = "Lato Bold"),
    plot.title = element_text(family = "Lato Bold",
                              color = "white",
                              hjust = .5,
                              size = 15.5,
                              margin = margin(b = 1)),
    plot.subtitle = element_text(family = "Lato",
                              color = "white",
                              hjust = .5,
                              size = 7.5,
                              margin = margin(b = 5))
  )


# Energy ------------------------------------------------------------------
coldplay_energy <- coldplay |> 
  group_by(album_name, album_release_year) |> 
  summarize(median = round(median(energy), 2)) |> 
  ungroup() |> 
  mutate(color_font = case_when(median == max(median) ~ "#f2ad51",
                                median == min(median) ~ "#58389e",
                                TRUE ~ "white")) |> 
  arrange(-album_release_year) 

median_energy <- coldplay_energy |> 
  select(median, color_font) |> 
  mutate(median = paste0(median, "\n")) 

plot2 <- coldplay |> 
  mutate(album_name = paste(album_name, "|", album_release_year),
         album_name = fct_reorder(album_name, -album_release_year)) |> 
  ggplot(aes(x = energy, y = album_name, fill = stat(x))) +
  geom_density_ridges_gradient(rel_min_height = 0.01,
                               color = "white",
                               linetype = 1,
                               lwd = 0.1,
                               show.legend = FALSE) +
  scale_fill_viridis_c(option = "plasma") +
  coord_cartesian(expand = c(0, 0),
                  clip = "off") +
  annotate(geom = "segment", x = .5, xend = .5, y = 1, yend = 11, size = .05, color = "white") +
  annotate(geom = "segment", x = .5, xend = .5, y = 11, yend = 12, size = .05, color = "grey25") +
  annotate(geom = "text", x = 1.4, y = 0.2:8.2, label = median_energy$median, color = median_energy$color_font, size = 3, family = "Lato Bold") +
  annotate("text", x = .55, y = 12.1, label = "ENERGETIC", color = "white", family = "Lato", size = 3, hjust = 0) +
  annotate("text", x = .45, y = 11.6, label = "SLUGGISH", color = "white", family = "Lato", size = 3, hjust = 1) +
  annotate("text", x = 1, y = 11.7, label = "1", color = "white", family = "Lato", size = 3, hjust = .5) +
  annotate("text", x = 0, y = 11.2, label = "0", color = "white", family = "Lato", size = 3, hjust = .5) +
  annotate(geom = "segment", x = .5, xend = .75, y = 11.7, yend = 11.7, 
           size = .1, color = "white", arrow = arrow(length = unit(.5, "mm"))) +
  annotate(geom = "segment", x = .5, xend = .25, y = 11.2, yend = 11.2, 
           size = .1, color = "white", arrow = arrow(length = unit(.5, "mm"))) +
  labs(title = "Energy",
       subtitle = "Perceptual measure of intensity and activity") +
  theme_minimal() +
  theme(
    plot.margin = margin(5,5,0,5),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_rect(fill = "black", color = "black"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(family = "Lato Bold",
                              color = "white",
                              hjust = .5,
                              size = 15.5,
                              margin = margin(b = 1)),
    plot.subtitle = element_text(family = "Lato",
                              color = "white",
                              hjust = .5,
                              size = 7.5,
                              margin = margin(b = 5))
  )


# Danceability ------------------------------------------------------------
coldplay_dance <- coldplay |> 
  group_by(album_name, album_release_year) |> 
  summarize(median = round(median(danceability), 2)) |> 
  ungroup() |> 
  mutate(color_font = case_when(median == max(median) ~ "#f2ad51",
                                median == min(median) ~ "#58389e",
                                TRUE ~ "white")) |> 
  arrange(-album_release_year) 

median_dance <- coldplay_dance |> 
  select(median, color_font) |> 
  mutate(median = paste0(median, "\n")) 

plot3 <- coldplay |> 
  mutate(album_name = paste(album_name, "|", album_release_year),
         album_name = fct_reorder(album_name, -album_release_year)) |> 
  ggplot(aes(x = danceability, y = album_name, fill = stat(x))) +
  geom_density_ridges_gradient(rel_min_height = 0.01,
                               color = "white",
                               linetype = 1,
                               lwd = 0.1,
                               show.legend = FALSE) +
  scale_fill_viridis_c(option = "plasma") +
  coord_cartesian(expand = c(0, 0),
                  clip = "off") +
  annotate(geom = "segment", x = .5, xend = .5, y = 1, yend = 11, size = .05, color = "white") +
  annotate(geom = "segment", x = .5, xend = .5, y = 11, yend = 12, size = .05, color = "grey25") +
  annotate(geom = "text", x = 1.4, y = 0.2:8.2, label = median_dance$median, color = median_dance$color_font, size = 3, family = "Lato Bold") +
  annotate("text", x = .55, y = 12.1, label = "DANCEY", color = "white", family = "Lato", size = 3, hjust = 0) +
  annotate("text", x = .45, y = 11.6, label = "NOT-SO DANCEY", color = "white", family = "Lato", size = 3, hjust = 1) +
  annotate("text", x = 1, y = 11.7, label = "1", color = "white", family = "Lato", size = 3, hjust = .5) +
  annotate("text", x = 0, y = 11.2, label = "0", color = "white", family = "Lato", size = 3, hjust = .5) +
  annotate(geom = "segment", x = .5, xend = .75, y = 11.7, yend = 11.7, 
           size = .1, color = "white", arrow = arrow(length = unit(.5, "mm"))) +
  annotate(geom = "segment", x = .5, xend = .25, y = 11.3, yend = 11.3, 
           size = .1, color = "white", arrow = arrow(length = unit(.5, "mm"))) +
  labs(title = "Danceability",
       subtitle = "Suitability of a track for dancing") +
  theme_minimal() +
  theme(
    plot.margin = margin(5,5,0,5),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_rect(fill = "black", color = "black"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(family = "Lato Bold",
                              color = "white",
                              hjust = .5,
                              size = 15.5,
                              margin = margin(b = 1)),
    plot.subtitle = element_text(family = "Lato",
                              color = "white",
                              hjust = .5,
                              size = 7.5,
                              margin = margin(b = 5))
  )


# Final plot --------------------------------------------------------------
patchwork <- plot1 | plot2 | plot3 
  
patchwork + plot_annotation(title = 'COLDPLAY',
                  theme = theme(plot.title = element_text(size = 80,
                                                          color = "white",
                                                          family = "Coldplay",
                                                          hjust = .5,
                                                          margin = margin(b = 20)),
                                plot.margin = margin(5, 5, 0, 5),
                  plot.background = element_rect(fill = "black", color = "black"),
                  panel.background = element_rect(fill = "black", color = "black"))) 


ggsave("coldplay.png",  width = 1080, height = 608, units= "px", dpi = 320)



# Ternary plot ------------------------------------------------------------
library(ggtern)

coldplay |> 
slice_max(danceability, n = 1) |> 
  select(track_name)

coldplay_ternary <- coldplay |> 
  mutate(group = factor(case_when(valence == max(valence) ~ 1,
                                  energy == max(energy) ~ 1,
                                  danceability == max(danceability) ~ 1,
                                  TRUE ~ 2))) 

coldplay_ternary |> 
  ggplot(aes(x = valence, y = energy, z = danceability, color = group, alpha = group, fill = "yellow")) +
  coord_tern() +
  geom_point(data = filter(coldplay_ternary, group == 2), size = 1, show.legend = FALSE) +
  geom_point(data = filter(coldplay_ternary, group == 1), size = 1.5, show.legend = FALSE) +
  scale_color_manual(values = c("#f2ad51", "#58389e")) +
  scale_alpha_manual(values = c(1, .5)) +
  #geom_text(aes(label = track_name), size = 1, check_overlap = TRUE) +
  #geom_text(aes(label = energy), size = 1, check_overlap = TRUE) +
  labs(x = "Upbeat\ntunes  ", y = "Energetic", z = "Dancey") +
  theme_noticks() +
  theme_hidegrid() +
  theme_hidelabels() +
  theme(plot.margin = margin(0,-205, 0, -205),
        plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black", color = "red"),
        axis.title = element_text(size = 4,
                                  color = "white",
                                  family = "Lato Bold",
                                  hjust = .5))
  
  
ggsave("coldplay.png",  width = 8.5725, height = 4.826, units= "cm", dpi = 320)