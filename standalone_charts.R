library(pacman)
p_load(tidyverse, janitor, scales, ggpattern, titanic)

titanic <- titanic_train |> 
  clean_names()

titanic |> 
  glimpse()

titanic_1 <- titanic |> 
  mutate(pclass_fct = factor(pclass),
         survived_fct = factor(survived)) |> 
  group_by(pclass_fct) |> 
  count(survived_fct) |> 
  mutate(total = sum(n),
         perc = round(n / total, 2) * 100,
         position = rank(survived_fct))

titanic_1 |> 
  ggplot(aes(pclass_fct, perc, fill = survived_fct)) +
  geom_col_pattern(
    aes(fill = interaction(pclass_fct, survived_fct),
        pattern = interaction(pclass_fct, survived_fct)),
    position = position_dodge(),
    pattern_colour = "white",
    pattern_fill = "white",
    pattern_density = .3,
    pattern_spacing = .01,
    pattern_size = .1,
    pattern_linetype = 2
  ) +
  scale_fill_manual(values = rep(c("#6141d9", "#ef6c3a", "#4fae54"), times = 2)) +  
  scale_pattern_manual(values = rep(c("stripe","none"), each = 3)) +
  scale_x_discrete(expand = c(0, 0),
                   labels = c("1st", "2nd", "3rd")) +
# Alternative with integers is to use labels = scales::label_ordinal()
  scale_y_continuous(
    limits = c(0, 80),
    expand = c(0, 0),
    labels = label_number(suffix = "%")
  ) +
  labs(
    x = "Ticket class",
    y = NULL,
    title = "",
    subtitle = ""
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    axis.line.x = element_line(colour = 'black', size =.5),
    legend.position = "none"
  )
  
  
