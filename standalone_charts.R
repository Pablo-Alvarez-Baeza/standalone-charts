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
  


  


Tdate = c("2020-04-20", "2020-04-22","2020-05-16","2020-05-29", "2020-06-20", "2020-07-02", "2020-07-18", "2020-07-19", "2020-07-22", "2020-09-14", "2020-10-10", "2020-10-15", "2020-11-22", "2020-12-22", "2020-12-24", "2020-12-25")
Tevents = data.frame(station1=c(1,0,0,1,1,1,1,0,0,0,1,1,0,1,0,1),station2=c(1,0,1,1,0,1,1,0,1,1,1,1,0,1,1,1), station3=c(0,1,1,1,1,0,1,1,1,1,1,1,1,1,1,0) )
Zevents<-zoo(Tevents,as.Date(Tdate))

library(tidyr)
library(dplyr)
library(ggplot2)

Tevents$dat <- as.Date(Tdate)


data <- Tevents %>% pivot_longer(cols = contains('station'), names_to = 'station')

ggplot(data) + geom_point(aes(x = dat, y = 2))+
  geom_vline(aes(xintercept  = dat), data = filter(data,value == 1)) +
  coord_cartesian(ylim = c(0,1))+
  facet_wrap(~station, ncol = 1, strip.position = 'left') +
  theme(axis.title.y       = element_blank(),
        axis.text.y        = element_blank(),
        axis.ticks.y       = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())
