#!/usr/bin/Rscript

options(crayon.enabled = FALSE)
library(tidyverse)
library(arrow)
library(patchwork)

# AHS
read_parquet("../data/AHS_metrics-all.parquet") %>%
  as_tibble() %>%
  select(
    Picture.Path,
    Gcc = Gcc_Bruna, 
    L_mean, A_mean, B_mean,
    r_mean, g_mean, b_mean,
    Year, DOY = Day, Hour, Minute, Dataset,
    Camera_ID, Mask.Path
  ) %>%
  filter(between(Year, 2010, 2016))  -> data.AHS

data.AHS %>%
  filter(Year == 2011) -> data.AHS.bigger

data.AHS.bigger %>%
filter(Camera_ID == "f02_u") %>%
filter(Hour %in% c(04,05,12,13,19,20)) %>%
filter(Minute <= 20) %>%
      pivot_longer(
#        cols = c("Gcc", "L_mean", "A_mean", "B_mean"),
        cols = c("Gcc", "L_mean"),
        names_to = "Metric",
        values_to = "value",
      ) %>%
mutate(Hour = sprintf("%.2d:00", Hour)) %>%
mutate(Type = if_else(Metric == "Gcc", "Gcc", "L* mean")) -> to_plot

to_plot %>%
filter(Type == "Gcc") %>%
ggplot(aes(x=DOY, y=value)) +
#  geom_point(aes(colour = Metric), alpha=0.2) +
  geom_line(alpha=0.7) +
  theme_bw(base_size=16) +
#  facet_wrap(~Hour, ncol = 6)
  ylab("Gcc value") +  
  theme(
    legend.position = "top", 
     axis.title.x=element_blank(),
     axis.ticks.x=element_blank(),
     axis.text.x=element_blank(),
  ) +
  facet_grid(Type~Hour) -> p.a

to_plot %>%
filter(Type == "L* mean") %>%
ggplot(aes(x=DOY, y=value)) +
#  geom_point(aes(colour = Metric), alpha=0.2) +
  geom_line(alpha=0.7) +
  theme_bw(base_size=16) +
#  facet_wrap(~Hour, ncol = 6)
  ylab("L* value") +  
  theme(
    legend.position = "top", 
    plot.margin = unit(c(0,0,0,0), "cm"),
    strip.text.x = element_blank()
  ) +
  facet_grid(Type~Hour) -> p.b

p <- p.a / p.b 

ggsave(
  "AHS-metrics-over-day.pdf",
  plot = p,
  path = "./images/",
#  scale = 1,
  width = 12,
  height = 6)
