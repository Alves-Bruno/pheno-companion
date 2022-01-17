#!/usr/bin/Rscript

options(crayon.enabled = FALSE)
library(tidyverse)
library(arrow)
library(patchwork)

highlight <- c(25, 87, 125, 201, 300)

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
filter(Hour == 05) %>%
filter(Minute <= 20) %>%
      pivot_longer(
        cols = c("Gcc", "L_mean", "A_mean", "B_mean"),
        names_to = "Metric",
        values_to = "value",
      ) %>%
mutate(Hour = sprintf("%.2d:00", Hour)) %>%
mutate(Type = if_else(Metric == "Gcc", "Gcc", "CIELab")) %>%
ggplot(aes(x=DOY, y=value)) +
#  geom_point(aes(colour = Metric), alpha=0.2) +
  geom_line(aes(colour = Metric), alpha=0.7) +
  theme_bw(base_size=16) +
#  facet_wrap(~Hour, ncol = 6)
  ylab("Metrics Values") +  
  theme(
    legend.position = "top", 
  ) + 
  scale_x_continuous(breaks=highlight, labels=highlight) +  
  facet_grid(rows=vars(Type), cols=vars(Hour), scales="free_y") -> plot

df.redLines <- tribble(~X, 25, 87, 125, 201, 300) %>% mutate(Y=1)

plot <- plot +
    geom_vline(data=df.redLines, aes(xintercept=X), alpha=.2, size=0.75, color="black")
##    geom_label(data=df.redLines, aes(x=X, y=-40, label = X), size=2.75)

plot 

ggsave(
  "AHS-metrics-at-4.pdf",
  plot = plot,
  path = "./images/",
#  scale = 1,
  width = 7,
  height = 5)
