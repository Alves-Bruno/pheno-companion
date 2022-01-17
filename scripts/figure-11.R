#!/usr/bin/Rscript

options(crayon.enabled = FALSE)
library(tidyverse)
library(arrow)
library(patchwork)

# PEG
read_parquet("../data/dataset_PEG_stats_all_years.parquet") %>%
  as_tibble() %>%
  select(
    Gcc = Gcc_Bruna, 
    L_mean, A_mean, B_mean,
    r_mean, g_mean, b_mean,
    Year, DOY = Day, Hour, Minute = Seq, Dataset
  ) %>%
  mutate(Dataset = "PEG", Camera_ID="peg") -> data.PEG

data.PEG %>%
  filter(Year == 2015) -> data.PEG.bigger

bind_rows(
  data.PEG.bigger %>%
  mutate(type="Manual Filter"),

  data.PEG.bigger %>%
  filter(L_mean > 10) %>%
#  filter(between(L_mean, 40, 60)) %>%
  mutate(type="L* Filter\n10 - 100"),

  data.PEG.bigger %>%
#  filter(L_mean > 10) %>%
  filter(between(L_mean, 40, 58)) %>%
  mutate(type="L* Filter\n40 - 60"),

  data.PEG.bigger %>%
  filter(between(Hour, 08, 16)) %>%
  mutate(type="Hour-based\nFilter")

) -> PEG.plot

PEG.plot %>%
  ggplot(aes(x=DOY, y=Gcc)) +
  geom_point(alpha=.2) +
  theme_bw(base_size=16) +
  theme(axis.title.x=element_blank()) +
  facet_grid(
#    Camera_ID~factor(type, levels=c("Original", "L* Filter", "Manual Filter"))
    Camera_ID~factor(type, levels=c("Manual Filter", "L* Filter\n10 - 100", "L* Filter\n40 - 60", "Hour-based\nFilter"))
#    Camera_ID~factor(type, levels=c("Original", "L* Filter"))
  ) -> gcc

PEG.plot %>%
  left_join(
    PEG.plot %>% 
      group_by(Camera_ID, type) %>% 
      count(n()) %>% 
      select(number_of_images = n)
   ) -> PEG.plot

gcc +
#  ylim(0.2, 0.8) + 
  geom_label( 
    data= PEG.plot %>% group_by(Camera_ID, type) %>% slice(1), 
    aes(x=310, y=0.364, label=number_of_images),
    label.padding = unit(0.08, "lines"),
    size = 3.5 
  ) -> gcc

PEG.plot %>%
  rowwise() %>%
  filter(
    Minute %in% c(1, 4) 
  ) %>%
  mutate(Minute = if_else(Minute == 1, 0.00, 0.5)) %>%
  mutate(Hour = Hour + Minute) %>%
  ungroup() -> PEG.plot 

r <- (PEG.plot$r_mean / 256)
g <- (PEG.plot$g_mean / 256)
b <- (PEG.plot$b_mean / 256)

PEG.plot %>%
  ggplot() +
  geom_tile(aes(x=DOY, y=Hour), fill=rgb(r, g, b), color=rgb(r, g, b)) + 
  theme_bw(base_size=16) +
  theme(legend.position = "none") +  
  ylim(5, 20) + 
  facet_grid(
#    Camera_ID~factor(type, levels=c("Original", "L* Filter", "Manual Filter"))
    Camera_ID~factor(type, levels=c("Manual Filter", "L* Filter\n10 - 100", "L* Filter\n40 - 60", "Hour-based\nFilter"))
  ) -> map

p <- gcc / map

ggsave(
  "PEG-filters.png",
  plot = p,
  path = "./images/",
#  scale = 1,
  width = 7,
  height = 6)
