#!/usr/bin/Rscript

options(crayon.enabled = FALSE)
library(tidyverse)
library(arrow)
library(patchwork)

# GDK
read_parquet("../data/GDK_metrics-all.parquet") %>%
  as_tibble() %>%
  select(
    Gcc = Gcc_Bruna, 
    L_mean, A_mean, B_mean,
    r_mean, g_mean, b_mean,
    Year, DOY = Day, Hour, Minute, Dataset,
    Camera_ID, Mask.Path
  ) -> data.GDK

data.GDK %>%
  filter(Year == 2009) -> data.GDK.bigger

bind_rows(

  data.GDK.bigger %>%
  mutate(type="Original"),

  data.GDK.bigger %>%
  filter(L_mean > 10) %>%
  mutate(type="L* Filter"),

  data.GDK.bigger %>%
  filter(between(Hour, 08, 16)) %>%
  mutate(type="Hour-based\nFilter")

) -> GDK.plot

GDK.plot %>%
  ggplot(aes(x=DOY, y=Gcc)) +
  geom_point(alpha=.2) +
  theme_bw(base_size=16) +
  theme(axis.title.x=element_blank()) +
  facet_grid(
    Camera_ID~factor(type, levels=c("Original", "L* Filter", "Hour-based\nFilter"))
  ) -> gcc

GDK.plot %>%
  left_join(
    GDK.plot %>% 
      group_by(Camera_ID, type) %>% 
      count(n()) %>% 
      select(number_of_images = n)
   ) -> GDK.plot

gcc +
#  ylim(0.2, 0.8) + 
  geom_label( 
    data= GDK.plot %>% group_by(Camera_ID, type) %>% slice(1), 
    aes(x=320, y=0.68, label=number_of_images),
    label.padding = unit(0.08, "lines"),
    size = 3.5 
  ) -> gcc

GDK.plot %>%
  rowwise() %>%
  filter(
    Minute %in% c(0)
  ) %>%
  ungroup() -> GDK.plot 

r <- (GDK.plot$r_mean / 256)
g <- (GDK.plot$g_mean / 256)
b <- (GDK.plot$b_mean / 256)

GDK.plot %>%
  ggplot() +
  geom_tile(aes(x=DOY, y=Hour), fill=rgb(r, g, b), color=rgb(r, g, b)) + 
  theme_bw(base_size=16) +
  theme(legend.position = "none") +  
  facet_grid(
    Camera_ID~factor(type, levels=c("Original", "L* Filter", "Hour-based\nFilter"))
  ) -> map

p <- gcc / map

ggsave(
  "GDK-filters.png",
  plot = p,
  path = "./images/",
#  scale = 1,
  width = 6,
  height = 7.5)
