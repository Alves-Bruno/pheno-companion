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
 
bind_rows(
  data.AHS.bigger %>%
  mutate(type="Original"),

  data.AHS.bigger %>%
  filter(L_mean > 10) %>%
  mutate(type="L* Filter"),

  data.AHS.bigger %>%
  filter(between(Hour, 08, 15)) %>%
  mutate(type="Hour-based\nFilter")

) -> AHS.plot

AHS.plot %>%
  rowwise() %>%
  filter(
    (Camera_ID == "f02_u" && (Minute %in% c(20)) ) ||
    (Camera_ID == "t24_d" && (Minute %in% c( 0))  )
  ) %>%
  ungroup() -> AHS.plot 

AHS.plot %>%
  ggplot(aes(x=DOY, y=Gcc)) +
  geom_point(alpha=.2) +
  theme_bw(base_size=16) +
  theme(axis.title.x=element_blank()) +
  facet_grid(
    Camera_ID~factor(type, levels=c("Original", "L* Filter", "Hour-based\nFilter"))
  ) -> gcc

AHS.plot %>%
  left_join(
    AHS.plot %>% 
      group_by(Camera_ID, type) %>% 
      count(n()) %>% 
      select(number_of_images = n)
   ) -> AHS.plot

gcc +
  ylim(0.2, 0.8) + 
  geom_label( 
    data= AHS.plot %>% group_by(Camera_ID, type) %>% slice(1), 
    aes(x=320, y=0.75, label=number_of_images),
    label.padding = unit(0.08, "lines"),
    size = 3.5 
  ) -> gcc


r <- (AHS.plot$r_mean / 256)
g <- (AHS.plot$g_mean / 256)
b <- (AHS.plot$b_mean / 256)

AHS.plot %>%
  ggplot() +
  geom_tile(aes(x=DOY, y=Hour), fill=rgb(r, g, b), color=rgb(r, g, b)) + 
  theme_bw(base_size=16) +
  theme(legend.position = "none") +  
  facet_grid(
    Camera_ID~factor(type, levels=c("Original", "L* Filter", "Hour-based\nFilter"))
  ) -> map

p <- gcc / map

ggsave(
  "AHS-filters.png",
  plot = p,
  path = "./images/",
#  scale = 1,
  width = 6,
  height = 6)
