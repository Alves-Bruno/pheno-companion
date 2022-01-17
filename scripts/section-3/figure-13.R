#!/usr/bin/Rscript

options(crayon.enabled = FALSE)
library(tidyverse)
library(arrow)
library(patchwork)

read_parquet("../data/data_AHS_adj_lstar.parquet") %>%
  as_tibble() -> data.AHS.adj.lstar

cam <- "f02_u"
#cam <- "t24_d"

min <- 0
if(cam == "t24_d"){
  min <- 0
}else{
  min <- 20
}

data.AHS.adj.lstar %>%
  rename(DOY = doy) %>%
  filter(Camera_ID == cam) %>%
  filter(Minute == min) %>%
  mutate(outlier = if_else(type == "to_adjust", TRUE, FALSE)) %>%
  mutate(alpha = if_else(outlier, 0.0, 1.0)) -> map.df

r <- (map.df$r_mean / 256)
g <- (map.df$g_mean / 256)
b <- (map.df$b_mean / 256)
alp <- (map.df$alpha)

map.df -> map.df2

map.df2 %>% filter(outlier == TRUE) -> map.df_outliers
map.df2 %>% filter(outlier == FALSE) -> map.df_non_outliers

r <- (map.df_non_outliers$r_mean / 256)
g <- (map.df_non_outliers$g_mean / 256)
b <- (map.df_non_outliers$b_mean / 256)
alp <- (map.df_non_outliers$alpha)

ggplot() +
  geom_tile(data=map.df_outliers, aes(x=DOY, y=Hour, fill=outlier, color=outlier)) + 
  geom_tile(data=map.df_non_outliers, aes(x=DOY, y=Hour), fill=rgb(r, g, b), color=rgb(r, g, b)) + 
  theme_bw(base_size=16) +
  theme(
    legend.position = "none",
    panel.spacing=unit(0.4, "lines"),
    panel.margin=unit(0.4, "lines"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_grid(run.window~run.level, scale="free") -> p 

map.df %>% 
  group_by(run.window, run.level, outlier) %>% 
  mutate(n_images = n()) %>% 
  select(n_images, outlier) %>% 
  distinct() -> labels

p +
#  ylim(4.6, 24) + 
  geom_label( 
    data= labels %>% filter(outlier == TRUE),
    aes(x=318, y=19, label=n_images),
#    aes(x=46, y=20, label=n_images),
    label.padding = unit(0.08, "lines"),
    color = "red3",
    size = 4.8 
  ) -> p

ggsave(
  "AHS-color-map-with-lstar.png",
  plot = p,
  path = "./images/",
#  scale = 1,
  width = 18,
  height = 8)
