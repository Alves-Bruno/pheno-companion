#!/usr/bin/Rscript

options(crayon.enabled = FALSE)
library(tidyverse)
library(arrow)
library(patchwork)

#cam <- "f02_u"
cam <- "t24_d"

min <- 0
if(cam == "t24_d"){
  min <- 0
}else{
  min <- 20
}

read_parquet("../data/data_AHS_adj_lstar.parquet") %>%
  as_tibble() -> data.AHS.adj.lstar

data.AHS.adj.lstar %>%
  rename(DOY = doy) %>%
  filter(Camera_ID == cam) %>%
  filter(Minute == min) %>%
  filter(run.level == 0.1, run.window == 3) %>%
  mutate(run.level = "Color reference") %>%
  filter(DOY %in% seq(0,400, 5)) -> map.df

r <- (map.df$r_mean / 256)
g <- (map.df$g_mean / 256)
b <- (map.df$b_mean / 256)
alp <- (map.df$alpha)

ggplot() +
  geom_tile(data=map.df, aes(x=DOY, y=Hour), fill=rgb(r, g, b)) + 
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
  facet_grid(~run.level, scale="free") -> original_colors

data.AHS.adj.lstar %>%
#data.AHS.adj %>%
#  filter(Hour <= 8) %>%
  filter(run.level %in% c(0.1, 0.5, 0.9)) %>%
  filter(run.window %in% c(3, 365)) %>%
  rename(DOY = doy) %>%
  filter(Camera_ID == cam) %>%
  filter(Minute == min) %>%
  mutate(outlier = if_else(type == "to_adjust", TRUE, FALSE)) %>%
  mutate(alpha = if_else(outlier, 0.0, 1.0)) -> map.df

bind_rows(
  map.df %>%
    filter(run.level == 0.1) %>%
    mutate(run.level = "Original colors"),

  map.df %>% 
    mutate(r_mean = if_else(outlier, R, r_mean)) %>%
    mutate(g_mean = if_else(outlier, G, g_mean)) %>%
    mutate(b_mean = if_else(outlier, B, b_mean)) %>%
    mutate(run.level = as.character(run.level))

) -> map.df

map.df %>%
    filter(run.level == "Original colors") %>%
    mutate(L = x, a = y, b = z) %>%
    select(run.window, run.level, Camera_ID, DOY, Hour, Minute, L, a, b) -> df.ref

map.df %>%
    filter(run.level != "Original colors") %>%
    mutate(L = ifelse(is.na(x.border), x, x.border),
           a = ifelse(is.na(y.border), y, y.border),
           b = ifelse(is.na(z.border), z, z.border)) %>%
    select(run.window, run.level, Camera_ID, DOY, Hour, Minute, L, a, b) -> df.temp0

df.temp0 %>%
    filter(run.level != "Original colors") %>%
    left_join(
        df.ref %>% select(-run.level, -run.window),
        by=c("Camera_ID", "DOY", "Hour", "Minute"),
        suffix = c(".level", ".ref")) %>%
    mutate(dE = sqrt((L.level - L.ref)^2 + (a.level - a.ref)^2 + (b.level - b.ref)^2)) %>%
    left_join(
        map.df %>%
        filter(run.level != "Original colors") %>%    
        select(run.level, run.window, Camera_ID, DOY, Hour, Minute, r_mean, g_mean, b_mean),
        by = c("run.window", "run.level", "Camera_ID", "DOY", "Hour", "Minute")
    ) %>%
    group_by(run.level, run.window, Camera_ID, DOY, Hour, Minute) -> df.temp1

df.temp1 %>%
    group_by(run.level, run.window, Camera_ID, DOY, Hour) %>%
    arrange(Minute) %>%
    slice(1) %>%
    ungroup %>%
#    filter(DOY %in% seq(1,400, 3)) %>%
    mutate(dE.alpha = dE/max(dE)) %>%
##    mutate(dE.alpha = if_else(dE.alpha <= 0.24, 0.00, dE.alpha)) %>%
    print -> df.temp2

df.temp2 %>% 
  filter(DOY %in% seq(0,400, 5)) -> df.temp3

#%>%
#  mutate(Hour = 1) -> df.temp3

r <- (df.temp3$r_mean / 256) 
g <- (df.temp3$g_mean / 256)
b <- (df.temp3$b_mean / 256)

#white <- 0
#r <- r + white
#g <- g + white
#b <- b + white

ggplot() +
#  geom_tile(data=df.temp3, aes(x=DOY, y=Hour), fill=rgb(r, g, b)) +
#  geom_tile(data=df.temp3, aes(x=DOY, y=Hour, alpha=dE.alpha), fill=rgb(r, g, b)) +
  geom_tile(data=df.temp3, aes(x=DOY, y=Hour, fill=dE)) +
  theme_bw(base_size=16) +
  theme(
    panel.spacing=unit(0.4, "lines"),
    panel.margin=unit(0.4, "lines"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_grid(run.window~run.level, scale="free") + #-> p#+
  labs(fill="Distance") +
  theme(
    plot.margin=unit(c(1,0.5,0.5,0.5),"cm"),
    legend.position = c(0.5, 1.25),
    legend.direction="horizontal",
#    legend.key.size = unit(0.5, 'cm'), #change legend key size
    legend.key.height = unit(0.2, 'cm'), #change legend key height
    legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=10), #change legend title font size
    legend.text = element_text(size=8)
  ) + 
  scale_fill_gradient(low = "white", high = "red", na.value = NA) -> p  

layout <- "
##BBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
AABBBB
##BBBB
"
p <- original_colors + p + plot_layout(design = layout)

ggsave(
  "color-correction-distance.png",
  plot = p,
  path = "./images/",
#  scale = 1,
  width = 8,
  height = 4.5)
