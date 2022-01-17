#!/usr/bin/Rscript
# Load libs
options(crayon.enabled = FALSE)
library(tidyverse)
library(arrow)
library(patchwork)

p90_aggregation_simple <- function(df, window_size=5){

  df %>%
    arrange(Year, DOY) -> metrics.ordered

  df %>%
    select(Year, DOY) %>%
    distinct %>%
    arrange(Year, DOY) %>%
    slice(1:(tail(seq(1,n(), by=window_size), n=1)-1)) %>%
    mutate(order = rep(1:(n()/window_size), each=window_size)) -> df.center.order

  df %>%
    # Associate groups and filter out those pictures without a group
    left_join(df.center.order, by = c("Year", "DOY")) %>%
    filter(!is.na(order)) %>%
    group_by(order) %>%
    summarize(
        Year = min(Year),
        DOY = median(DOY), # this effectively gets the DOY in the center
        p90 = quantile(Gcc, probs=c(.90)), .groups="drop") %>%
    return()

}

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

bind_rows(
  data.AHS %>%
    mutate(Dataset = paste(Dataset, Camera_ID, sep="\n")) %>%
    group_by(Dataset) %>%
    group_modify(~p90_aggregation_simple(.x)) %>% 
    mutate(gcc_type = "Gcc P90"),

  data.AHS %>%
    mutate(Dataset = paste(Dataset, Camera_ID, sep="\n")) %>%
    mutate(gcc_type = "Gcc")
) %>%
  ggplot() +
  geom_point(aes(x=DOY, y=Gcc, colour=gcc_type), alpha=0.05) +
  geom_point(aes(x=DOY, y=p90, colour=gcc_type)) +
  geom_line(aes(x=DOY, y=p90, colour=gcc_type)) +
  theme_bw(base_size=16) +
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    legend.spacing = unit(1, "mm"),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    legend.box.spacing = unit(0, "pt"),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.title = element_blank()
  ) +
#  ylab("Gcc") +
  scale_color_manual(
    breaks = c("Gcc", "Gcc P90"), 
    values=c("black", "red")
  ) + 
  labs(
    y = "Gcc value",
    colour= "Gcc type"
  ) +
  facet_grid(Dataset~Year) -> p.ahs

ggsave(
  "AHS-gcc-p90-together.jpg",
  plot = p.ahs,
  path = "./images/",
#  scale = 1,
  width = 15,
  height = 4)
