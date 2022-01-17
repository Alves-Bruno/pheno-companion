#!/usr/bin/Rscript

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

p90_aggregation_complex <- function(df, window_size=5){

  add_order <- function(df){
    df %>%
      select(year, doy) %>%
      distinct -> df.dist

      if(nrow(df.dist) < window_size){
         #print("Agreggation error.")
         return(df %>% mutate(order = -1))
      }
      
      df.dist %>%
      slice(1:(tail(seq(1,n(), by=window_size), n=1)-1)) %>%
      mutate(order = rep(1:(n()/window_size), each=window_size)) %>%
      return()
  }

  df %>%
    select(year, doy, Camera_ID, run.level, run.window) %>%
    #distinct %>%
    arrange(year, doy) %>%
    group_by(Camera_ID, run.level, run.window) %>%
    group_modify(~ add_order(.x)) -> df.center.order

  if(nrow(df.center.order %>% filter(order == -1)) > 0){
    print("Aggregation error - window_size is greater than grouped nrows().")
    return(df)
  }

  #return(df.center.order)
  #print(df.center.order %>% as.data.frame)

  df %>%
    # Associate groups and filter out those pictures without a group
    left_join(df.center.order, by = c("year", "doy", "Camera_ID", "run.level", "run.window")) %>%
    filter(!is.na(order)) %>%
    group_by(order, run.level, run.window, Camera_ID) %>%
    summarize(
        .groups = "keep",
        year = min(year),
        doy = median(doy), # this effectively gets the doy in the center
        p90.gcc = quantile(Gcc_Bruna, probs=c(.90)),
        p90.merged.gcc = quantile(merged.Gcc, probs=c(.90)),
        p90.new.gcc = quantile(new.Gcc, probs=c(.90), na.rm = TRUE)) %>%
    ungroup() %>%
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

data.AHS %>%
  filter(Year == 2011) -> data.AHS.bigger

data.GDK %>%
  filter(Year == 2009) -> data.GDK.bigger

data.PEG %>%
  filter(Year == 2015) -> data.PEG.bigger

data.AHS.bigger %>%
  filter(between(Hour, 8, 15)) %>%
  group_by(Camera_ID) %>%
  group_modify(~p90_aggregation_simple(.x)) %>%
  mutate(dataset = "AHS") %>%
  mutate(Type = "Hour-based filter") -> ahs

data.GDK.bigger %>%
  filter(between(Hour, 8, 15)) %>%
  group_by(Camera_ID) %>%
  group_modify(~p90_aggregation_simple(.x)) %>%
  mutate(dataset = "GDK") %>%
  mutate(Type = "Hour-based filter") -> gdk

data.PEG.bigger %>%
  filter(between(Hour, 8, 15)) %>%
  group_by(Camera_ID) %>%
  group_modify(~p90_aggregation_simple(.x)) %>%
  mutate(dataset = "PEG") %>%
  mutate(Type = "Hour-based filter") -> peg

read_parquet("../data/data_AHS_adj_lstar.parquet") %>%
  as_tibble() -> data.AHS.adj.lstar

read_parquet("../data/data_GDK_adj_lstar.parquet") %>%
  as_tibble() -> data.GDK.adj.lstar

read_parquet("../data/data_PEG_adj_lstar.parquet") %>%
  as_tibble() -> data.PEG.adj.lstar

data.AHS.adj.lstar %>%
  p90_aggregation_complex() -> data.AHS.bigger.p90.lstar

data.PEG.adj.lstar %>%
  p90_aggregation_complex() -> data.PEG.bigger.p90.lstar

data.GDK.adj.lstar %>%
  p90_aggregation_complex() -> data.GDK.bigger.p90.lstar

bind_rows(

ahs %>% 
  mutate(run.level = 0.1), 
ahs %>% 
  mutate(run.level = 0.5), 
ahs %>% 
  mutate(run.level = 0.9), 

gdk %>% 
  mutate(run.level = 0.1), 
gdk %>% 
  mutate(run.level = 0.5), 
gdk %>% 
  mutate(run.level = 0.9), 


peg %>% 
  mutate(run.level = 0.1), 
peg %>% 
  mutate(run.level = 0.5), 
peg %>% 
  mutate(run.level = 0.9), 


data.AHS.bigger.p90.lstar %>%
  select(doy, p90.merged.gcc, run.level, run.window, Camera_ID) %>%
  mutate(Code = paste(sprintf("%.2d", run.window), run.level, sep=" -- ")) %>%
  rename(p90 = p90.merged.gcc, DOY = doy) %>%
  filter(
    (run.window == 365 & run.level == 0.1) |
    (run.window == 3 & run.level == 0.1) |

    (run.window == 365 & run.level == 0.5) |
    (run.window == 3 & run.level == 0.5) |  

    (run.window == 365 & run.level == 0.9) |
    (run.window == 3 & run.level == 0.9) 
  ) %>%
  mutate(dataset = "AHS") %>%
  mutate(Type = if_else(run.window==3, "Local adjust", "Global adjust")) , #%>%

data.GDK.bigger.p90.lstar %>%
  select(doy, p90.merged.gcc, run.level, run.window, Camera_ID) %>%
  mutate(Code = paste(sprintf("%.2d", run.window), run.level, sep=" -- ")) %>%
  rename(p90 = p90.merged.gcc, DOY = doy) %>%
  filter(
    (run.window == 365 & run.level == 0.1) |
    (run.window == 3 & run.level == 0.1) |

    (run.window == 365 & run.level == 0.5) |
    (run.window == 3 & run.level == 0.5) |  

    (run.window == 365 & run.level == 0.9) |
    (run.window == 3 & run.level == 0.9) 
  ) %>%
  mutate(dataset = "GDK") %>%
  mutate(Type = if_else(run.window==3, "Local adjust", "Global adjust")) , #%>%

data.PEG.bigger.p90.lstar %>%
  select(doy, p90.merged.gcc, run.level, run.window, Camera_ID) %>%
  mutate(Code = paste(sprintf("%.2d", run.window), run.level, sep=" -- ")) %>%
  rename(p90 = p90.merged.gcc, DOY = doy) %>%
  filter(
    (run.window == 365 & run.level == 0.1) |
    (run.window == 3 & run.level == 0.1) |

    (run.window == 365 & run.level == 0.5) |
    (run.window == 3 & run.level == 0.5) |  

    (run.window == 365 & run.level == 0.9) |
    (run.window == 3 & run.level == 0.9) 
  ) %>%
  mutate(dataset = "PEG") %>%
  mutate(Type = if_else(run.window==3, "Local adjust", "Global adjust")) , #%>%


#  mutate(Type = if_else(run.window==3, paste0("Local (", run.level, ")"), paste0("Global (", run.level, ")"))) %>%
#  mutate(Type = paste("L/Adj", Code, sep=" - ")) %>%
#  select(-Code, -contains("run")),

) %>%
  mutate(dataset_cam = paste(dataset, Camera_ID, sep="\n")) %>%
    ggplot() +
#    ggplot(aes(x=DOY, y = p90, color=as.factor(Type))) +
    geom_line(aes(x=DOY, y = p90, color=as.factor(Type), linetype=as.factor(Type))) +
#    geom_line(aes(x=DOY, y = baseline), color="black") +
#    geom_line(alpha=0.7) +
#    geom_point(alpha=0.3) +
    theme_bw(base_size=16) +
    theme(legend.position = "top") +
    labs(color='Type:')  +
    xlab("DOY") + 
    ylab("Gcc P90") + 
#    theme(
#      legend.position = c(0.08, 0.85),
#      legend.key.size = unit(0.40, 'cm'),
#      legend.title = element_text(size=14), 
#      legend.text = element_text(size=13)
#    ) +
#    scale_color_brewer(palette="Set1", labels = c("Global (0.8)", "Local (0.8)", "Hour-based filter")) +
     scale_colour_manual(
       labels = c("Global adjust", "Local adjust", "Hour-based filter"),
       values = c("#E41A1C", "#377EB8", "black")
     ) + 
    scale_linetype_manual(
       labels = c("Global adjust", "Local adjust", "Hour-based filter"),
       values = c("solid", "solid", "dashed"), 
     ) +
#    scale_color_brewer(palette="Set1") +
    guides(linetype = FALSE) + 
    facet_grid(dataset_cam~run.level, scales="free_y") -> p
#    facet_wrap(~dataset_cam, ncol=3) -> p

p 

ggsave(
  "impacts-on-p90.png",
  plot = p,
  path = "./images/",
#  scale = 1,
  width = 15,
  height = 10)
