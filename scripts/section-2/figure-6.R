#!/usr/bin/Rscript

options(crayon.enabled = FALSE)
library(tidyverse)
library(arrow)
library(patchwork)

library(pracma)

library(reticulate)
reticulate::use_miniconda('r-reticulate')
# Steps to install reticulate 
# install.packages('reticulate')
# reticulate::install_miniconda()
# reticulate::conda_install('r-reticulate', 'python-kaleido')
# reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
# reticulate::use_miniconda('r-reticulate')

# Get input data
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

# Compute statistical values 
# Get the first 100 rows of data.AHS as an example
data.AHS %>% 
  select(x=L_mean, y=A_mean, z=B_mean) %>% 
  slice(100:200) -> data

vars <- c("x", "y", "z")
type <- "t"
level <- .5
segments <- 15

# https://en.wikipedia.org/wiki/Hotelling%27s_T-squared_distribution
# m and p
m <- nrow(data)
p <- 3

# Compute the covariance matrix and center
v <- MASS::cov.trob(data[,vars], nu = nrow(data))

# Shape and center
shape <- v$cov
center <- v$center

# Cholesky decompose
chol_decomp <- chol(shape)

# Compute radius
radius <- sqrt(stats::qf(level, p, m - p + 1) * (p*m)/(m-p+1))

radius.small <- sqrt(stats::qf(0.1, p, m - p + 1) * (p*m)/(m-p+1))
radius.medium <- sqrt(stats::qf(0.5, p, m - p + 1) * (p*m)/(m-p+1))
radius.big <- sqrt(stats::qf(0.9, p, m - p + 1) * (p*m)/(m-p+1))

# The go to X method, implements x = L^{-1}.(y-mu)
go_to_X <- function(point, mu, L){
  # I am not fully sure if inv(L) really does implement L^{-1}
  # It seems so "oui, oui, bien sûr, il faut faire inv(L)"
  # Change inv() to solve(), since solve() is in R default lib.
  L_inverse <- solve(L)
  x <- L_inverse %*% (point - mu)
  c(x)
}

# Do the thing
#df %>%
data %>%
  rowwise() %>%
  mutate(X = list(go_to_X(c(x, y, z), center, t(chol_decomp)))) %>%
  mutate(
    N.x = X[[1]],
    N.y = X[[2]],
    N.z = X[[3]]
  ) %>%
  ungroup() %>%
  select(-X) %>%
  print -> data.normal

# CREATE SPHERES 
# Compute a perfect sphere using code from 
# https://stackoverflow.com/questions/35821584/how-to-draw-ellipsoid-with-plotly
theta <- seq(-pi/2, pi/2, by=0.1)
phi <- seq(0, 2*pi, by=0.2)
mgrd <- meshgrid(phi, theta)
phi <- c(mgrd$X)
theta <-  c(mgrd$Y)

lapply(c(radius.small, radius.medium, radius.big), function(radius) {
    x <- cos(theta) * cos(phi) * radius #+ center[[1]]
    dim(x) <- NULL
    y <- cos(theta) * sin(phi) * radius #+ center[[2]] 
    dim(y) <- NULL
    z <- sin(theta) * radius #+ center[[3]]
    dim(z) <- NULL

    tibble (x = x,
            y = y,
            z = z,
            radius = radius)
}) %>%
    bind_rows %>%
    print -> df.sphere

# CREATE ELLIPSES
df.sphere %>%
    rowwise() %>%
    mutate(Z = list(center + c(t(chol_decomp) %*% matrix(c(x, y, z), ncol=1)))) %>%
    mutate(e.x = Z[[1]],
           e.y = Z[[2]],
           e.z = Z[[3]]) %>%
    select(-Z) %>%
    print -> df.ellipses

# PLOT POINTS
library(plotly)
library(htmlwidgets)

plot_ly() %>%
add_markers(data=data.normal,
        x=~x,
        y=~y, 
        z=~z,
        color=~type,
        marker = list(
          color='black',
          size=12, 
          line=list(color='black')            
        ),
        #name="in",
        type="scatter3d",
        size = 0.2) %>%
        layout(
           plot_bgcolor='white',
           paper_bgcolor='white',
           margin = list(autoexpand=FALSE, l=0, b=0, r=0, t=0, pad=0),
           scene = list(
             xaxis = list(
             #title = "l* mean"#,
             title = "L",
             titlefont = list(size = 30),
             gridwidth = 10,
             showticklabels = FALSE
             ),
             yaxis = list(
             title = "A",
             titlefont = list(size = 30),
             #title = "a mean"#,
             gridwidth = 10,
             showticklabels = FALSE
             ),
             zaxis = list(
             title = "B",
             titlefont = list(size = 30),
             #title = "b mean"#,
             gridwidth = 10,
             showticklabels = FALSE
             ), 
             camera = list(
             eye = list(x = -0.5, y = 2.3, z = 0.3),
             center = list(x = 0, y = 0, z = 0)#,
              #projection = list(type = "orthographic")
             )
           )
         ) -> points.plot 
#saveWidget(ellipse.plot, "ellipse.html", selfcontained = FALSE)
save_image(points.plot, scale=1, file = "./images/Y.png", dpi="retina")

# PLOT POINTS IN X
library(plotly)
library(htmlwidgets)

plot_ly() %>%
add_markers(data=data.normal,
        x=~N.x,
        y=~N.y, 
        z=~N.z,
        color=~type,
        marker = list(
          color='black',
          size=12, 
          line=list(color='black')            
        ),
        #name="in",
        type="scatter3d",
        size = 0.2) %>%
        layout(
           plot_bgcolor='white',
           paper_bgcolor='white',
           margin = list(autoexpand=FALSE, l=0, b=0, r=0, t=0, pad=0),
           scene = list(
             xaxis = list(
             #title = "l* mean"#,
             title = "L'",
             titlefont = list(size = 30),
             gridwidth = 10,
             showticklabels = FALSE
             ),
             yaxis = list(
             title = "A'",
             titlefont = list(size = 30),
             #title = "a mean"#,
             gridwidth = 10,
             showticklabels = FALSE
             ),
             zaxis = list(
             title = "B'",
             titlefont = list(size = 30),
             #title = "b mean"#,
             gridwidth = 10,
             showticklabels = FALSE
             ), 
             camera = list(
             eye = list(x = 2.1 , y = 0, z = 0.1),
             center = list(x = 0, y = 0, z = 0)#,
              #projection = list(type = "orthographic")
             )
           )
         ) -> points.X.plot 

save_image(points.X.plot, scale=1, file = "./images/X.png")

# PLOT SPHERE
df.sphere %>%
    select(radius) %>%
    unique %>%
    arrange(radius) %>%
    mutate(Order = 1:n()) -> df.order

df.sphere %>% left_join(df.order, by="radius") -> df.sphere2
df.order %>% pull(Order) -> list.order

lapply(list.order, function(my.order) {

    x      = df.sphere2 %>% filter(Order == my.order) %>% pull(x)
    y      = df.sphere2 %>% filter(Order == my.order) %>% pull(y)
    z      = df.sphere2 %>% filter(Order == my.order) %>% pull(z)

#    plot_ly(scene = paste0("scene", my.order)) %>%
    plot_ly() %>%
        add_trace(
            x = x,
            y = y,
            z = z,
color=~as.factor(my.order), 
            type = "mesh3d",
            opacity = 1, 
            alphahull=0) %>%
        add_markers(data=data.normal,
                    x=~N.x,
                    y=~N.y, 
                    z=~N.z,
                    marker = list(
                      color='red',
                      size=12, 
                      line=list(color='red')            
                    ),
                    type="scatter3d",
                    size = 0.2)  %>%
        layout(
           plot_bgcolor='white',
           paper_bgcolor='white',
           margin = list(autoexpand=FALSE, l=0, b=0, r=0, t=0, pad=0),
           scene = list(
             xaxis = list(
             #title = "l* mean"#,
             title = "L'",
             gridwidth = 10,
             titlefont = list(size = 30),
             showticklabels = FALSE
             ),
             yaxis = list(
             title = "A'",
             titlefont = list(size = 30),
             gridwidth = 10,
             #title = "a mean"#,
             showticklabels = FALSE
             ),
             zaxis = list(
             title = "B'",
             titlefont = list(size = 30),
             gridwidth = 10,
             #title = "b mean"#,
             showticklabels = FALSE
             ), 
             camera = list(
             eye = list(x = 1.9 , y = 0, z = 0.1 ),
             center = list(x = 0, y = 0, z = 0)#,
              #projection = list(type = "orthographic")
             )
           )
         )

}) -> plotly.plots

save_image(plotly.plots[[1]], scale=1, file = "./images/plotA.png")
save_image(plotly.plots[[2]], scale=1, file = "./images/plotB.png")
save_image(plotly.plots[[3]], scale=1, file = "./images/plotC.png")

## GGPLOT - FINAL PLOT 
library(ggimage)
#library(jpeg)

plot_theme <- theme(
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

tibble(
Picture.Path=c("./images/Y.png", "./images/X.png"),
#type = c("up"),
legend = c("CIELab Space (Y)", "Normalized Space (X)")
) %>%
  ggplot() +
  theme_bw(base_size=14) + 
  geom_image(aes(x=0, y=0, image = Picture.Path), size=1.35) + 
  plot_theme + 
  facet_wrap(~factor(legend, levels=c("CIELab Space (Y)", "Normalized Space (X)"))) -> p1

tibble(
Picture.Path=c("./images/plotA.png", "./images/plotB.png", "./images/plotC.png"),
#type = c("up"),
legend = c("Level = 0.1", "Level = 0.5", "Level = 0.9")
) %>%
  ggplot() +
  theme_bw(base_size=14) + 
  geom_image(aes(x=0, y=0, image = Picture.Path), size=1.27) + 
  plot_theme + 
  facet_wrap(~legend) -> p2

layout <- "
##AAAAAA##
BBBBBBBBBB
"

p <- (p1 / p2) + plot_layout(design = layout)
ggsave("./images/space_conversion.pdf", plot=p, width=6, height=4)
