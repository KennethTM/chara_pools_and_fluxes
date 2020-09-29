#Libraries and paths
Sys.setenv(TZ="GMT");Sys.setlocale("LC_TIME", "English")

library(tidyverse);library(lubridate);library(readxl);library(grid);library(gridExtra)
library(emmeans);library(betareg)

#Paths
data_path <- paste0(getwd(), "/data/")
fig_path <- paste0(getwd(), "/figures/")

#Load data
fig_3_a_b_data <- read.delim2(paste0(data_path, "fig_3_a_b.txt"))
fig_3_c_data <- read.delim2(paste0(data_path, "fig_3_c.txt"))
fig_4_data <- read.delim2(paste0(data_path, "fig_4.txt"))
fig_5_data <- read.delim2(paste0(data_path, "fig_5.txt"))
fig_s1_data <- read.delim2(paste0(data_path, "fig_s1.txt"))

p_crust_algae <- tribble(~p_algae, ~p_crust,
                         2.31,	0.3848,
                         1.961,	0.652,
                         2.208,	0.236,
                         1.827,	0.1164,
                         2.49,	0.0484,
                         2.32,	0.972,
                         1.4,	0.46,
                         1.4,	0.484,
                         1.4,	0.096,
                         1.234,	0.108,
                         1.515,	0.164,
                         1.507,	0.541,
                         1.02,	0.337,
                         0.92,	0,
                         0.92,	0.53,
                         0.971,	0.34,
                         0.319,	0.026,
                         0.58,	0,
                         0.62,	0.095,
                         0.594,	0.06)

#ggplot theme
theme_pub <- theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(colour = "black"), 
        strip.background = element_rect(fill = "white"))
theme_set(theme_pub)
