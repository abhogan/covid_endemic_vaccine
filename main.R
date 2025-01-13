# Author: A Hogan
# Updated 1 November 2024

# load packages
library(tidyverse)
library(patchwork)
library(purrr)
library(furrr)
library(ggpubr)
library(lazymcmc)
library(squire)
library(nimue)
library(data.table)
library(parallel)
library(countrycode)
library(safirimmunity)

# plotting things
theme_set(theme_bw(base_size = 14))
th <- theme(strip.background = element_rect(fill = NA),
            panel.border = element_blank(),
            axis.line = element_line(),
            legend.text.align = 0,
            legend.text = element_text(size = 15),
            axis.text=element_text(size=16),
            axis.title=element_text(size=16),
            strip.text.x = element_text(size = 16),
            strip.text.y = element_text(size = 16))
