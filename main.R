# Author: A Hogan
# Updated 12 March 2025

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
library(ggpubr)
library(scales)

# functions
source("../covid_endemic_vaccine/R/vaccine_function.R")
source("../covid_endemic_vaccine/R/utils.R")

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

c1 <- "#d31f11"
c2 <- "#f47a00"
c3 <- "#62c8d3"
c4 <- "#007191"
c5 <- "mediumpurple1"
c6 <- "#363432"
c7 <- "#90A19D"
c8 <- "#F0941F"

# Note that the runs themselves generally need to be run on a high-performance computing cluster.
# see 0_set_up_runs/ for the scripts to set up the runs
# see scenarios/ for parameters used as inputs

# the raw output files are too large to store on github - but the processed outputs are saved here, and figures and tables can be generated from the processed outputs

# Plotting files are as follows
# 1A_illustration_curves.R. Produces curves in Figure 1A
# 2A_scenario_curves.R. Produces figures in Figure 2A

# plots_calibration_initial_FigS3 File: calibrationrtb1mu
# plots_calibration_mud1_Fig2.R produces Fig 2, FigS2. File: calibrationmud1
# plots_routine_Fig3. Produces Fig 3, Fig S5. File: routinemud1
# plots_routineescape_fig4. Produces Fig 4, Fig S7, Fig S8. File: routineescapemud1

# File to test out model calibration: calibrationagertb1

# Tables
# analysis/table_routine.R
# analysis/table_routineescape.R




