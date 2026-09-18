#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-
####
#### GENERATE THE PLOTS AND TABLE TO WRITE THE REPORT IN Rmarkdown
####
#### Dorleta Garcia (AZTI)
#### 2021/07/21
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-

library(icesTAF)
library(flextable)
library(dplyr)


mkdir('report')


# Variables

source("data_00__dtyr_global.R") # Fmsy, dtyr

stockcode <- 'hke.27.3a46-8abd'
run <- 'final'


# Report tables and figuras
source('report_01_Tables.R')
source('report_02_Graphs.R')

# SAG tables
source('report_03_StandardGraphs.R')

# Advice sheet tables
source('report_04_AdviceSheet.R')

# STF comparison
source('report_05_STF_comparison.R')


# WG presentation
rmarkdown::render(paste0("WGBIE2025_",stockcode,"_assessment.Rmd"))

# WG report
rmarkdown::render("WGBIE_2025_09_Hake_north.Rmd")

