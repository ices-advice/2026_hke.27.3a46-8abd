################################################################################
#  hke.27.3a46-8abd_assessment : model code                                    #
#------------------------------------------------------------------------------#
#   Sonia Sanchez-Maroño (AZTI)                                                #
#   created:  25/04/2023                                                       #
#   modified:                                                                  #
################################################################################

# model.R - model-related code
# ~/*_hke.27.3a46-8abd_assessment/model.R

# Copyright: AZTI, 2023
# Author: Sonia Sanchez-Maroño (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


library(icesTAF)

mkdir("model")

source("data_00__dtyr_global.R")


#==============================================================================
# ASSESSMENT FILES                                                         ----
#==============================================================================

# dtyr       <- 2024
ass.yr <- as.numeric(substr(dtyr,3,4)) + 1

ctlp_file <- paste0("nhake-wg",ass.yr-1,".ctl")
datp_file <- paste0("nhake-wg",ass.yr-1,".dat")

ctrl_file    <- paste0("nhake-wg",ass.yr,".ctl")
data_file    <- paste0("nhake-wg",ass.yr,".dat")


#==============================================================================
# ASSESSMENT                                                               ----
#==============================================================================


## - MODEL RUNS

if (cluster == TRUE) {
  
  
  #==============================================================================
  # CLUSTER                                                                  ----
  #==============================================================================
  
  ## Copy files for CLUSTER runs
  
  source('model_00a_cluster_in.R')
  
  
  ## Cluster runs (see ./cluster/cluster.txt)
  
  
  ## Copy files from CLUSTER run outputs
  
  finalrun <- "saly" # saly : same as last year with lower loglik (finally taken first run, as in complete agreement)
  finalnam <- paste0("wg",ass.yr)
  
  source('model_00b_cluster_out.R')
  
} else {
  
  ## Model runs (without cluster)
  source('model_01_Runs.R')
  
}

## - RETROS

## Retrospective patterns
source("model_02_Retros.R")


#==============================================================================
# SHORT-TERM FORECAST                                                      ----
#==============================================================================

# load packages
library(r4ss)
library(dplyr)
library(parallel)
library(doParallel)
library(icesAdvice)


## Select run -----------------------------------------------------------------

run <- 'model/final' # model folder

## RP --------------------------------------------------------------------------

source("data_00__dtyr_global.R") # reference points, nfleet, dtyr


## Fix intermediate year --------------------------------------------------------

year_inter <- dtyr + 1

tacadv <- read.csv( file.path(taf.boot.path("data"),"advice_tac.csv"))

TAC         <- tacadv[tacadv$year == year_inter, "tac"]    # intermediate year TAC
TACadvice   <- tacadv[tacadv$year == year_inter, "advice"] # intermediate year advice


# directories
mod_path   <- file.path(getwd(), run)
stf_path   <- file.path("model","stf")
stfGM_path <- file.path("model","stfGM")
stfGMlowlast_path <- file.path("model","stfGMlowlast")


## STF runs
source('model_03a_Forecast_settings.R')
source('model_03b_Forecast_runs.R') # to run in cluster all values see ss_10_runs_stf.R
source('model_03c_Forecast_summary.R')
source('model_03d_Forecast_interpolation.R')

## STF runs with geometric mean (historical ) recruitment
source('model_04a_ForecastGM_settings.R')
source('model_04b_ForecastGM_runs.R')
source('model_04c_ForecastGM_summary.R')
source('model_04d_ForecastGM_interpolation.R')

## STF runs with geometric mean of latest low recruitment (since 2020)
source('model_05a_ForecastGMlowlast_settings.R')
source('model_05b_ForecastGMlowlast_runs.R')
source('model_05c_ForecastGMlowlast_summary.R')
source('model_05d_ForecastGMlowlast_interpolation.R')


## Selected criteria for STF recruitment
# Options:
# - ""          for using recrutiment estimated by Stock Synthesis
# - "GM"        for using geometric mean of historical recruitments (1990-dtyr)
# - "GMlowlast" for using geometric mean of latest low recruitments (2020-dtyr)
stfrec_sel <- "GMlowlast" 
saveRDS(stfrec_sel, file = file.path("model","stfrec_sel.RDS"))
# Note: the specific recruitment values are replaced in the summary table, if necessary (i.e. for options different to "")


