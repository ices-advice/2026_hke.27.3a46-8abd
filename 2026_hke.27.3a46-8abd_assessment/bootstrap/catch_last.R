################################################################################
#  hke.27.3a46-8abd_assessment : latest data                                   #
#------------------------------------------------------------------------------#
#   Sonia Sanchez-Maroño (AZTI)                                                #
#   created:  25/04/2023                                                       #
#   modified:                                                                  #
################################################################################

# catch_indices.R - Copy latest catch information and indices
# ~/*_hke.27.3a46-8abd_assessment/bootstrap/catch_indices.R

# Copyright: AZTI, 2023
# Author: Sonia Sanchez-Maroño (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


#==============================================================================
# DATA                                                                     ----
#==============================================================================

foln   <- "InterCatch"
dat_wd <- file.path("..","..","initial/data/")
inp_wd <- file.path(dat_wd,foln)

dir.create(file.path("..",foln))

for (filen in dir(inp_wd))
    file.copy(file.path(inp_wd, filen), file.path("..",foln,filen), copy.date = TRUE)

# other files

filenams <- c(
  "advice_tac.csv",                   # historical TACs and catch advice
  "RefPts_table.csv",                  # adopted reference points
  "BMSlandings_2015_2025_summary.csv"  # BMS landings
)

for (filen in filenams)
  file.copy(file.path(dat_wd, filen), file.path("..",filen), copy.date = TRUE)



#==============================================================================
# REMOVE                                                                   ----
#==============================================================================

rm(foln, dat_wd, inp_wd, filen)

