################################################################################
#  hke.27.3a46-8abd_assessment : global data                                   #
#------------------------------------------------------------------------------#
#   Sonia Sanchez-Maroño (AZTI)                                                #
#   created:  25/04/2023                                                       #
#   modified:                                                                  #
################################################################################

# data_00__dtyr_global.R - Load previous assessment files
# ~/*_hke.27.3a46-8abd_assessment/data_00__dtyr_global.R

# Copyright: AZTI, 2026
# Author: Sonia Sanchez-Maroño (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3



#==============================================================================
# LIBRARIES                                                                ----
#==============================================================================

ac <- as.character


#==============================================================================
# DIRECTORIES                                                              ----
#==============================================================================

# Files -----------------------------------------------------------------------


#==============================================================================
# VARIABLES                                                                ----
#==============================================================================

dtyr <- 2025

nfleet <- 9


# Reference points ------------------------------------------------------------

refpts_tab <- read.csv(file.path(icesTAF::taf.data.path(), "RefPts_table.csv"))

Blim      <- as.numeric(refpts_tab[refpts_tab$Reference.point == "B_lim", "Value"])             # 61563
Bpa       <- as.numeric(refpts_tab[refpts_tab$Reference.point == "B_pa", "Value"])              # 78405
Fpa       <- as.numeric(refpts_tab[refpts_tab$Reference.point == "F_pa", "Value"])              # 0.537
Fmsy      <- as.numeric(refpts_tab[refpts_tab$Reference.point == "F_MSY", "Value"])             # 0.243
FmsyLower <- as.numeric(refpts_tab[refpts_tab$Reference.point == "MAP range F_lower", "Value"]) # 0.147
FmsyUpper <- as.numeric(refpts_tab[refpts_tab$Reference.point == "MAP range F_upper", "Value"]) # 0.370
MSYBtrig  <- as.numeric(refpts_tab[refpts_tab$Reference.point == "MSY B_trigger", "Value"])     # 78405
Bmsy      <- as.numeric(refpts_tab[refpts_tab$Reference.point == "B_MSY", "Value"])             # 163929

