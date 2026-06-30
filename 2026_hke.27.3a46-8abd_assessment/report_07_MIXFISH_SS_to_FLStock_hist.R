################################################################################
#  WGBIE northern hake - Stock Synthesis output to FLR objects (historical)    #
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  02/06/2023                                                       #
#   modified:                                                                  #
################################################################################

# report_07_MIXFISH_SS_to_FLStock_hist.R - transforming Stock Synthesis output (without STF years) to FLR objects
# ~/report_07_MIXFISH_SS_to_FLStock_hist.R

# Copyright: AZTI, 2023
# Author: Sonia Sanchez (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


rm(list=ls())

source("data_00__dtyr_global.R") # dtyr
yy   <- substr(dtyr+1,3,4)


#==============================================================================
# LIBRARIES                                                                ----
#==============================================================================

# remotes::install_github("r4ss/r4ss")
# remotes::install_github("flr/ss3om")

# Load libraries ----------------------------------------------------------

library(r4ss)
library(ss3om)      # for readLFSss3
library(ggplot2)
library(dplyr)

packageVersion("ss3om") # ‘0.5.3.9004’


#==============================================================================
# FILE FOLDERS                                                             ----
#==============================================================================

ass.wd <- "model/final" # data directory
# ass.wd <- "model/final/Forecast/Fmult1"

ref.wd <- "WGMIXFISH"                 # reference directory
out.wd <- file.path(ref.wd, "output") # output directory
fun.wd <- file.path(ref.wd, "fun")
fig.wd <- file.path(ref.wd, "plots")

if (!dir.exists(out.wd)) dir.create(out.wd, recursive = TRUE)
if (!dir.exists(fig.wd)) dir.create(fig.wd, recursive = TRUE)

data.file   <- file.path("model", "final", paste0("nhake-wg",yy,".dat"))
sumTab.file <- file.path("output", "summary_table_final.RData")

outfile_all <- file.path(out.wd, paste0("wgbie",dtyr+1,"_nhke_FLStock_seasonal.RData")) # without any correction

outfile_est <- file.path(out.wd, paste0("wgbie",dtyr+1,"_nhke_FLStock_csim_ini.RData")) # without weights corrected
outfile_ecw <- file.path(out.wd, paste0("wgbie",dtyr+1,"_nhke_FLStock_csim.RData"))     # with weights corrected
outfile_obs <- file.path(out.wd, paste0("wgbie",dtyr+1,"_nhke_FLStock_cobs_ini.RData")) # without weights corrected
outfile_ocw <- file.path(out.wd, paste0("wgbie",dtyr+1,"_nhke_FLStock_cobs.RData"))     # with weights corrected

cat.table <- file.path(out.wd, "catch_check.csv")

f17.plot    <- file.path(fig.wd, "F17_check.png")
ssb.plot    <- file.path(fig.wd, "SSB_check.png")
rec.plot    <- file.path(fig.wd, "rec_check.png")

flstkest.plot    <- file.path(fig.wd, "FLStock_csim_check.png")
flstkestcwt.plot <- file.path(fig.wd, "FLStock_csim_cwt_check.png")
flstkobs.plot    <- file.path(fig.wd, "FLStock_cobs_check.png")
flstkobscwt.plot <- file.path(fig.wd, "FLStock_cobs_cwt_check.png")


#==============================================================================
# DATA                                                                     ----
#==============================================================================

# SS3 info

ss3Dat  <- SS_readdat_3.30(data.file)                                       # data
replist <- SSgetoutput(dirvec = ass.wd, getcovar = F, verbose = FALSE)[[1]] # output
# replist2 <- SSgetoutput(dirvec=ass.wd)$replist1
# # replist_sum <- SSsummarize(list(replist1 = replist))  # output summary
# # replist_sum2 <- SSsummarize(SSgetoutput(dirvec=ass.wd))_sum
# 
# names(replist)[!names(replist) %in% names(replist2)]
# names(replist2)[!names(replist2) %in% names(replist)]


# Catch info

fltnms <- setNames(replist$definitions$Fleet_name, 1:17)

catch <- as_tibble(replist$timeseries) %>% filter(Era == 'TIME') %>% 
  select("Yr", "Seas", starts_with("obs_cat"), starts_with("retain(B)"), starts_with("dead(B)")) 
names(catch) <- c('year', 'season', paste('LanObs', fltnms[1:9], sep = "_"), paste('LanEst', fltnms[1:9], sep = "_"),
                  paste('CatEst', fltnms[1:9], sep = "_"))

aux1 <- catch %>% select(starts_with('CatEst')) - catch %>% select(starts_with('LanEst'))
names(aux1) <- paste('DisEst', fltnms[1:9], sep = "_")

catch <- catch %>% bind_cols(aux1) 
catch <- catch %>% tidyr::pivot_longer(cols = names(catch)[-(1:2)], names_to = 'id', values_to = 'value') %>% 
  mutate(indicator = substr(id,1,6), fleet = substr(id, 8, nchar(id))) %>% 
  select('year', 'season', 'fleet', 'indicator', 'value')  

discdat <- as_tibble(ss3Dat$discard_data) 
discdat <- discdat %>% mutate(Flt = fleet, fleet = fltnms[Flt], 
                              season = recode(month, `2.5` = 1, `5.5` = 2, `8.5` = 3, `11.5` = 4), 
                              indicator = 'DisObs') %>% 
  select('year', 'season', 'fleet', 'indicator', 'obs') %>% 
  rename(value = obs)

catch <- catch %>% bind_rows(discdat) %>%
  mutate(category = ifelse(substr(indicator,1,3) == 'Lan', 'Landings', ifelse(substr(indicator,1,3) == 'Dis', 'Discards', 'Catch')),
         type = ifelse(substr(indicator,4,6) == 'Est', 'Estimated', 'Observed'),
         time = year + 1.125 - 1/season)  %>% 
  select('year', 'season', 'time', 'fleet', 'category', 'type', 'indicator', 'value')


catch.sum <- catch %>% filter(year > 0) %>% 
  group_by(year, indicator) %>% summarise(value = sum(value)) %>% 
  tidyr::pivot_wider(names_from = "indicator", values_from = "value") %>% 
  mutate(CatObs = colSums(rbind(LanObs,DisObs), na.rm = TRUE), 
         CatObs = ifelse(is.na(LanObs) & is.na(DisObs), NA, CatObs))


#==============================================================================
# FLStock: with estimated catch                                            ----
#==============================================================================

# Seasonal  -------------------------------------------------------------------

hke.stk <- readFLSss3(ass.wd)  # FLStock 
# hke.stk <- buildFLSss330(readOutputss3(ass.wd)) # same as readFLSss3

hke.stkn.age0 <- stock.n(hke.stk)

dnms.age <- dnms.len <- dimnames(hke.stk)
names(dnms.len)[1] <- "size"
dnms.len$size <- as.character(replist$lbinspop)

hke.stkn.age <- FLQuant(dimnames = dnms.age)
hke.stkn.len <- FLQuant(dimnames = dnms.len)

for (u in dnms.age$unit) for (s in dnms.age$season) {
  
  if (u == "F2") { sex <- 1; bs <- 2} else if (u == "F3") { sex <- 1; bs <- 3} else 
    if (u == "M2") { sex <- 2; bs <- 2} else if (u == "M3") { sex <- 2; bs <- 3 }
  
  # nlen <- replist$natlen %>% filter(Sex == sex, BirthSeas == bs, `Beg/Mid` == "B", Seas == s, Yr %in% dnms.len$year) %>% 
  #   select(Yr, dnms.len$size) %>% 
  #   tidyr::pivot_longer(-Yr, names_to = "size", values_to = "value") %>% 
  #   tidyr::pivot_wider(id_cols = size, names_from = "Yr", values_from = "value")
  
  nlen <- replist$natlen %>% filter(Sex == sex, BirthSeas == bs, `Beg/Mid` == "B", Seas == s, Yr %in% dnms.len$year) %>% 
    select(Yr, dnms.len$size)
  
  if (any(nlen$Yr != dnms.len$year)) stop("Check years.")
  if (any(names(nlen)[-1] != dnms.len$size)) stop("Check size classes.")
  
  # nage <- replist$natage %>% filter(Sex == sex, BirthSeas == bs, `Beg/Mid` == "B", Seas == s, Yr %in% dnms.age$year) %>% 
  #   select(Yr, dnms.age$age) %>% 
  #   tidyr::pivot_longer(-Yr, names_to = "age", values_to = "value") %>% 
  #   tidyr::pivot_wider(id_cols = age, names_from = "Yr", values_from = "value")
  
  nage <- replist$natage %>% filter(Sex == sex, BirthSeas == bs, `Beg/Mid` == "B", Seas == s, Yr %in% dnms.age$year) %>% 
    select(Yr, dnms.age$age) 
  
  if (any(nage$Yr != dnms.age$year)) stop("Check years.")
  if (any(names(nage)[-1] != dnms.age$size)) stop("Check size classes.")
  
  hke.stkn.len[,,u,s,] <- nlen %>% select(-Yr) %>% t()
  hke.stkn.age[,,u,s,] <- nage %>% select(-Yr) %>% t()
  
}

if (sum(hke.stkn.age - hke.stkn.age0) != 0)
  stop("Check numbers-at-age")

save(hke.stkn.age, hke.stkn.len, hke.stk, file = outfile_all)


# Annual  ---------------------------------------------------------------------

dnms <- dimnames(hke.stk)

# Characteristics
# - ages    : 0-15
ages  <- as.numeric(dnms$age)
# - years   : 1978-dtyr (or dtyr+3 if forecast = TRUE)
yrs      <- as.numeric(dnms$year)
yrs.hist <- min(yrs):dtyr
yrs.proj <- dtyr+1:dtyr+3
yrs.ass  <- yrs.hist
# - units   : "F2" "F3" "M2" "M3"
units <- dnms$unit
# - seasons : 1-4
ssons <- as.numeric(dnms$season)


# Collapse to one season and unit

# stk <- noseason(nounit(hke.stk), weighted = TRUE)
stk <- simplify(hke.stk, weighted = TRUE) # combines season (noseason), area (noarea) and unit (no unit), if necessary
# note: spwn.season argument not used as hke spans at 2 different seasons

# needs correction for recruits

# - numbers: sum
raf <-  stock.n(hke.stk)[1,,"F2",2,] + stock.n(hke.stk)[1,,"F3",3,]
ram <-  stock.n(hke.stk)[1,,"M2",2,] + stock.n(hke.stk)[1,,"M3",3,] 
stock.n(stk)[1,] <- raf + ram

# - natural mortality: weighted mean
mf <- replist$M_at_age %>% filter(Sex == 1 & Yr %in% yrs) %>% select(Yr, '0') %>% 
  mutate_all(function(x) as.numeric(x)) %>% 
  tidyr::pivot_wider(names_from = Yr, values_from = '0') %>% unlist()
mm <- replist$M_at_age %>% filter(Sex == 2 & Yr %in% yrs) %>% select(Yr, '0') %>% 
  mutate_all(function(x) as.numeric(x)) %>% 
  tidyr::pivot_wider(names_from = Yr, values_from = '0') %>% unlist()

m(stk)[1,,] <- (raf * mf + ram * mm)/(raf+ram) 

# - maturity = 0
mat(stk)[1,] <- 0


# correct harvest
harvest(stk) <- harvest(stock.n(stk), catch=catch.n(stk), m=m(stk)) # changes for age 0 (initially 0)


#==============================================================================
# FLSR                                                                     ----
#==============================================================================

hke.sr  <- readFLSRss3(ass.wd) # FLSR

srr <- window(hke.sr, end=dtyr)
# Remove information for projection years
# stk <- window( stk, start=1978, end=dtyr)
# srr <- window( hke.sr, start=1978, end=dtyr)

# deterministic value
model(srr) <- segreg() # bevholtss3: model from SS available at FLCore 
srr <- fmle(srr)

plot(srr)



#==============================================================================
# CHECKs                                                                   ----
#==============================================================================

# Fbar  -----------------------------------------------------------------------

stk@range[c("minfbar","maxfbar")] <- c(1,7)
f17 <- as.data.frame(fbar(stk)) %>% select(year, data) %>% 
  mutate(basis = "FLStock")

fvals <- replist$derived_quants %>% filter(grepl("^F_",Label)) %>% 
  mutate(year = as.numeric(unlist(lapply(strsplit(Label, "_"), function(x) x[2])))) %>% 
  rename(data = Value) %>% select(year, data) %>% 
  mutate(basis = "Stock Synthesis")

f17_plot <- ggplot(bind_rows(f17, fvals), aes(year, data, colour = basis, shape = basis)) + 
  geom_point() + 
  labs(y = "Fbar (1-7)", x = "Year", color = "F source", shape = "F source") +
  theme_bw() +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.75, 0.95),  # x and y coordinates within plot area
        legend.justification = c("left", "top"))

png(f17.plot, 10, 6, 'in', res = 600); print(f17_plot); dev.off()


# female only SSB  ------------------------------------------------------------

SSssb <- subset(replist$derived_quants,substring(Label,1,6)%in%c("SSB_19","SSB_20")) %>% 
  mutate(Yr=as.numeric(substring(Label,5,9)))

plot_df <- bind_rows(SSssb %>% select(Yr, Value) %>% 
                       rename(Year = Yr, SSB = Value) %>% 
                       mutate(Source = "Stock Synthesis"), 
                     as.data.frame(ssb(stk)) %>% select(year, data) %>% 
                       rename(SSB = data, Year = year) %>% 
                       mutate(Source = "FLStock - combined sexes"), 
                     as.data.frame(unitSums(ssb(hke.stk[,,c("F2","F3"),1,]))) %>% 
                       select(year, data) %>% 
                       rename(SSB = data, Year = year) %>% 
                       mutate(Source = "FLStock - female-only"))

ssb_plot <- ggplot(plot_df, aes(x = Year, y = SSB, color = Source)) +
  geom_line(data = subset(plot_df, Source != "Stock Synthesis")) +
  geom_point(data = subset(plot_df, Source == "Stock Synthesis")) +
  scale_color_manual(values = c("Stock Synthesis" = "black", "FLStock - combined sexes" = "black", "FLStock - female-only" = "red")) +
  labs(#title = "SSB comparison", 
       y = "SSB", x = "Year", color = "SSB source") +
  ylim(0, max(plot_df$SSB) * 1.05) +
  theme_bw() +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.05, 0.95),  # x and y coordinates within plot area
        legend.justification = c("left", "top"))

png(ssb.plot, 10, 6, 'in', res = 600); print(ssb_plot); dev.off()


# - Sex ratio in stock numbers
stknF <- stock.n(hke.stk)[,ac(yrs),c("F2","F3"),1]; stknF[1,] <- raf
stknM <- stock.n(hke.stk)[,ac(yrs),c("M2","M3"),1]; stknM[1,] <- ram
stkn  <- stknF + stknM

ratF_stkn <- stknF/stkn
ratM_stkn <- stknM/stkn

range(ratF_stkn)  # 0.3155807 0.8144674

# - Sex ratio in catch numbers
catnF <- seasonSums(unitSums(catch.n(hke.stk)[,ac(yrs),c("F2","F3")]))
catnM <- seasonSums(unitSums(catch.n(hke.stk)[,ac(yrs),c("M2","M3")]))
catn  <- catnF + catnM

ratF_catn <- catnF/catn
ratM_catn <- catnM/catn

range(ratF_catn)  # 0.3337724 0.8141462


# We correct maturity to get only-female SSB
alpha <- c(ssb(srr)[,ac(yrs)]/ssb(stk))

mat(stk) <- sweep( mat(stk), 2, alpha, "*")

if (any(round(ssb(stk) - ssb(srr), 2) > 0))
  stop("Check SSB values")


# Recruitment  ----------------------------------------------------------------

SSrec <- subset( replist$derived_quants, substring(Label,1,7) %in% c("Recr_19","Recr_20")) %>%
  mutate(Yr=as.numeric(substring(Label,6,10)))

plot_df <- bind_rows(SSrec %>% select(Yr, Value) %>% 
                       rename(Year = Yr, Recruitment = Value) %>% 
                       mutate(Source = "Stock Synthesis"), 
                     as.data.frame(rec(stk)) %>% select(year, data) %>% 
                       rename(Year = year, Recruitment = data) %>% 
                       mutate(Source = "FLStock"), 
                     as.data.frame(rec(hke.sr)) %>% select(year, data) %>% 
                       rename(Year = year, Recruitment = data) %>% 
                       mutate(Source = "FLSR"))

rec_plot <- ggplot(plot_df, aes(x = Year, y = Recruitment, color = Source, linetype = Source)) +
  geom_line(data = subset(plot_df, Source != "Stock Synthesis")) +
  geom_point(data = subset(plot_df, Source == "Stock Synthesis")) +
  scale_color_manual(values = c("Stock Synthesis" = "black", "FLStock" = "green", "FLSR" = "black")) +
  scale_linetype_manual(values = c("Stock Synthesis" = "blank", "FLStock" = "dashed", "FLSR" = "solid")) +
  labs(#title = "Recruitment comparison", 
       y = "Recruitment", x = "Year", 
       color = "REC source", linetype = "REC source") +
  ylim(min(plot_df$Recruitment) * 0.95, max(plot_df$Recruitment) * 1.05) +
  theme_bw() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.05, 0.95),
    legend.justification = c("left", "top")
  )

png(rec.plot, 10, 6, 'in', res = 600); print(rec_plot); dev.off()


# Catch  ----------------------------------------------------------------------

if (any(round(landings.n(stk) + discards.n(stk) - catch.n(stk),8) != 0))
  stop("Check catch numbers-at-age")

if (any(landings(stk) != computeLandings(stk) | discards(stk) != computeDiscards(stk) | catch(stk) != computeCatch(stk)))
  stop("Check total catch values in the FLStock")

if (any(catch.sum$year != yrs.ass)) stop("Check years")

dat0 <- data.frame( year = catch.sum$year, 
            lanStk = landings(stk)[,ac(yrs.ass),drop=TRUE], 
            lanEst = catch.sum$LanEst, 
            lanObs = catch.sum$LanObs, 
            disStk = discards(stk)[,ac(yrs.ass),drop=TRUE], 
            disEst = catch.sum$DisEst, 
            disObs = catch.sum$DisObs, 
            catStk = catch(stk)[,ac(yrs.ass),drop=TRUE], 
            catEst = catch.sum$CatEst, 
            catObs = colSums(rbind(catch.sum$LanObs, catch.sum$DisObs), na.rm = TRUE)) %>% 
  mutate(dif_lanEst = lanStk - lanEst, dif_disEst = disStk - disEst, dif_catEst = catStk - catEst, 
         pdif_lanEst = lanStk/lanEst-1, pdif_disEst = disStk/disEst-1, pdif_catEst = catStk/catEst-1)

write.csv(dat0, file = cat.table)


#==============================================================================
# Summary                                                                  ----
#==============================================================================

dat <- data.frame( year = catch.sum$year, indicator = "land", type = "FLStock", value = landings(stk)[,ac(yrs.ass),drop=TRUE]) %>% 
  bind_rows( data.frame(year = catch.sum$year, indicator = "land", type = "est", value = catch.sum$LanEst)) %>% 
  bind_rows( data.frame(year = catch.sum$year, indicator = "land", type = "obs", value = catch.sum$LanObs)) %>% 
  bind_rows( data.frame(year = catch.sum$year, indicator = "disc", type = "FLStock", value = discards(stk)[,ac(yrs.ass),drop=TRUE])) %>% 
  bind_rows( data.frame(year = catch.sum$year, indicator = "disc", type = "est", value = catch.sum$DisEst)) %>% 
  bind_rows( data.frame(year = catch.sum$year, indicator = "disc", type = "obs", value = catch.sum$DisObs)) %>% 
  bind_rows( data.frame(year = catch.sum$year, indicator = "catch", type = "FLStock", value = catch(stk)[,ac(yrs.ass),drop=TRUE])) %>% 
  bind_rows( data.frame(year = catch.sum$year, indicator = "catch", type = "est", value = catch.sum$CatEst)) %>% 
  bind_rows( data.frame(year = catch.sum$year, indicator = "catch", type = "obs", value = catch.sum$CatObs))


pobs <- ggplot(dat, aes(year, value, col = type, lty = type)) +  
  geom_line(linewidth = 1.25) + 
  facet_grid(indicator ~ ., scales = "free") + 
  theme_bw() + ylab("tonnes")

png(flstkest.plot, 10, 6, 'in', res = 600); print(pobs); dev.off()


#==============================================================================
# FLStock: with observed catch                                             ----
#==============================================================================

stk.obs <- stk


# Replace catch information by observations

lanRat <- catch.sum$LanObs/landings(stk.obs)[,ac(yrs.ass),drop=TRUE]
disRat <- catch.sum$DisObs/discards(stk.obs)[,ac(yrs.ass),drop=TRUE]
disRat[is.na(disRat)] <- 0

sd(lanRat)/mean(lanRat) # CV = 0.08755448
sd(disRat)/mean(disRat) # CV = 1.127463

landings.n(stk.obs)[,ac(yrs.ass),] <- sweep(landings.n(stk.obs)[,ac(yrs.ass),], 2, lanRat, "*")
discards.n(stk.obs)[,ac(yrs.ass),] <- sweep(discards.n(stk.obs)[,ac(yrs.ass),], 2, disRat, "*")

catch.n(stk.obs)[,ac(yrs.ass),] <- landings.n(stk.obs)[,ac(yrs.ass),] + discards.n(stk.obs)[,ac(yrs.ass),]

catch.wt(stk.obs)[,ac(yrs.ass),] <- (landings.n(stk.obs)[,ac(yrs.ass),] * landings.wt(stk.obs)[,ac(yrs.ass),] + 
                                  discards.n(stk.obs)[,ac(yrs.ass),] * discards.wt(stk.obs)[,ac(yrs.ass),]) / (landings.n(stk.obs)[,ac(yrs.ass),] + discards.n(stk.obs)[,ac(yrs.ass),])

landings(stk.obs) <- computeLandings(stk.obs)
discards(stk.obs) <- computeDiscards(stk.obs)
catch(stk.obs)    <- computeCatch(stk.obs)

if (any(round(computeLandings(stk.obs)[,ac(yrs.ass),],8) != catch.sum$LanObs) | 
    any(round(computeDiscards(stk.obs)[,ac(yrs.ass),],8) != catch.sum$DisObs, na.rm = TRUE) |
    any(round(computeCatch(stk.obs)[,ac(yrs.ass),],8) != round(catch.sum$CatObs,8)))
  stop("Catches incorrectly raised")

harvest(stk.obs) <-  harvest(stock.n(stk.obs), catch=catch.n(stk.obs), m=m(stk.obs))

pobs <- ggplot(dat, aes(year, value, col = type, lty = type)) +
  geom_line(linewidth = 1.25) +
  facet_grid(indicator ~ ., scales = "free") +
  theme_bw()

# plot

dat[dat$indicator == "land" & dat$type == "FLStock","value"]  <- landings(stk.obs)[,ac(catch.sum$year),drop=TRUE]
dat[dat$indicator == "disc" & dat$type == "FLStock","value"]  <- discards(stk.obs)[,ac(catch.sum$year),drop=TRUE]
dat[dat$indicator == "catch" & dat$type == "FLStock","value"] <- catch(stk.obs)[,ac(catch.sum$year),drop=TRUE]

pobs <- ggplot(dat, aes(year, value, col = type, lty = type)) +
  geom_line(linewidth = 1.25) +
  facet_grid(indicator ~ ., scales = "free") +
  theme_bw()

png(flstkobs.plot, 10, 6, 'in', res = 600); print(pobs); dev.off()


#==============================================================================
# FLStock: with estimated catch + weights corrected                        ----
#==============================================================================

stk.cwt <- stk


# Correct mean weights to match total landings and discards

lanRat <- catch.sum$LanEst/landings(stk.cwt)[,ac(yrs.ass),drop=TRUE]
disRat <- catch.sum$DisEst/discards(stk.cwt)[,ac(yrs.ass),drop=TRUE]
disRat[is.na(disRat)] <- 0

sd(lanRat)/mean(lanRat) # CV = 0.08601066
sd(disRat)/mean(disRat) # CV = 0.1666901

landings.wt(stk.cwt)[,ac(yrs.ass),] <- sweep(landings.wt(stk.cwt)[,ac(yrs.ass),], 2, lanRat, "*")
discards.wt(stk.cwt)[,ac(yrs.ass),] <- sweep(discards.wt(stk.cwt)[,ac(yrs.ass),], 2, disRat, "*")

catch.wt(stk.cwt)[,ac(yrs.ass),] <- (landings.n(stk.cwt)[,ac(yrs.ass),] * landings.wt(stk.cwt)[,ac(yrs.ass),] + 
                                  discards.n(stk.cwt)[,ac(yrs.ass),] * discards.wt(stk.cwt)[,ac(yrs.ass),]) / (landings.n(stk.cwt)[,ac(yrs.ass),] + discards.n(stk.cwt)[,ac(yrs.ass),])

landings(stk.cwt) <- computeLandings(stk.cwt)
discards(stk.cwt) <- computeDiscards(stk.cwt)
catch(stk.cwt)    <- computeCatch(stk.cwt)

# check values

if (any(round(computeLandings(stk.cwt)[,ac(yrs.ass),],8) != round(catch.sum$LanEst,8)) | 
    any(round(computeDiscards(stk.cwt)[,ac(yrs.ass),],8) != round(catch.sum$DisEst,8), na.rm = TRUE) | 
    any(round(computeCatch(stk.cwt)[,ac(yrs.ass),],8) != round(catch.sum$CatEst,8), na.rm = TRUE))
  stop("Catches incorrectly raised")

harvest(stk.cwt) <-  harvest(stock.n(stk.cwt), catch=catch.n(stk.cwt), m=m(stk.cwt))

# plot

dat[dat$indicator == "land" & dat$type == "FLStock","value"]  <- landings(stk.cwt)[,ac(catch.sum$year),drop=TRUE]
dat[dat$indicator == "disc" & dat$type == "FLStock","value"]  <- discards(stk.cwt)[,ac(catch.sum$year),drop=TRUE]
dat[dat$indicator == "catch" & dat$type == "FLStock","value"] <- catch(stk.cwt)[,ac(catch.sum$year),drop=TRUE]

pobscwt <- ggplot(dat, aes(year, value, col = type, lty = type)) +  
  geom_line(linewidth = 1.25) + 
  facet_grid(indicator ~ ., scales = "free") + 
  theme_bw()

png(flstkestcwt.plot, 10, 6, 'in', res = 600); print(pobscwt); dev.off()


#==============================================================================
# FLStock: with observed catch + weights corrected                         ----
#==============================================================================

stk.obs.cwt <- stk.cwt


# Replace catch information by observations

lanRat <- catch.sum$LanObs/landings(stk.obs.cwt)[,ac(yrs.ass),drop=TRUE]
disRat <- catch.sum$DisObs/discards(stk.obs.cwt)[,ac(yrs.ass),drop=TRUE]
disRat[is.na(disRat)] <- 0

sd(lanRat)/mean(lanRat) # CV = 0.08606727
sd(disRat)/mean(disRat) # CV = 1.183908

landings.n(stk.obs.cwt)[,ac(yrs.ass),] <- sweep(landings.n(stk.obs.cwt)[,ac(yrs.ass),], 2, lanRat, "*")
discards.n(stk.obs.cwt)[,ac(yrs.ass),] <- sweep(discards.n(stk.obs.cwt)[,ac(yrs.ass),], 2, disRat, "*")

catch.n(stk.obs.cwt)[,ac(yrs.ass),] <- landings.n(stk.obs.cwt)[,ac(yrs.ass),] + discards.n(stk.obs.cwt)[,ac(yrs.ass),]

catch.wt(stk.obs.cwt)[,ac(yrs.ass),] <- (landings.n(stk.obs.cwt)[,ac(yrs.ass),] * landings.wt(stk.obs.cwt)[,ac(yrs.ass),] + 
                                      discards.n(stk.obs.cwt)[,ac(yrs.ass),] * discards.wt(stk.obs.cwt)[,ac(yrs.ass),]) / (landings.n(stk.obs.cwt)[,ac(yrs.ass),] + discards.n(stk.obs.cwt)[,ac(yrs.ass),])

landings(stk.obs.cwt) <- computeLandings(stk.obs.cwt)
discards(stk.obs.cwt) <- computeDiscards(stk.obs.cwt)
catch(stk.obs.cwt)    <- computeCatch(stk.obs.cwt)

if (any(round(computeLandings(stk.obs.cwt)[,ac(yrs.ass),],8) != catch.sum$LanObs) | 
    any(round(computeDiscards(stk.obs.cwt)[,ac(yrs.ass),],8) != catch.sum$DisObs, na.rm = TRUE) |
    any(round(computeCatch(stk.obs.cwt)[,ac(yrs.ass),],8) != round(catch.sum$CatObs,8)))
  stop("Catches incorrectly raised")

harvest(stk.obs.cwt) <-  harvest(stock.n(stk.obs.cwt), catch=catch.n(stk.obs.cwt), m=m(stk.obs.cwt))

pobs <- ggplot(dat, aes(year, value, col = type, lty = type)) +
  geom_line(linewidth = 1.25) +
  facet_grid(indicator ~ ., scales = "free") +
  theme_bw()

# plot

dat[dat$indicator == "land" & dat$type == "FLStock","value"]  <- landings(stk.obs.cwt)[,ac(catch.sum$year),drop=TRUE]
dat[dat$indicator == "disc" & dat$type == "FLStock","value"]  <- discards(stk.obs.cwt)[,ac(catch.sum$year),drop=TRUE]
dat[dat$indicator == "catch" & dat$type == "FLStock","value"] <- catch(stk.obs.cwt)[,ac(catch.sum$year),drop=TRUE]

pobs <- ggplot(dat, aes(year, value, col = type, lty = type)) +
  geom_line(linewidth = 1.25) +
  facet_grid(indicator ~ ., scales = "free") +
  theme_bw()

png(flstkobscwt.plot, 10, 6, 'in', res = 600); print(pobs); dev.off()


#==============================================================================
# SAVE: FLStock & FLSR                                                     ----
#==============================================================================

# nsamp <- 1000
# fit_bh <- eqsr_fit( window(stk,start=1978,end=2018), nsamp = nsamp, models = c("Bevholt")) 

hke.stk.ini    <- stk
hke.stk        <- stk.cwt
hke.stk.obs    <- stk.obs
hke.stk.obscwt <- stk.obs.cwt


# Save FLR objects (FLStock & FLSR) and SRR with parametric uncertainty

save( hke.stk.ini, hke.sr, file = outfile_est)
save( hke.stk, hke.sr, file = outfile_ecw)
save( hke.stk.obs, file = outfile_obs)
save( hke.stk.obscwt, file = outfile_ocw)


#==============================================================================
# PLOTS                                                                    ----
#==============================================================================


plot(FLStocks(est = hke.stk.ini, obs = hke.stk.obs, est_cwt = hke.stk, obs_cwt = hke.stk.obscwt)) + 
  facet_wrap(~qname, scales="free")

# without weights correction
plot(FLStocks(est = hke.stk.ini, obs = hke.stk.obs)) + facet_wrap(~qname, scales="free")
# with weights correction
plot(FLStocks(est_cwt = hke.stk, obs_cwt = hke.stk.obscwt)) + facet_wrap(~qname, scales="free")

