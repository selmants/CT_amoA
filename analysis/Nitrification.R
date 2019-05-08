## Soil nitrification potentials
## for CT_amoA study
## Paul C. Selmants
## 2018-04-13
## updated 2019-05-08
## R version 3.5.3

#load dplyr version 0.8.0.1 and tidyr version 0.8.3 into R
library(dplyr)
library(tidyr)

#Read raw, blank-corrected NO3 concentration data (mg NO3-N/L) 
#into R and calculate regression slope for each sample incubation period
NO3_slope <- read.csv("NO3_raw.csv", stringsAsFactors = FALSE) %>%
	group_by(stand, sample) %>%
	summarize(slope = lm(NO3_bc ~ incub_h)$coefficients[2])

#Read nitrif soil sample mass (g) and GWC data (proportion) into R and
#reformat ID labels into separate columns for Stand and Sample
soil <- read.csv("nitrif_soil.csv", stringsAsFactors = FALSE) %>%
	separate(Soil_ID, c("stand", "sample"), convert = TRUE) %>%
	select(zone, stand, sample, fwt_g, GWC)

#join NO3 incubation slope data with soil data, calculate 
#potential nitrifcation rates in mg NO3-N/kg dry soil/day, and
#summarize by calculating stand mean and standard error
nitrif <- left_join(NO3_slope, soil, by = c("stand", "sample")) %>%
	mutate(volH2O = fwt_g*GWC/1000,
		dry_soil = fwt_g/(1+GWC)/1000, 
		rate_h = slope*((0.1+volH2O)/dry_soil),
		rate_d = rate_h*24) %>%
	group_by(zone, stand) %>%
	summarize(nitr_mean = mean(rate_d),
		nitr_se = (sd(rate_d))/(sqrt(6)))

#read in soluble and bound foliar condensed tannin (CT) concentration data (%),
#sum to get total foliar % CT, mulitiply by 10 to get mg/g. 
ct <- read.csv('foliarCT_test.csv', stringsAsFactors = FALSE) %>%
	mutate(totalCT = (boundCT + solubleCT)*10) %>%
	select(zone, stand, totalCT) 
#calculate foliar ct mean and standard deviation by zone
zone_ct <- ct %>%
	group_by(zone) %>%
	summarize(ct_mean = mean(totalCT), 
		ct_sd = sd(totalCT), 
		ct_se = ct_sd/sqrt(18))
#calculate foliar ct mean and standard error by stand
stand_ct <- ct %>%
	group_by(zone, stand) %>%
	summarize(ct_mean = mean(totalCT),
		ct_se = (sd(totalCT))/sqrt(6))

#join soil potential nitrification data with foliar condensed tannin data
ctnitrif <- left_join(nitrif, stand_ct, by = c('zone', 'stand'))

#linear model of mean stand-level soil nitrification potential 
#as a function of mean stand-level foliar condensed tannins
nitrCTmod <- lm(nitr_mean ~ ct_mean, data = ctnitrif)

