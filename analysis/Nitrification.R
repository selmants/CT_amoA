## Nitrification potentials in soil
## for CT_amoA study
## Paul C. Selmants
## 2018-04-13
## R version 3.4.4

#load dplyr version 0.7.4 into  R
library(dplyr)

#Read raw, blank-corrected NO3 concentration data (mg NO3-N/L) 
#into R and calculate regression slope for each sample incubation period
NO3_slope <- read.csv("NO3_raw.csv", stringsAsFactors = FALSE) %>%
	group_by(Stand, Sample) %>%
	summarize(slope = lm(NO3_bc ~ incub_h)$coefficients[2])

#Read nitrif soil sample mass (g) and GWC data (proportion) into R and
#reformat ID labels into separate columns for Stand and Sample
soil <- read.csv("nitrif_soil.csv", stringsAsFactors = FALSE) %>%
	mutate(Stand = rep(c('BSH', 'FSH', 'HEN', 'INT', 'LCO',
	 'LVY', 'RRD','UCO', 'UIN'), each = 6),
		Sample = rep(c(1:6), times = 9)) %>%
	select(Stand, Sample, fwt_g, GWC)

#join NO3 incubation slope data with soil data, calculate 
#potential nitrifcation rates in mg NO3-N/kg dry soil/day, and
#summarize by calculating stand mean and standard error
nitrif <- left_join(NO3_slope, soil, by = c("Stand", "Sample")) %>%
	mutate(volH2O = fwt_g*GWC/1000,
		dry_soil = fwt_g/(1+GWC)/1000, 
		rate_h = slope*((0.1+volH2O)/dry_soil),
		rate_d = rate_h*24) %>%
	group_by(Stand) %>%
	summarize(nitr_mean = mean(rate_d),
		nitr_se = (sd(rate_d))/(sqrt(6)))

#read in soluble and bound foliar condensed tannin (CT) concentration data,
#sum to get total CT, and calculate mean and standard error by stand
ct <- read.csv('foliarCT_test.csv', stringsAsFactors = FALSE) %>%
	mutate(totalCT = boundCT + solubleCT) %>%
	select(Stand, totalCT) %>%
	group_by(Stand) %>%
	summarize(CT_mean = mean(totalCT),
		CT_se = (sd(totalCT))/sqrt(6))

#join soil potential nitrification data with foliar condensed tannin data
ctnitrif <- left_join(nitrif, ct, by = 'Stand') 

#linear model of mean stand-level soil nitrification as a function of 
#mean stand-level foliar condensed tannins
nitrCTmod <- lm(nitr_mean ~ CT_mean, data = ctnitrif)

#Pull out R-squared and p-value from summary of linear model 
rsq <- round(summary(nitrCTmod)$adj.r.squared, digits = 2)
p_value <- round(summary(nitrCTmod)$coefficients[2,4], digits = 3)


