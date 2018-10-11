## Archaeal and bacterial amoA gene abundance
## from CT_amoA study
## Paul C. Selmants
## 2018-04-13
## R version 3.4.4

#load dplyr version 0.7.4 into R
library(dplyr)

#read in Archaeal amoA gene abundance qPCR data and format into
#amoA copies per ng of DNA per soil sample
arcamoA <- read.csv("arc_qPCR.csv", stringsAsFactors = FALSE) %>%
	mutate(Stand = rep(c('BSH', 'FSH', 'HEN', 'INT', 'LCO',
		 'LVY', 'RRD', 'UCO', 'UIN'), each = 6),
		Sample = rep(c(1:6), times = 9),
		arc_copies_ngDNA = arc_amoA_5ngDNA/5) %>%
	select(Stand, Sample, arc_copies_ngDNA)

#read in Bacterial amoA gene abundance qPCR data and format into
#amoA copies per ng of DNA per soil sample
bacamoA <- read.csv("bac_qPCR.csv", stringsAsFactors = FALSE) %>%
	mutate(Stand = rep(c('BSH', 'FSH', 'HEN', 'INT', 'LCO',
		 'LVY', 'RRD', 'UCO', 'UIN'), each = 6),
		Sample = rep(c(1:6), times = 9),
		bac_copies_ngDNA = bac_copies_10ngDNA/10) %>%
	select(Stand, Sample, bac_copies_ngDNA) 

#read in soil mass, GWC and total DNA concentration data
soilDNA <- read.csv('DNA_soil.csv', stringsAsFactors = FALSE) %>%
	mutate(Stand = rep(c('BSH', 'FSH', 'HEN', 'INT', 'LCO',
		 'LVY', 'RRD', 'UCO', 'UIN'), each = 6),
		Sample = rep(c(1:6), times = 9)) %>%
	select(Stand, Sample, fwt_g, GWC, DNA_ng_uL) 

#join Archaea and Bacteria amoA data with soil data and calculate
#amoA copies per gram of soil, A:B amoA gene copy ratios
amoA <- left_join(arcamoA, bacamoA, by = c('Stand', 'Sample')) %>%
	left_join(., soilDNA, by = c('Stand', 'Sample')) %>%
	mutate(drysoil_g = fwt_g-(GWC*fwt_g), 
		DNAsoil_ng_g = (DNA_ng_uL*100)/drysoil_g, 
		arc_copies_gsoil = arc_copies_ngDNA*DNAsoil_ng_g, 
		bac_copies_gsoil = bac_copies_ngDNA*DNAsoil_ng_g, 
		abratio = arc_copies_gsoil/bac_copies_gsoil,
		Zone = rep(c('Hybrid', 'Hybrid', 'Narrowleaf', 'Fremont',
			'Narrowleaf', 'Fremont', 'Fremont', 'Narrowleaf', 
			'Hybrid'), each = 6)) %>%
	select(Zone, Stand, Sample, arc_copies_gsoil, bac_copies_gsoil, abratio)

#summarize amoA data by stand	
amoAmean <- amoA %>%
	group_by(Stand) %>%
	summarize(arc_mean = mean(arc_copies_gsoil)/1e6, 
		arc_se = ((sd(arc_copies_gsoil))/sqrt(6))/1e6,
		bac_mean = mean(bac_copies_gsoil, na.rm = TRUE)/1e6,
		bac_se = ((sd(bac_copies_gsoil, na.rm = TRUE))/sqrt(6))/1e6, 
		ab_mean = mean(abratio, na.rm = TRUE),
		ab_se = ((sd(abratio, na.rm = TRUE))/sqrt(6)))

#read in soluble and bound foliar condensed tannin (CT) concentration data,
#sum to get total CT, and calculate mean and standard error by stand	
ct <- read.csv('foliarCT_test.csv', stringsAsFactors = FALSE) %>%
	mutate(totalCT = boundCT + solubleCT) %>%
	select(Stand, totalCT) %>%
	group_by(Stand) %>%
	summarize(CT_mean = mean(totalCT),
		CT_se = (sd(totalCT))/sqrt(6))

#join amoA and ct dataframes
amoAct <- left_join(amoAmean, ct, by = 'Stand')

##linear model of Archaeal amoA gene abundance as a function of
#foliar condensed tannins
arcCTmod <- lm(arc_mean ~ CT_mean, data = amoAct) 

bacCTmod <- lm(bac_mean ~ CT_mean, data = amoAct)

#summarize archaeal:bacterial amoA ratios by Zone
abmean <- amoA %>%
	group_by(Zone) %>%
	summarize(abmean = mean(abratio, na.rm = TRUE),
		abse = sd(abratio, na.rm = TRUE)/sqrt(6))
