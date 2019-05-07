## Archaeal and bacterial amoA gene abundance
## from CT_amoA study
## Paul C. Selmants
## 2018-04-13
## R version 3.5.3

#load dplyr 0.8.0.1 and tidyr 0.8.3 into R
library(dplyr)
library(tidyr)

#read in Archaeal amoA gene abundance qPCR data and format into
#amoA copies per ng of DNA per soil sample
arcamoA <- read.csv("arc_qPCR.csv", stringsAsFactors = FALSE) %>%
	separate(ID, c('stand', 'sample'), sep = 3, convert = TRUE) %>%
	mutate(arc_copies_ngDNA = arc_amoA_5ngDNA/5) %>%
	select(stand, sample, arc_copies_ngDNA)

#read in Bacterial amoA gene abundance qPCR data and format into
#amoA copies per ng of DNA per soil sample
bacamoA <- read.csv("bac_qPCR.csv", stringsAsFactors = FALSE) %>%
	separate(ID, c('stand', 'sample'), convert = TRUE) %>%
	mutate(bac_copies_ngDNA = bac_copies_10ngDNA/10) %>%
	select(stand, sample, bac_copies_ngDNA) 

#read in soil mass, GWC and total DNA concentration data
soilDNA <- read.csv('DNA_soil.csv', stringsAsFactors = FALSE) %>%
	separate(ID, c('stand', 'sample'), convert = TRUE) %>%
	select(stand, sample, fwt_g, GWC, DNA_ng_uL) 

#join Archaea and Bacteria amoA data with soil data and calculate
#amoA copies per gram of soil, A:B amoA gene copy ratios
amoA <- left_join(arcamoA, bacamoA, by = c('stand', 'sample')) %>%
	left_join(., soilDNA, by = c('stand', 'sample')) %>%
	mutate(drysoil_g = fwt_g-(GWC*fwt_g), 
		DNAsoil_ng_g = (DNA_ng_uL*100)/drysoil_g, 
		arc_copies_gsoil = arc_copies_ngDNA*DNAsoil_ng_g, 
		bac_copies_gsoil = bac_copies_ngDNA*DNAsoil_ng_g, 
		abratio = arc_copies_gsoil/bac_copies_gsoil,
		zone = rep(c('hybrid', 'hybrid', 'narrowleaf',
			'fremont', 'narrowleaf', 'fremont', 'fremont',
			'narrowleaf', 'hybrid'), each = 6)) %>%
	select(zone, stand, sample, arc_copies_gsoil, bac_copies_gsoil, abratio)

#summarize amoA data by stand	
amoAmean <- amoA %>%
	group_by(zone, stand) %>%
	summarize(arc_mean = mean(arc_copies_gsoil)/1e6, 
		arc_se = ((sd(arc_copies_gsoil))/sqrt(6))/1e6,
		bac_mean = mean(bac_copies_gsoil, na.rm = TRUE)/1e6,
		bac_se = ((sd(bac_copies_gsoil, na.rm = TRUE))/sqrt(6))/1e6, 
		ab_mean = mean(abratio, na.rm = TRUE),
		ab_se = ((sd(abratio, na.rm = TRUE))/sqrt(6)))

#join amoA and stand_ct dataframe from Nitrification.R script
amoAct <- left_join(amoAmean, stand_ct, by = c('zone', 'stand'))
	
#linear model of Archaeal amoA gene abundance 
#as a function of foliar condensed tannins
arcCTmod <- lm(arc_mean ~ ct_mean, data = amoAct)
#linear model of Bacterial amoA gene abundance 
#as a function of foliar condensed tannins
bacCTmod <- lm(bac_mean ~ ct_mean, data = amoAct)
#summarize archaeal:bacterial amoA ratios by zone
abmean <- amoA %>%
	group_by(zone) %>%
	summarize(abmean = mean(abratio, na.rm = TRUE),
		absd = sd(abratio, na.rm = TRUE),
		abse = absd/sqrt(18))
