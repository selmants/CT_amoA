## ggplot2 R code for three panel scatterplot figure
## of foliar condensed tannins as a predictor of soil nitrification
## potentials (a), soil archaeal amoA gene abundance (b), and 
## soil bacterial amoA gene abundance (c) for 
## CT_amoA study
## Paul C. Selmants
## 2018-10-05
## R version 3.4.4 

# load ggplot2 version 2.2.1 and gridExtra version 2.3 into R
library(ggplot2)
library(gridExtra)

#make a scatterplot of linear relationship between stand means of soil
#potential nitrification and foliar condensed tannins using dataframe from
#'Nitrification.R' script
np <- ggplot(ctnitrif, aes(CT_mean, nitr_mean)) +
	geom_errorbar(aes(ymin = nitr_mean - nitr_se, 
		ymax = nitr_mean + nitr_se), colour = 'dark grey', width = 0, 
		size = 0.3) +
	geom_errorbarh(aes(xmin = CT_mean - CT_se, 
		xmax = CT_mean + CT_se), colour = 'dark grey', height = 0,
		size = 0.3) +
	geom_point(size = 3) +
	geom_smooth(method = lm, se = FALSE) +
	scale_y_continuous(breaks = seq(0,14,2), limits = c(0,14),
		name = expression(
			'Potential nitrification (mg NO'[3]['-']*'-N kg'^-1*' d'^-1*')')) +
	scale_x_continuous(breaks = seq(0,20,4), limits = c(0,22),
		name = "") + 
	theme_classic() +
	theme(axis.title.x = element_text(size=11),
           axis.text.x  = element_text(size=10),
           axis.title.y = element_text(size=11),
           axis.text.y = element_text(size=10),
           panel.grid.minor=element_blank(),
           panel.grid.major=element_blank()) +
	annotate("text", x = 0.25, y = 14, label = "a)", size = 4.5)

#Scatterplot of linear relationship between stand means of
#soil archaeal amoA gene abundance and foliar condensed tannins using
#dataframe from 'amoA.R' script
ap <- ggplot(amoAct, aes(CT_mean, arc_mean)) +
	geom_errorbar(aes(ymin = arc_mean - arc_se, 
		ymax = arc_mean + arc_se), colour = 'dark grey', width = 0, 
		size = 0.3) +
	geom_errorbarh(aes(xmin = CT_mean - CT_se, 
		xmax = CT_mean + CT_se), colour = 'dark grey', height = 0,
		size = 0.3) +
	geom_point(size = 2.5) +
	geom_smooth(method = lm, se = FALSE) +
	scale_y_continuous(breaks = seq(0,24,4), limits = c(0,24),
		name = expression(Archaeal~italic(amoA)~(10^6~copies~g^-1))) +
	scale_x_continuous(breaks = seq(0,20,4), limits = c(0,22),
		name = "") + 
	theme_classic() +
	theme(axis.title.x = element_text(size=11),
           axis.text.x  = element_text(size=10),
           axis.title.y = element_text(size=11),
           axis.text.y = element_text(size=10)) +
	annotate("text", x = 0.25, y = 24, label = "b)", size = 4.5)

#Scatterplot of linear relationship between stand means of
#soil bacterial amoA gene abundance and foliar condensed tannins using
#dataframe from 'amoA.R' script
bp <- ggplot(amoAct, aes(CT_mean, bac_mean)) +
	geom_errorbar(aes(ymin = bac_mean - bac_se, 
		ymax = bac_mean + bac_se), colour = 'dark grey', width = 0, 
		size = 0.3) +
	geom_errorbarh(aes(xmin = CT_mean - CT_se, 
		xmax = CT_mean + CT_se), colour = 'dark grey', height = 0,
		size = 0.3) +
	geom_point(size = 2.5) +
	scale_y_continuous(breaks = seq(0,24,4), limits = c(0,24),
		name = expression(Bacterial~italic(amoA)~(10^6~copies~g^-1))) +	
	scale_x_continuous(breaks = seq(0,20,4), limits = c(0,22),
		name = expression(Foliar~condensed~tannins~(mg~g^-1))) + 
	theme_classic() +
	theme(axis.title.x = element_text(size=11),
           axis.text.x  = element_text(size=10),
           axis.title.y = element_text(size=11),
           axis.text.y = element_text(size=10)) +
	annotate("text", x = 0.25, y = 24, label = "c)", size = 4.5)

#arrange into three-panel graph of nitrification, archaeal amoA,
# and bacterial amoA as a function of foliar CT
grid.arrange(np, ap, bp, ncol = 3)
#save plot as PDF
nabp <- arrangeGrob(np, ap, bp, ncol = 1)
ggsave('Fig1.pdf', nabp, width = 4, height = 10) 

