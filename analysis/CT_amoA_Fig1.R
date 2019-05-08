## ggplot2 R code for three panel scatterplot figure
## of foliar condensed tannins as a predictor of soil nitrification
## potentials (a), soil archaeal amoA gene abundance (b), and 
## soil bacterial amoA gene abundance (c) for 
## CT_amoA study
## Paul C. Selmants
## 2018-10-05
## R version 3.5.3

# load ggplot2 version 2.3.1.1 and gridExtra version 2.3 into R
library(ggplot2)
library(gridExtra)

#scatterplot of linear relationship between stand means of soil
#potential nitrification and foliar condensed tannins using dataframe from
#'Nitrification.R' script
np <- ggplot(ctnitrif, aes(ct_mean, nitr_mean, shape = zone)) +
	geom_errorbar(aes(ymin = nitr_mean - nitr_se, 
		ymax = nitr_mean + nitr_se), colour = 'dark grey', width = 0, 
		size = 0.3) +
	geom_errorbarh(aes(xmin = ct_mean - ct_se, 
		xmax = ct_mean + ct_se), colour = 'dark grey', height = 0,
		size = 0.3) +
	geom_point( size = 2.5) +
	geom_smooth(method = lm, se = FALSE, aes(group = 1)) +
	scale_y_continuous(breaks = seq(0,14,2), limits = c(0,14),
		name = bquote(
			'Nitrification potential (mg NO'[3]['-']*'-N kg'^-1*' d'^-1*')')) +
	scale_x_continuous(breaks = seq(0,200,40), limits = c(0,220),
		name = "") + 
	theme_classic() +
	theme(axis.title.x = element_text(size=11),
           axis.text.x  = element_text(size=10),
           axis.title.y = element_text(size=11),
           axis.text.y = element_text(size=10),
           legend.position = "none") +
	annotate("text", x = 0.25, y = 14, label = "a)", size = 4.5)

#Scatterplot of linear relationship between stand means of
#soil archaeal amoA gene abundance and foliar condensed tannins using
#dataframe from 'amoA.R' script
ap <- ggplot(amoAct, aes(ct_mean, arc_mean, shape = zone)) +
	geom_errorbar(aes(ymin = arc_mean - arc_se, 
		ymax = arc_mean + arc_se), colour = 'dark grey', width = 0, 
		size = 0.3) +
	geom_errorbarh(aes(xmin = ct_mean - ct_se, 
		xmax = ct_mean + ct_se), colour = 'dark grey', height = 0,
		size = 0.3) +
	geom_point(size = 2.5) +
	geom_smooth(method = lm, se = FALSE, aes(group = 1)) +
	scale_y_continuous(breaks = seq(0,24,4), limits = c(0,24),
		name = expression(Archaeal~italic(amoA)~(10^6~copies~g^-1))) +
	scale_x_continuous(breaks = seq(0,200,40), limits = c(0,220),
		name = "") + 
	theme_classic() +
	scale_shape_discrete(name = 'Zone', 
		labels = c('Fremont', 'hybrid', 'narrowleaf')) +
	theme(axis.title.x = element_text(size=11),
           axis.text.x  = element_text(size=10),
           axis.title.y = element_text(size=11),
           axis.text.y = element_text(size=10),
           legend.title.align = 0.5,
           legend.justification = c(1,1),
           legend.position = c(1,1),
           legend.background = element_rect(fill="gray90", 
           	size=.5, linetype="dotted")) +
	annotate("text", x = 0.25, y = 24, label = "b)", size = 4.5)

#Scatterplot of linear relationship between stand means of
#soil bacterial amoA gene abundance and foliar condensed tannins using
#dataframe from 'amoA.R' script
bp <- ggplot(amoAct, aes(ct_mean, bac_mean, shape = zone)) +
	geom_errorbar(aes(ymin = bac_mean - bac_se, 
		ymax = bac_mean + bac_se), colour = 'dark grey', width = 0, 
		size = 0.3) +
	geom_errorbarh(aes(xmin = ct_mean - ct_se, 
		xmax = ct_mean + ct_se), colour = 'dark grey', height = 0,
		size = 0.3) +
	geom_point(size = 2.5) +
	scale_y_continuous(breaks = seq(0,24,4), limits = c(0,24),
		name = expression(Bacterial~italic(amoA)~(10^6~copies~g^-1))) +	
	scale_x_continuous(breaks = seq(0,200,40), limits = c(0,220),
		name = expression(Foliar~condensed~tannins~(mg~g^-1))) + 
	theme_classic() +
	theme(axis.title.x = element_text(size=11),
           axis.text.x  = element_text(size=10),
           axis.title.y = element_text(size=11),
           axis.text.y = element_text(size=10),
           legend.position = "none") +
	annotate("text", x = 0.25, y = 24, label = "c)", size = 4.5)

#arrange into three-panel graph of nitrification, archaeal amoA,
# and bacterial amoA as a function of foliar CT
grid.arrange(np, ap, bp, ncol = 1)
#save plot as PDF
nabp <- arrangeGrob(np, ap, bp, ncol = 1)
ggsave('Fig1.pdf', nabp, width = 4, height = 10) 

