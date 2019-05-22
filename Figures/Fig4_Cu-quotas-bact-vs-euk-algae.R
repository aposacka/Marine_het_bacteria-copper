#----------------------------------------------------
# Script to produce Fig. 4 of the Frontiers manuscript
# comaring Cu:C ratios of eukaryotic algae (published)
# and heterotrophic bacteria
#-----------------------------------------------------


# load the data from csv
library(tidyverse)
data <-read_csv("Data/05_Cu-quotas-lit-aquil-tidydata.csv") 

# convert some variables to factors, for some
# reason they don't get converted from char to 
# factor with read_csv

data$Strategy <- as.factor(data$Strategy)
data$Taxon<- as.factor(data$Taxon)
data$Species<- as.factor(data$Species)
data$Domain<- as.factor(data$Domain)

#--------------------------------------
# find the median for bacteria and phytos
# these values will be plotted. P values 
# can be found in the Rmd file
# Fig4_Bact-vs-phytos-Cu-quotas-stats.Rmd
#---------------------------------------

data%>%
  group_by(Domain)%>%
  summarise(medianz = median(Cu_C))

#----------------------------------------
# Manufacture the plot 
#----------------------------------------

p1 <- data%>%
	filter(Cu_C<15, Domain %in% c("Euk_algae","Het_bacteria"))%>%
	ggplot(aes(x = Domain, y = Cu_C,fill = Domain))+
	geom_boxplot()+
	#geom_jitter(width = 0.05, size = 3, alpha = 1/2)+
	scale_fill_manual(values = c("#00AFBB", "#CCCCCC"))+
  scale_x_discrete(labels = c("Eukaryotic \n phytoplankton", "Heterotrophic \n bacteria"))+
	ylab(expression("Cu:C" ~(mu~mol:mol)))+
  annotate("text", x = 2.12, y = 12.5, label = "Levene, p = 0.029", fontface = 1,size = 4) +
  annotate("text", x = 1.8, y = 11.5, label = "Wilcoxon (two-sided), p = 0.095", fontface = 1,size = 4) +
  annotate("text", x = 1, y = 6, label = "Median = 1.72", fontface = 1,size = 4.5, color = "#00AFBB")+ 
  annotate("text", x = 2, y = 6, label = "Median = 1.11", fontface = 1,size = 4.5, color = "#333333") +
  ylim(0,12.5)+
	theme_bw()+
	theme(panel.grid.major.x = element_blank(), 
				panel.grid.minor.x = element_blank(),
				panel.grid.major.y = element_blank(),
				panel.grid.minor.y = element_blank(),
				axis.title.y= element_text(size = 12),
				axis.title.x= element_blank(),
				axis.text.x = element_text(colour = c("#00AFBB", "#333333")),
				axis.text = element_text(size = 12),
				legend.text = element_text(size = 10),
				legend.position = "none")

#------------------
# Save plot at tiff
#-----------------

library(cowplot)

save_plot (filename ="Fig4_Cu-quotas-bact_vs_euk-algae.tiff", plot = p1, base_height = 4, base_width = 4)



