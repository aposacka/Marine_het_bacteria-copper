# script to produce Fig. 4 of the Frontiers manuscript with statistical tests


# load the data from csv
library(tidyverse)
data <-read_csv("C:/Users/Ania/Bact_Stoichiometry/Frontiers/Copper_to-carbon-ratios-compilation/Cu_quotas-synthesis-Aquil.csv") 
glimpse(data)

# sort out the variable formats
data$Strategy <- as.factor(data$Strategy)
data$Taxon<- as.factor(data$Taxon)
data$Species<- as.factor(data$Species)
data$Domain<- as.factor(data$Domain)

# check names for Domains
#levels(data$Domain)
# are there duplicated values at Cu_C>5?
#ata%>%select(Ref,Domain,Species,Cu_C)%>%filter(Cu_C>5)

p1 <- data%>%
	filter(Cu_C<15, Domain %in% c("Euk_algae","Het_bacteria"))%>%
	ggplot(aes(x = Domain, y = Cu_C,fill=Domain))+
	geom_boxplot()+
#	geom_jitter(width=0.05)+
	scale_fill_manual(values=c("#00AFBB", "#E7B800"))+
	ylab(expression("Cu:C" ~(mu~mol:mol)))+
	ylim(0,12.5)+
	theme_bw()+
	theme(panel.grid.major.x = element_blank(), 
				panel.grid.minor.x = element_blank(),
				panel.grid.major.y = element_blank(),
				panel.grid.minor.y = element_blank(),
				axis.title.y= element_text(size=12),
				axis.title.x= element_blank(),
				axis.text.x = element_text(angle = 25, hjust = 0.9),
				axis.text = element_text(size=12),
				legend.text = element_text(size=10),
				legend.position = "none")

save_plot (filename="Fig4_Bact_vs_Phytos.tiff", plot= p1, base_height= 4, base_width =9.5)

#-----------------------------STATISTICAL ANALYSIS---------------------------------------------

p1

