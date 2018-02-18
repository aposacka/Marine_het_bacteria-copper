#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~README~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to produce a figure with metals:p at various Cu levels in 4 strains 
# of henetrotrophic bacteria

# loading packages
library(tidyverse)
library(cowplot)

# loading data
metal_data <-read_csv("Datasets/Bact_phosph_metals.csv") 
growth <-read_csv("Datasets/GR_rates-bact-figs.csv")


#metal_data$Strain <- as.factor(metal_data$Strain)
#metal_data$Cu_level <- as.factor(metal_data$Cu_level)
#metal_data$Me_P <- as.factor(metal_data$Me_P)


# changing the order of factors and converting them to factors
metal_data$Strain<-factor(metal_data$Strain, levels = c("Dokd_P16","R.pomeroyi", "PAlt_P2","PAlt_P26"))
metal_data$Cu_level <- factor(metal_data$Cu_level, levels = c("0.6","2","10","25","50"))
metal_data$Me_P <- factor(metal_data$Me_P,levels = c("Fe_P","Zn_P","Cu_P","Mn_P", "Co_P"))

# checking the orders were correctly converted
#levels(metal_data$Strain)
#levels(metal_data$Cu_level)
#levels(metal_data$Me_P)

# converting char variables to factors in the growth dataset
growth$Strain<- as.factor(growth$Strain)
growth$Cu_total <- as.factor(growth$Cu_total)


# extracting only 4 strains (not plotting other Pseudoalteromonas)
subset <- growth%>%
	filter(Strain %in% c("Dokdonia sp","Pseudoalteromonas(P2)" ,
											 "Pseudoalteromonas(P26)","R.pomeroyi"))

# ordering of the factors
subset$Strain<-factor(subset$Strain, levels = c("Dokdonia sp","R.pomeroyi", 
																								"Pseudoalteromonas(P2)", 
																								"Pseudoalteromonas(P26)"))

# ----------------------------------------------------------------------------
# Cu quota plot [bottom]

copper<-metal_data%>%
  filter(Me_P=="Cu_P")

p1 <-copper%>%
	group_by(Strain,Cu_level,Me_P)%>%
	summarise(mean_q=mean(Quota))%>%
	ggplot(aes(x = Cu_level,y = mean_q,group = 1))+
	geom_point(data=copper,aes(x = Cu_level,y = Quota, color= Strain), size = 1.5, alpha = 1/6)+
	stat_summary(fun.y=mean, geom="point",size = 1.5)+
	facet_wrap(~Strain, scales = "free_y",nrow = 1)+
	geom_line(linetype="dashed")+
#	ylab(expression(atop("Cu quota", 
#										 paste("(mmol Cu:mol P)"))))+
	ylim(0,0.23)+
	theme(legend.position = "none",
				strip.text= element_blank(),
				axis.title.x = element_blank(),
			  axis.title.y = element_blank(),
  			axis.text = element_text(size=6),
 	  		axis.title = element_text(size=6))

#------------------------------------------------------------------------
# growth rates [top]

p2 <- subset%>%
  group_by(Strain,Cu_total)%>%
  summarise(mean_q=mean(mu_day))%>%
  ggplot(aes(x = Cu_total,y = mean_q,group = 1))+
  geom_point(data=subset,aes(x = Cu_total,y = mu_day, color=Strain), size = 1.5, alpha = 1/6)+
  stat_summary(fun.y=mean, geom="point",size = 1.5, color="CCCCCC")+
  facet_wrap(~Strain, scales = "free_y",nrow = 1)+
  geom_line(linetype="dashed")+
  ylim(1,23)+
  theme(legend.position = "none",
        strip.text= element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size=6),
        axis.title = element_text(size=6))


#~~~~~~~~~~~~~~~save plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot <-plot_grid(p2,p1,align = "v",nrow=2)

save_plot (filename="Fig_1.tiff", plot= plot, base_height= 2.5, base_width=5)


