#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~README~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to produce a figure with metals:phosphorous at various Cu levels in 4 strains 
# of heterotrophic bacteria shown Fig.1 in Frontiers in Marine Science

# loading packages
library(tidyverse)
library(cowplot)

# loading data
metals <-read_csv("Data/01_Bact-metals-P-norm-tidydata.csv") 
growth <-read_csv("Data/01_Bact-growth-rates-tidydata.csv")


# changing the order of factors and converting them to factors
metals$Strain<-factor(metals$Strain, levels = c("Dokd_P16","R.pomeroyi", "PAlt_P2","PAlt_P26"))
metals$Cu_level <- factor(metals$Cu_level, levels = c("0.6","2","10","25","50"))
metals$Me_P <- factor(metals$Me_P,levels = c("Fe_P","Zn_P","Cu_P","Mn_P", "Co_P"))

growth

# converting char variables to factors in the growth dataset
growth$Strain<- as.factor(growth$Strain)
growth$Cu_total <- as.factor(growth$Cu_total)
#growth$Cu_level <- factor(growth$Cu_level, levels = c("0.6","2","10","25","50"))


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

p1 <-metals%>%
  filter(Me_P == "Cu_P")%>%
	group_by(Strain,Cu_level)%>%
	ggplot(aes(x = Cu_level,y = Quota,color = Strain))+
	geom_jitter(width = 0.1,size = 3, alpha = 1/2)+
	stat_summary(fun.y = mean, geom = "point", pch = "_", size = 10)+
  scale_color_manual(values = c("#009E73", "#666666", "#E69F00","#0072B2"))+ 
  #annotate("rect", xmin=1.8, xmax=3.3, ymin=0.0, ymax=0.25, alpha=.4, fill="gray")+
  scale_y_continuous(limits=c(0,0.25),expand=c(0,0))+
	facet_wrap(~Strain, scales = "free_y",nrow = 1)+
	ylab(expression(atop("Cu quota", 
								 paste("(mmol Cu:mol P)"))))+
  theme_bw()+
  theme(legend.position = "none",
				strip.text = element_blank(),
				axis.title.x = element_blank(),
				panel.grid.major.x = element_blank(), 
				panel.grid.minor.x = element_blank(),
				panel.grid.major.y = element_blank(),
				panel.grid.minor.y = element_blank(),
  			axis.text = element_text(size=10),
				axis.title.y = element_text(size=11))

#------------------------------------------------------------------------
# growth rates [top]


p2 <- subset%>%
  group_by(Strain,Cu_total)%>%
  ggplot(aes(x = Cu_total,y = mu_day,color = Strain))+
  geom_jitter( width = 0.1, size = 3, alpha = 1/2)+
  stat_summary(fun.y=mean, geom="point", pch = "_", size = 10)+
  scale_color_manual(values=c("#009E73", "#666666", "#E69F00","#0072B2"))+
#                     labels=c("Dokd-P16", "R.pomeroyi DSS-3", "PAlt-P2", "PAlt-P26"))+
#  annotate("rect", xmin=1.8, xmax=3.3, ymin=0.0, ymax=23, alpha=.3, fill="gray")+
  scale_y_continuous(limits=c(0,23),expand=c(0,0))+
  facet_wrap(~Strain, scales = "free_y",nrow = 1)+
  ylab ("Growth rate"~ (d^{-1}))+
  theme_bw()+
  theme(legend.position = "none",
        #axis.title = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size=10),
        axis.title.y = element_text(size=11))

#------------------------------------------------------------------
# saving the plot

plot <-plot_grid(p2,p1,align = "v",nrow=2)

save_plot (filename="Fig1_Bact-Cu-growth.tiff", plot= plot,
           base_height= 4, base_width=9)


