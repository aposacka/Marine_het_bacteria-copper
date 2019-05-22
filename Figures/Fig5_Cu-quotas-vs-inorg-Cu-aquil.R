# 
# Script to generate Fig.5 in the Frontiers manuscript. This figure compares quotas of 
# phytoplankton and ehterotrophic bacteria as a function of inorganic Cu (cultured in Aquil)


library(tidyverse)
library(cowplot)

dat <-read_csv("Data/05_Cu-quotas-lit-aquil-tidydata.csv") 

dat$Taxon<- as.factor(dat$Taxon)
dat$Species<- as.factor(dat$Species)
dat$Domain<- as.factor(dat$Domain)
dat$Strategy <- as.factor(dat$Strategy)


# find out the mean and medians for plotting
dat%>%
  group_by(Strategy)%>%
  filter(Cu_C<20)%>% # exluding the unusual obs at 29 umol:mol
  summarise(meanz =mean(Cu_C),
            meadianz =median(Cu_C))


#---------------Phytoplankton----------------------------

phytos<-dat%>%
	filter(Strategy == "Photosynthetic",Cu_C<13)%>%
	ggplot(aes(x = Cu_prime_log,y = Cu_C,fill = Taxon))+
	geom_jitter(shape=21,size=5,alpha=1/2, width=0.02)+
	annotate("rect", xmin = 14.5, xmax = 12, ymin = 2.8, ymax = 4.9, alpha =.3, fill ="gray")+
	annotate("segment", x = 13.74, xend = 12, y = 6, yend = 6,
					 arrow=arrow(ends="both", angle=90,length=unit(.2,"cm")))+
	annotate("text", x = 12.8, y = 7.0, label="log [Cu'] range in seawater", size=4.5) +
	annotate("text", x = 13.7, y = 12.5, label="Eukaryotic Phytoplankton", fontface = 2,size=4.5) +
	scale_x_reverse(name ="-log([Cu'])",expand=c(0,0))+
	scale_y_continuous(breaks = c(0,1.5,3,4.5,6,7.5,9,10.5,12),limits = c(0,12.6))+
	ylab(expression("Cu:C" ~(mu~mol:mol)))+
	geom_hline(data = dat, aes(yintercept = 2.30),linetype="dashed",size=0.8)+
	geom_hline(data = dat, aes(yintercept = 1.77),linetype="dotted",size=0.8)+
	theme_bw()+
	theme(panel.grid.major.x = element_blank(), 
				panel.grid.minor.x = element_blank(),
				panel.grid.major.y = element_blank(),
				panel.grid.minor.y = element_blank(),
				axis.title = element_text(size=14),
				axis.text = element_text(size=14),
				legend.text = element_text(size=10),
				legend.position = "bottom")


#-------------Het.bacteria-------------------------------------

bact<-dat%>%
	filter(Strategy == "Heterotrophic")%>%
	ggplot(aes(x=Cu_prime_log,y=Cu_C,fill=Taxon))+
	geom_jitter(shape=23, size=5,alpha=1/2)+
	annotate("rect", xmin=14.5, xmax=12, ymin=2.8, ymax=4.9, alpha=.3, fill="gray")+
	annotate("segment", x = 13.74, xend = 12, y = 6, yend = 6,
					 arrow=arrow(ends="both", angle=90,length=unit(.2,"cm")))+
	annotate("text", x = 12.8, y = 7.0, label="log [Cu'] range in seawater", size=4.5) +
	annotate("text", x = 13.7, y = 12.5, label="Heterotrophic bacteria", fontface = 2,size=4.5) +
	scale_x_reverse(name="-log([Cu'])",expand=c(0,0))+
	ylab(expression("Cu:C" ~(mu~mol:mol)))+
	scale_y_continuous(breaks = c(0,1.5,3,4.5,6,7.5,9,10.5,12),limits = c(0,12.6))+
	geom_hline(data = dat, aes(yintercept = 1.28),linetype="dashed",size=0.8)+
	theme_bw()+
	guides(color=guide_legend(nrow =2))+
	theme(panel.grid.major.x = element_blank(), 
				panel.grid.minor.x = element_blank(),
				panel.grid.major.y = element_blank(),
				panel.grid.minor.y = element_blank(),
				axis.title = element_text(size=14),
				axis.text = element_text(size=14),
				legend.text = element_text(size=10),
				legend.position = "bottom")

#------Saving-the-plot---------------------------------------------

plot <-plot_grid(phytos,bact,
								 labels = c("A", "B"),label_size = 12, align = "h")

save_plot(filename = "Fig5_bact.png", plot = bact, base_height = 4, base_width = 9.5 )

save_plot (filename="Fig5_Cu-quotas-vs-inorg-Cu-published.png", plot= plot, base_height= 4, base_width =9.5)
