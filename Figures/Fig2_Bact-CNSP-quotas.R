#~----------------------------------------------------------------------------
# Script for producing FiG 2: cellular C-N-P-S of bacteria and N:C, S:P ratios
#-----------------------------------------------------------------------------

library(tidyverse)
library(cowplot)

dat <- read.csv("Data/02_Bact-CNSP-tidydata.csv")

### NEED TO FIX Pseudoalteromonas name!!!!!!!!!!
glimpse(dat)
levels(dat$Strain)
levels(dat$Strain) <- gsub("Pseudoalteromonas sp (26)","Pseudoalt sp. (P26)", levels(dat$Strain))
write.csv(dat,"02_Bact-CNSP-tidydata.csv")

# setting the levels (order) of factors for plotting
dat$Strain<- factor(dat$Strain, levels = c("Dokdonia sp","R.pomeroyi",
																						 "Pseudoalteromonas sp (P2)",
																						 "Pseudoalteromonas sp (P26)"))

dat$Cu_level <- factor(dat$Cu_level, levels = c("0.6","2","10","25","50"))

dat$Macronutrient <- factor(dat$Macronutrient, 
														 levels = c("Carbon","Nitrogen","Phosphorous","Sulfur",
				 									 					 "C:N ratio", "S:P ratio"))

levels(dat$Strain)
#----------------------------
# Carbon quotas
#----------------------------
levels(dat$Macronutrient)

p1 <- dat%>%
  filter(Macronutrient == "Carbon")%>%
	group_by(Strain,Cu_level)%>%
	ggplot(aes(x = Cu_level,y = Quota_fmol_cell,color = Strain))+
	stat_summary(fun.y = mean, geom = "point", pch = "_", size = 7)+
	geom_point(alpha=1/3,size=3)+
  scale_color_manual(values=c("#009E73","#666666", "#E69F00","#0072B2"))+
	facet_wrap(~Strain, scales = "free_y", nrow=1)+
	ylab(expression(atop("C quota", 
											 paste(~(fmol~C~cell^{-1})))))+
	ylim(3,40)+
  theme_bw()+
	theme(legend.position = "none",
				strip.text= element_blank(),
				axis.title.x = element_blank(),
				panel.grid.major.x = element_blank(), 
				panel.grid.minor.x = element_blank(),
				panel.grid.major.y = element_blank(),
				panel.grid.minor.y = element_blank(),
				axis.text = element_text(size=11),
				axis.title.y = element_text(size=11),
				legend.text = element_text(size=11))

p1
#--------------------------
# Nitrogen
#--------------------------

p2 <- dat%>%
  filter(Macronutrient == "Nitrogen")%>%
	group_by(Strain,Cu_level)%>%
	ggplot(aes(x = Cu_level,y = Quota_fmol_cell, color = Strain, group = 1))+
	stat_summary(fun.y = mean, geom = "point",pch = "_", size = 7)+
	geom_point(alpha=1/3,size=3)+
  scale_color_manual(values=c("#009E73","#666666","#E69F00","#0072B2"))+
	facet_wrap(~Strain, scales = "free_y", nrow=1)+
	ylab(expression(atop("N quota", 
											 paste(~(fmol~N~cell^{-1})))))+
	ylim(0,12.5)+
  theme_bw()+
  theme(legend.position = "none",
        strip.text= element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size=11),
        axis.title.y = element_text(size=11),
        legend.text = element_text(size=11))

#------------------------
# Phosphorous quotas
#------------------------

p3 <- dat%>%
  filter(Macronutrient == "Phosphorous")%>%
	group_by(Strain,Cu_level)%>%
  ggplot(aes(x=Cu_level,y=Quota_fmol_cell, color = Strain))+
  stat_summary(fun.y=mean, geom="point",pch = "_", size = 7)+
  scale_color_manual(values=c("#009E73","#666666", "#E69F00","#0072B2"))+
  geom_point(alpha=1/3,size=3)+
	facet_wrap(~Strain, scales = "free_y", nrow=1)+
	ylab(expression(atop("P quota", 
											 paste(~(fmol~P~cell^{-1})))))+
	ylim(0,0.8)+
  theme_bw()+
  theme(legend.position = "none",
        strip.text= element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size=11),
        axis.title.y = element_text(size=11),
        legend.text = element_text(size=11))

#--------------------------
# Sulfur
#--------------------------

p4 <- dat%>%
  filter(Macronutrient == "Sulfur")%>%
	group_by(Strain,Cu_level)%>%
  ggplot(aes(x=Cu_level,y = Quota_fmol_cell, color = Strain, group = 1))+
  stat_summary(fun.y=mean, geom="point", pch = "_", size = 7)+
  geom_point(alpha=1/3,size=3)+
  scale_color_manual(values=c("#009E73", "#666666", "#E69F00","#0072B2"))+
	facet_wrap(~Strain, scales = "free_y", nrow=1)+
	ylab(expression(atop("S quota", 
											 paste(~(fmol~S~cell^{-1})))))+
	ylim(0,0.6)+
  theme_bw()+
  theme(legend.position = "none",
        strip.text= element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size=11),
        axis.title.y = element_text(size=11),
        legend.text = element_text(size=11))

#--------------------------------------------------------------------------------
# Carbon: nitrogen
#--------------------------------------------------------------------------------

p5 <- dat%>%
  filter(Macronutrient == "C:N ratio")%>%
	group_by(Strain,Cu_level)%>%
  ggplot(aes(x=Cu_level,y = Quota_fmol_cell, color = Strain, group = 1))+
  stat_summary(fun.y=mean, geom="point",pch = "_",size = 7)+
  scale_color_manual(values=c("#009E73", "#666666", "#E69F00","#0072B2"))+
  geom_point(alpha=1/3,size=3)+
	facet_wrap(~Strain, scales = "free_y", nrow=1)+
	ylab(expression(atop("C:N ratio", 
											 paste("(mol:mol)"))))+
	geom_hline(yintercept= 5.1,linetype = "3313",size=1)+
  annotate("rect", xmin=0.1, xmax=5.7, ymin=4.03, ymax=7.23, alpha=.3, fill="gray")+
	ylim(2,8)+
  theme_bw()+
  theme(legend.position = "none",
        strip.text= element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size=11),
        axis.title.y = element_text(size=11),
        legend.text = element_text(size=11))

#----------------------------------------------------------
# Sulfur:phosphorous
#-----------------------------------------------------------


p6 <- dat%>%
  filter(Macronutrient == "S:P ratio")%>%
	group_by(Strain,Cu_level)%>%
  ggplot(aes(x = Cu_level,y = Quota_fmol_cell, color = Strain))+
  stat_summary(fun.y = mean, geom = "point",pch = "_", size = 7)+
  geom_point(alpha=1/3,size=3)+
  scale_color_manual(values = c("#009E73", "#666666", "#E69F00","#0072B2"))+
  facet_wrap(~Strain, scales = "free_y", nrow=1)+
	ylab(expression(atop("S:P ratio", 
											 paste("(mol:mol)"))))+
	geom_hline(yintercept= 0.7,linetype = "3313",size=1)+
  annotate("rect", xmin=0.1, xmax=5.7, ymin=0.23, ymax=1.17, alpha=.3, fill="gray")+
	ylim(0.2,2.2)+
  theme_bw()+
  theme(legend.position = "none",
        strip.text= element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size=11),
        axis.title.y = element_text(size=11),
        legend.text = element_text(size=11))

#----------arrange+save----------------------------------

plot <-plot_grid(p1,p2,p3,p4,p5,p6,align = "v",nrow=6)

save_plot (filename="Fig.2_Bact-CNSP-quotas.tiff", plot= plot, base_height= 9, base_width = 8)


