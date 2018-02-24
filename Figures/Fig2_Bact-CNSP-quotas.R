#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`README~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
# Script for producing FiG 2: cellular C-N-P-S of bacteria and N:C, S:P ratios

library(tidyverse)
library(cowplot)

data <- read_csv("C:/Users/Ania/Bact_Stoichiometry/Plots_macronutrients/Bact_C-N-S-P-data-tidy.csv")

data$Cu_level <- as.factor(data$Cu_level)
data$Strain <- as.factor(data$Strain)
data$Macronutrient <-as.factor(data$Macronutrient)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# function to calculate standard error
# se <- function(x){sd(x)/sqrt(length(x))}

# setting the levels (order) of factors for plotting
data$Strain<- factor(data$Strain, levels = c("Dokdonia sp","R.pomeroyi",
																						 "Pseudoalteromonas sp (P2)",
																						 "Pseudoalteromonas sp (P26)"))

data$Cu_level <- factor(data$Cu_level, levels = c("0.6","2","10","25","50"))

data$Macronutrient <- factor(data$Macronutrient, 
														 levels = c("Carbon","Nitrogen","Phosphorous","Sulfur",
				 									 					 "C:N ratio", "S:P ratio"))

#------------------------------------------------------------------------------------------
# Carbon quotas
#------------------------------------------------------------------------------------------

carb <- data%>%
  filter(Macronutrient == "Carbon")

p1 <- carb%>%
	group_by(Strain,Cu_level,Macronutrient)%>%
	summarise(mean_q = mean(Quota_num))%>%
	ggplot(aes(x = Cu_level,y = mean_q,color = Strain, group=1))+
	stat_summary(fun.y = mean, geom = "point", pch = "_", size = 7)+
	geom_point(data = carb,aes(x = Cu_level,y = Quota_num, color = Strain),alpha=1/3,size=3)+
  scale_color_manual(values=c("#009E73","#666666", "#E69F00","#0072B2"))+
	facet_wrap(~Strain, scales = "free_y", nrow=1)+
	ylab(expression(atop("C quota", 
											 paste(~(fmol~C~cell^{-1})))))+
#	annotate("text", x = 2.5, y = 40, 
#					 label= c("Dokd-P16","R. pomeroyi",
#					 				 "PAlt-P2","PAlt-P26"),size=3.5) +
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


#--------------------------------------------------------------------------------------------
# Nitrogen
#--------------------------------------------------------------------------------------------

nitrogen <- data%>%
  filter(Macronutrient=="Nitrogen")

p2 <- nitrogen%>%
	group_by(Strain,Cu_level,Macronutrient)%>%
	summarise(mean_q=mean(Quota_num))%>%
	ggplot(aes(x = Cu_level,y = mean_q, color = Strain, group = 1))+
	stat_summary(fun.y = mean, geom = "point",pch = "_", size = 7)+
  scale_color_manual(values=c("#009E73","#666666","#E69F00","#0072B2"))+
	geom_point(data=nitrogen,aes(x = Cu_level,y = Quota_num, color = Strain),alpha=1/3,size=3)+
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

#---------------------------------------------------------------------------------------------
# Phosphorous quotas
#---------------------------------------------------------------------------------------------

phosph <- data%>%
  filter(Macronutrient=="Phosphorous")

p3 <- phosph%>%
	group_by(Strain,Cu_level,Macronutrient)%>%
	summarise(mean_q=mean(Quota_num))%>%
  ggplot(aes(x=Cu_level,y=mean_q, color = Strain, group = 1))+
  stat_summary(fun.y=mean, geom="point",pch = "_", size = 7)+
  scale_color_manual(values=c("#009E73","#666666", "#E69F00","#0072B2"))+
  geom_point(data = phosph,aes(x = Cu_level,y = Quota_num, color = Strain),alpha=1/3,size=3)+
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

#------------------------------------------------------------------------------------------
# Sulfur
#------------------------------------------------------------------------------------------

sulf <- data%>%
  filter(Macronutrient=="Sulfur")

p4 <- sulf%>%
	group_by(Strain,Cu_level,Macronutrient)%>%
  summarise(mean_q=mean(Quota_num))%>%
  ggplot(aes(x=Cu_level,y=mean_q, color = Strain, group = 1))+
  stat_summary(fun.y=mean, geom="point", pch = "_", size = 7)+
  scale_color_manual(values=c("#009E73", "#666666", "#E69F00","#0072B2"))+
  geom_point(data = sulf,aes(x = Cu_level,y = Quota_num, color = Strain),alpha=1/3,size=3)+
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

CN <- data%>%
  filter(Macronutrient == "C:N ratio")

p5 <-CN%>%
	group_by(Strain,Cu_level,Macronutrient)%>%
  summarise(mean_q=mean(Quota_num))%>%
  ggplot(aes(x=Cu_level,y=mean_q, color = Strain, group = 1))+
  stat_summary(fun.y=mean, geom="point",pch = "_",size = 7)+
  scale_color_manual(values=c("#009E73", "#666666", "#E69F00","#0072B2"))+
  geom_point(data = CN,aes(x = Cu_level,y = Quota_num),alpha=1/3,size=3)+
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

SP <- data%>%
  filter(Macronutrient=="S:P ratio")

p6 <-SP%>%
	group_by(Strain,Cu_level,Macronutrient)%>%
  summarise(mean_q=mean(Quota_num))%>%
  ggplot(aes(x=Cu_level,y=mean_q, group=1, color = Strain))+
  stat_summary(fun.y=mean, geom="point",pch = "_", size = 7)+
  scale_color_manual(values=c("#009E73", "#666666", "#E69F00","#0072B2"))+
  geom_point(data = SP,aes(x = Cu_level,y = Quota_num),alpha=1/3,size=3)+
  facet_wrap(~Strain, scales = "free_y", nrow=1)+
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

#~~~~~arrange+save~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`

plot <-plot_grid(p1,p2,p3,p4,p5,p6,align = "v",nrow=6)

save_plot (filename="Fig.2_CNSP.tiff", plot= plot, base_height= 9, base_width = 8)


