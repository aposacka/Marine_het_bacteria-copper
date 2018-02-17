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
se <- function(x){sd(x)/sqrt(length(x))}

# setting the levels (order) of factors for plotting
data$Strain<- factor(data$Strain, levels = c("Dokdonia sp","R.pomeroyi",
																						 "Pseudoalteromonas sp (P2)",
																						 "Pseudoalteromonas sp (P26)"))

data$Cu_level <- factor(data$Cu_level, levels = c("0.6","2","10","25","50"))

data$Macronutrient <- factor(data$Macronutrient, 
														 levels = c("Carbon","Nitrogen","Phosphorous","Sulfur",
				 									 					 "C:N ratio", "S:P ratio"))
#~~~~~~~subsetting~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# subset each macronutrient
carb <- data%>%
	filter(Macronutrient=="Carbon")
nitrogen <- data%>%
	filter(Macronutrient=="Nitrogen")
phosph <- data%>%
	filter(Macronutrient=="Phosphorous")
sulf <- data%>%
	filter(Macronutrient=="Sulfur")
CN <- data%>%
	filter(Macronutrient=="C:N ratio")
SP <- data%>%
	filter(Macronutrient=="S:P ratio")

#~~~~~~~plots~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p1 <- carb%>%
	group_by(Strain,Cu_level,Macronutrient)%>%
	summarise(mean_q=mean(Quota_num),sderr=se(Quota_num))%>%
	ggplot(aes(x=Cu_level,y=mean_q,group=1))+
	stat_summary(fun.y=mean, geom="point",size=4, pch=21,stroke=1.2)+
	geom_errorbar(aes(ymin=mean_q-sderr, ymax=mean_q+sderr),width=.2,size=0.7)+
	geom_point(data=carb,aes(x=Cu_level,y=Quota_num),alpha=1/4,size=3,color="#666666")+
	facet_wrap(~Strain, scales = "free_y", nrow=1)+
	ylab(expression(atop("C quota", 
											 paste(~(fmol~C~cell^{-1})))))+
	annotate("text", x = 2.5, y = 40, 
					 label= c("Dokd-P16","R. pomeroyi",
					 				 "PAlt-P2","PAlt-P26"),size=3.5) +
	ylim(3,40)+
	theme(legend.position = "none",
				strip.text= element_blank(),
				panel.grid.minor.x = element_blank(),
				panel.grid.major.x = element_blank(),
				axis.title.x = element_blank(),
				axis.text = element_text(size=14),
				axis.title = element_text(size=11),
				legend.text = element_text(size=14))


p2 <- nitrogen%>%
	group_by(Strain,Cu_level,Macronutrient)%>%
	summarise(mean_q=mean(Quota_num),sderr=se(Quota_num))%>%
	ggplot(aes(x=Cu_level,y=mean_q,group=1))+
	stat_summary(fun.y=mean, geom="point",size=4, pch=21,stroke=1.2)+
	geom_errorbar(aes(ymin=mean_q-sderr, ymax=mean_q+sderr),width=.2,size=0.7)+
	geom_point(data=nitrogen,aes(x=Cu_level,y=Quota_num),alpha=1/3,size=3,color="#666666")+
	facet_wrap(~Strain, scales = "free_y", nrow=1)+
	ylab(expression(atop("N quota", 
											 paste(~(fmol~N~cell^{-1})))))+
	annotate("text", x = 2.5, y = 12.5, 
					 label= c("Dokd-P16","R. pomeroyi",
					 				 "PAlt-P2","PAlt-P26"),size=3.5) +
	ylim(0,12.5)+
	xlab(expression("Cu"~(nmol~L^{-1})))+
	theme(legend.position = "none",
				strip.text= element_blank(),
				panel.grid.minor.x = element_blank(),
				panel.grid.major.x = element_blank(),
				axis.title.x = element_blank(),
				axis.text = element_text(size=14),
				axis.title = element_text(size=11),
				legend.text = element_text(size=14))

p3 <- phosph%>%
	group_by(Strain,Cu_level,Macronutrient)%>%
	summarise(mean_q=mean(Quota_num),sderr=se(Quota_num))%>%
	ggplot(aes(x=Cu_level,y=mean_q,group=1))+
	stat_summary(fun.y=mean, geom="point",size=4, pch=21,stroke=1.2)+
	geom_errorbar(aes(ymin=mean_q-sderr, ymax=mean_q+sderr),width=.2,size=0.7)+
	geom_point(data=phosph,aes(x=Cu_level,y=Quota_num),alpha=1/3,size=3,color="#666666")+
	facet_wrap(~Strain, scales = "free_y", nrow=1)+
	ylab(expression(atop("P quota", 
											 paste(~(fmol~P~cell^{-1})))))+
	annotate("text", x = 2.5, y = 0.8, 
					 label= c("Dokd-P16","R. pomeroyi",
					 				 "PAlt-P2","PAlt-P26"),size=3.5) +
	ylim(0,0.8)+
	theme(legend.position = "none",
				strip.text= element_blank(),
				panel.grid.minor.x = element_blank(),
				panel.grid.major.x = element_blank(),
				axis.title.x = element_blank(),
				axis.text = element_text(size=14),
				axis.title = element_text(size=11),
				legend.text = element_text(size=14))

p4 <- sulf%>%
	group_by(Strain,Cu_level,Macronutrient)%>%
	summarise(mean_q=mean(Quota_num),sderr=se(Quota_num))%>%
	ggplot(aes(x=Cu_level,y=mean_q,group=1))+
	stat_summary(fun.y=mean, geom="point",size=4, pch=21,stroke=1.2)+
	geom_errorbar(aes(ymin=mean_q-sderr, ymax=mean_q+sderr),width=.2,size=0.7)+
	geom_point(data=sulf,aes(x=Cu_level,y=Quota_num),alpha=1/3,size=3,color="#666666")+
	facet_wrap(~Strain, scales = "free_y", nrow=1)+
	ylab(expression(atop("S quota", 
											 paste(~(fmol~S~cell^{-1})))))+
	annotate("text", x = 2.5, y = 0.6, 
					 label= c("Dokd-P16","R. pomeroyi",
					 				 "PAlt-P2","PAlt-P26"),size=3.5) +
	ylim(0,0.6)+
	theme(legend.position = "none",
				strip.text= element_blank(),
				panel.grid.minor.x = element_blank(),
				panel.grid.major.x = element_blank(),
				axis.title.x = element_blank(),
				axis.text = element_text(size=14),
				axis.title = element_text(size=11),
				legend.text = element_text(size=14))

p5 <-CN%>%
	group_by(Strain,Cu_level,Macronutrient)%>%
	summarise(mean_q=mean(Quota_num),sderr=se(Quota_num))%>%
	ggplot(aes(x=Cu_level,y=mean_q,group=1))+
	stat_summary(fun.y=mean, geom="point",size=4, pch=21,stroke=1.2)+
	geom_errorbar(aes(ymin=mean_q-sderr, ymax=mean_q+sderr),width=.2,size=0.7)+
	geom_point(data=CN,aes(x=Cu_level,y=Quota_num),alpha=1/3,size=3,color="#666666")+
	facet_wrap(~Strain, scales = "free_y", nrow=1)+
	ylab(expression(atop("C:N ratio", 
											 paste("(mol:mol)"))))+
	annotate("text", x = 2.5, y = 8, 
					 label= c("Dokd-P16","R. pomeroyi",
					 				 "PAlt-P2","PAlt-P26"),size=3.5) +
	geom_hline(yintercept=4.4,linetype = "3313",size=1)+
	ylim(2,8)+
	theme(legend.position = "none",
				strip.text= element_blank(),
				panel.grid.minor.x = element_blank(),
				panel.grid.major.x = element_blank(),
				axis.title.x = element_blank(),
				axis.text = element_text(size=14),
				axis.title = element_text(size=11),
				legend.text = element_text(size=14))

p6 <-SP%>%
	group_by(Strain,Cu_level,Macronutrient)%>%
	summarise(mean_q=mean(Quota_num),sderr=se(Quota_num))%>%
	ggplot(aes(x=Cu_level,y=mean_q,group=1))+
	stat_summary(fun.y=mean, geom="point",size=4, pch=21,stroke=1.2)+
	geom_errorbar(aes(ymin=mean_q-sderr, ymax=mean_q+sderr),width=.2,size=0.7)+
	geom_point(data=SP,aes(x=Cu_level,y=Quota_num),alpha=1/3,size=3,color="#666666")+
	facet_wrap(~Strain, scales = "free_y", nrow=1)+
	ylab(expression(atop("S:P ratio", 
											 paste("(mol:mol)"))))+
	annotate("text", x = 2.5, y = 2.2, 
					 label= c("Dokd-P16","R. pomeroyi",
					 				 "PAlt-P2","PAlt-P26"),size=3.5) +
	geom_hline(yintercept=0.86,linetype = "3313",size=1)+
#	xlab(expression("Cu"~(nmol~L^{-1})))+
	ylim(0.2,2.2)+
	theme(legend.position = "none",
				strip.text= element_blank(),
				axis.title.x = element_blank(),
				panel.grid.minor.x = element_blank(),
				panel.grid.major.x = element_blank(),
				axis.text = element_text(size=14),
				axis.title = element_text(size=11),
				legend.text = element_text(size=14))

#~~~~~arrange+save~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`

plot <-plot_grid(p1,p2,p5,p3,p4,p6,align = "v",nrow=6)

save_plot (filename="CNSP.tiff", plot= plot, base_height= 12, base_width = 10)


