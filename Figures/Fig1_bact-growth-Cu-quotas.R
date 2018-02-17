# Script to reproduce Fig.1 in Frontiers in Marine Science, Posacka et al.
# Fig.1 

# REUSE THE BELOW SCRIPT FROM AGU2018 poster


ibrary(tidyverse)
#library(cowplot)

# loading data
data <-read_csv("Plots_metallome/Bact_TMS-Pnorm-tidy.csv") 

# function for standard error
se <- function(x){sd(x)/sqrt(length(x))}

# changing the name of some variables and converting them to factors
data$Strain<-factor(data$Strain, levels = c("Dokd_P16","Rugeria", "PAlt_P2","PAlt-P26"))
data$Cu_level <- factor(data$Cu_level, levels = c("0.6","2","10","25","50"))
data$Metal <- factor(data$Metal,levels = c("Fe_P","Zn_P","Cu_P","Mn_P", "Co_P"))

growth <-read_csv("C:/Users/Ania/Bact_Stoichiometry/Plots_copper/GR_rates-bact-figs.csv")
growth$Strain<- as.factor(growth$Strain)
growth$Cu_total <- as.factor(growth$Cu_total)
#,levels = c("0.6","2","10","25","50"))

subset <- growth%>%
  filter(Strain %in% c("Dokdonia sp","Pseudoalteromonas(P2)" ,
                       "Pseudoalteromonas(P26)","R.pomeroyi"))

subset$Strain<-factor(subset$Strain, levels = c("Dokdonia sp","R.pomeroyi", 
                                                "Pseudoalteromonas(P2)", "Pseudoalteromonas(P26)"))

#~~~~~~~~~~~~~~~~~~~~~~~subsetting~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

iron <-data%>%
  filter(Metal=="Fe_P")
zinc <-data%>%
  filter(Metal=="Zn_P")
copper<-data%>%
  filter(Metal=="Cu_P")
manganese <-data%>%
  filter(Metal=="Mn_P")
cobalt <-data%>%
  filter(Metal=="Co_P")

#~~~~~~~~~~~~~~~~~~~~plots~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p1 <-iron%>%
  group_by(Strain,Cu_level,Metal)%>%
  summarise(mean_q=mean(Quota))%>%
  ggplot(aes(x = Cu_level,y = mean_q,group = 1))+
  geom_point(data=iron,aes(x = Cu_level,y = Quota, color=Strain), size = 2, alpha = 1/1.5)+
  scale_color_manual(values=c("#3333FF", "#E7B800", "#FC4E07","#666666"))+
  stat_summary(fun.y=mean, geom="point",size = 1.5, pch = 21)+
  facet_wrap(~Strain, scales = "free_y",nrow = 1)+
  geom_line(linetype="dashed")+
  #	ylab(expression(atop("Fe quota", 
  #					 paste("(mmol Fe:mol P)"))))+
  ylim(0,7)+
  theme(legend.position = "none",
        strip.text= element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size=6),
        axis.title = element_text(size=4))

p2 <-zinc%>%
  group_by(Strain,Cu_level,Metal)%>%
  summarise(mean_q=mean(Quota))%>%
  ggplot(aes(x=Cu_level,y=mean_q,group=1))+
  geom_point(data=zinc,aes(x=Cu_level,y = Quota, color = Strain),alpha = 1/1.5,size = 2)+
  stat_summary(fun.y=mean, geom="point",size=1.5, pch=21)+
  facet_wrap(~Strain, scales = "free_y",nrow=1)+
  geom_line(linetype="dashed")+
  #	ylab(expression(atop("Zn quota", 
  #					 paste("(mmol Zn:mol P)"))))+
  #	ylim(0,6.5)+
  theme(legend.position = "none",
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size=6),
        axis.title = element_text(size=4))

p3 <-copper%>%
  group_by(Strain,Cu_level,Metal)%>%
  summarise(mean_q=mean(Quota))%>%
  ggplot(aes(x=Cu_level,y=mean_q,group=1))+
  geom_point(data=copper,aes(x=Cu_level,y=Quota, color=Strain),alpha=1/1.5,size=2)+
  stat_summary(fun.y=mean, geom="point",size=1.5, pch=21)+
  facet_wrap(~Strain, scales = "free_y",nrow=1)+
  geom_line(linetype="dashed")+
  #	ylab(expression(atop("Cu quota", 
  #					 paste("(mmol Cu:mol P)"))))+
  ylim(0,0.22)+
  theme(legend.position = "none",
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size=6),
        axis.title = element_text(size=4))


p4 <-manganese%>%
  group_by(Strain,Cu_level,Metal)%>%
  summarise(mean_q=mean(Quota))%>%
  ggplot(aes(x=Cu_level,y=mean_q,group=1))+
  geom_point(data=manganese,aes(x=Cu_level,y=Quota, color=Strain),alpha = 1/1.5,size = 2)+
  stat_summary(fun.y=mean, geom="point",size = 1.5, pch=21)+
  facet_wrap(~Strain, scales = "free_y",nrow=1)+
  geom_line(linetype="dashed")+
  #	ylab(expression(atop("Mn quota", 
  #					 paste("(mmol Mn:mol P)"))))+
  #	ylim(0,1.23)+
  theme(legend.position = "none",
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size=6),
        axis.title = element_text(size=4))

p5 <-cobalt%>%
  group_by(Strain,Cu_level,Metal)%>%
  summarise(mean_q=mean(Quota))%>%
  ggplot(aes(x=Cu_level,y=mean_q,group=1))+
  geom_point(data=cobalt,aes(x=Cu_level,y=Quota, color=Strain),alpha= 1/1.5, size=2)+
  scale_color_manual(values=c("#3333FF", "#E7B800", "#FC4E07","#666666"))+
  stat_summary(fun.y=mean, geom="point",size=1.5, pch=21)+
  facet_wrap(~Strain, scales = "free_y",nrow=1)+
  geom_line(linetype="dashed")+
  #	ylab(expression(atop("Co quota", 
  #					 paste("(mmol Co:mol P)"))))+
  #	ylim(0,0.06)+
  theme(legend.position = "none",
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size=6),
        axis.title = element_text(size=4))

p5
#~~~~~~~~~~~~~~~save plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(cowplot)
plot <-plot_grid(p6,p1,p2,p3,p5,p4,align = "v",nrow=6)

save_plot (filename="Cu_effects-metallome.tiff", plot= plot, base_height= 6, base_width=5)

#--------------GROWTH RATES-----------------------------------


p6 <- subset%>%
  group_by(Strain,Cu_total)%>%
  summarise(mean_q=mean(mu_day))%>%
  ggplot(aes(x = Cu_total,y = mean_q,group = 1))+
  geom_point(data=subset,aes(x = Cu_total,y = mu_day, color = Strain), size = 2, alpha = 1/1.5)+
  scale_color_manual(values=c("#F8766D", "#00BA38", "#619CFF","#666666"))+
  stat_summary(fun.y=mean, geom="point",size = 1.5, pch = 21)+
  facet_wrap(~Strain, scales = "free_y",nrow = 1)+
  geom_line(linetype="dashed")+
  ylim(1,23)+
  theme(legend.position = "none",
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size=6),
        axis.title = element_text(size=4))