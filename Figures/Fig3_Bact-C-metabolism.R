#~~~~~~~~~~~~~~~~~~~~~~~~~~~~README~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script for producing figures of Bacterial Carbon Metabolism 
# (Respiration, Carbon demand, growth efficiency)
# 

# loading packages
 library(tidyverse)
 library(cowplot)

# loading data
 data <-read_csv("Data/03_Bact-carb-metab-tidydata.csv")
 
# convert some variables to factors
data$Cu_level <- as.factor(data$Cu_level)
data$Strain<- as.factor(data$Strain)
data$Category <- as.factor(data$Category)

glimpse(data)
 
# setting the order of factors for plotting
data$Strain<- factor(data$Strain, levels = c("Dokd_P16",
																						 "R.pomeroyi",
																						 "PAlt_P26"))
 
data$Strain<- factor(data$Category, levels = c("BR_per_day",
                                              "BP",
                                              "BCD",
                                              "BGE"))
 
levels(data$Category)
levels(data$Strain)

#-------------------
# Plot

 p1 <-data%>%
      filter(Category == "BR_cell_day")%>%
    	group_by(Cu_level, Strain)%>%
	    ggplot(aes(x = Cu_level,y = Value, color = Strain))+
      stat_summary(fun.y = mean, geom = "point", pch = "_", size = 7)+
	    geom_point(alpha = 1/3,size = 3)+
	    facet_wrap(~Strain, scales = "free_y")+
      scale_color_manual(values = c("#009E73","#666666","#0072B2"))+
     # annotate ("rect", limits = c(min_xy, max_xy), alpha = .3, fill = "gray")+
    #  annotate("rect", xmin=1.8, xmax=3.3, ymin=0.0, ymax=600, alpha=.3, fill="gray")+
    #  scale_y_continuous(expand=c(0,0))+
	   ylab(expression("BR"[cell]~(fmol~O[2]~cell^{-1})))+
	    guides(colour = FALSE)+
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
	   

p2 <-data%>%
  filter(Category == "BP")%>%
  group_by(Cu_level, Strain)%>%
  ggplot(aes(x = Cu_level,y = Value, color = Strain))+
  stat_summary(fun.y = mean, geom = "point", pch = "_", size = 7)+
  geom_point(alpha = 1/3,size = 3)+
  facet_wrap(~Strain, scales = "free_y")+
  scale_color_manual(values = c("#009E73","#666666","#0072B2"))+
  # annotate ("rect", limits = c(min_xy, max_xy), alpha = .3, fill = "gray")+
  #  annotate("rect", xmin=1.8, xmax=3.3, ymin=0.0, ymax=600, alpha=.3, fill="gray")+
  #  scale_y_continuous(expand=c(0,0))+
  ylab(expression("BP" ~ (fmol~ C ~ d^{-1})))+
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

p3 <-data%>%
  filter(Category == "BCD")%>%
  group_by(Cu_level, Strain)%>%
  ggplot(aes(x = Cu_level,y = Value, color = Strain))+
  stat_summary(fun.y = mean, geom = "point", pch = "_", size = 7)+
  geom_point(alpha = 1/3,size = 3)+
  facet_wrap(~Strain, scales = "free_y")+
  scale_color_manual(values = c("#009E73","#666666","#0072B2"))+
  # annotate ("rect", limits = c(min_xy, max_xy), alpha = .3, fill = "gray")+
  #  annotate("rect", xmin=1.8, xmax=3.3, ymin=0.0, ymax=600, alpha=.3, fill="gray")+
  #  scale_y_continuous(expand=c(0,0))+
  ylab(expression("BCD" ~ (fmol~ C ~ d^{-1})))+
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

p4 <- data%>%
  filter(Category == "BGE")%>%
  group_by(Cu_level, Strain)%>%
  ggplot(aes(x = Cu_level,y = Value, color = Strain))+
  stat_summary(fun.y = mean, geom = "point", pch = "_", size = 7)+
  geom_point(alpha = 1/3,size = 3)+
  facet_wrap(~Strain, scales = "free_y")+
  scale_color_manual(values = c("#009E73","#666666","#0072B2"))+
  # annotate ("rect", limits = c(min_xy, max_xy), alpha = .3, fill = "gray")+
  #  annotate("rect", xmin=1.8, xmax=3.3, ymin=0.0, ymax=600, alpha=.3, fill="gray")+
  #  scale_y_continuous(expand=c(0,0))+
  ylab("BEG")+
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
  
#--------------------------------------------------
# arranging plots for the final figure + saving,
# both commands are from the cowplot package

 plot <-plot_grid(p1,p2,p3,p4, align = "v",nrow=4)

 save_plot (filename="Fig3_Bact-carb-metab.tiff", plot= plot, base_height= 6.5, base_width = 6.5)
