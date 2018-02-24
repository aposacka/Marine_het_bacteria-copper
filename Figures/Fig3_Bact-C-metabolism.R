#~~~~~~~~~~~~~~~~~~~~~~~~~~~~README~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script for producing figures of Bacterial Carbon Metabolism 
# (Respiration, Carbon demand, growth efficiency)
# 

# loading packages
 library(tidyverse)
 library(cowplot)

# loading data
 data <-read_csv("Bact_Growth-Efficiency/Bact_carb-metab-with-errors-final.csv")
 
# convert some variables to factors
 data$Cu_level <- as.factor(data$Cu_level)
 data$Strain<- as.factor(data$Strain)


# setting the order of factors to plot
 data$Strain<- factor(data$Strain, levels = c("Dokd_P16",
																						 "R.pomeroyi",
																						 "PAlt_P26"))


#~~~~~~~PLOTS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# plotting cellular resp. per day for each strain in a facet, this is top panel
 
 p1 <-data%>%
  	group_by(Cu_level, Strain)%>%
	  summarize(mean_BR=mean(BR_cell_day),std=sd(BR_cell_day),prop_err=std/sqrt(n()))%>%
	    ggplot(aes(x=Cu_level,y=mean_BR,group=1))+
	    geom_line(linetype="dashed")+
	    stat_summary(fun.y=mean, geom="point",size=4, pch=21,stroke=1.5)+
	    geom_errorbar(aes(ymin=mean_BR-prop_err, ymax=mean_BR+prop_err), width=.3)+
	    geom_point(data=data,aes(x=Cu_level,y=BR_cell_day),alpha=1/3,size=3,color="#666666")+
	    facet_wrap(~Strain, scales="free_y")+
	    ylab(expression("BR"[cell]~(fmol~O[2]~cell^{-1})))+
	    guides(colour=FALSE)+
	    theme(strip.text.x = element_blank(),
		        axis.title.x = element_blank(),
		        axis.text = element_text(size=15),
		        axis.title.y= element_text(size=13))


# plotting Bact. productivity for each strain

 p2 <- data %>%
    	group_by(Cu_level, Strain)%>%
	    summarize(mean_BPy=mean(BP), std=sd(BP),prop_err=std/sqrt(n()))%>%
	       ggplot(aes(x=Cu_level,y=mean_BPy,group=1))+
	       geom_line(linetype="dashed")+
	       stat_summary(fun.y=mean, geom="point",size=4,pch=21,stroke=1.5)+
	       geom_errorbar(aes(ymin=mean_BPy-prop_err, ymax=mean_BPy+prop_err), width=.3)+
	       geom_point(data=data,aes(x=Cu_level,y=BP),alpha=1/3,size=3,color="#666666")+
	       ylab(expression("BP" ~ (fmol~ C ~ d^{-1})))+
	       facet_wrap(~Strain, scales="free_y")+
      	 guides(colour=FALSE)+
      	 theme( strip.text.x = element_blank(),
		        		axis.title.x = element_blank(),
		        		axis.text = element_text(size=15),
		        		axis.title.y = element_text(size=13))


# plotting Bact. Carbon Demand for each strain
  p3 <- data %>%
	      group_by(Cu_level, Strain)%>%
	      summarize(mean_BCD=mean(BCD),prop_err=sqrt(sum(BCD_error)^2)/length(BCD_error))%>%
	         ggplot(aes(x=Cu_level,y=mean_BCD, group=1))+
	         geom_line(linetype="dashed")+
	         stat_summary(fun.y=mean, geom="point",size=4,pch=21,stroke=1.5)+
	         geom_errorbar(aes(ymin=mean_BCD-prop_err, ymax=mean_BCD+prop_err), width=.3)+
	         geom_point(data=data,aes(x=Cu_level,y=BCD),alpha=1/3,size=3,color="#666666")+
	         facet_wrap(~Strain, scales="free_y")+
	         ylab(expression("BCD" ~ (fmol~ C ~ d^{-1})))+
	         guides(colour=FALSE)+
	         theme(strip.text.x = element_blank(),
				         axis.title.x = element_blank(),
				         axis.text = element_text(size=15),
         		     axis.title.y = element_text(size=13))

# plotting Bact.Growth Efficiency for each strain

 p4 <- data %>%
     	 group_by(Cu_level, Strain)%>%
	     summarize(mean_BGE=mean(BGE),prop_err=sqrt(sum(BGE_error)^2)/length(BGE_error))%>%
	        ggplot(aes(x=Cu_level,y=mean_BGE, group=1))+
	        geom_line(linetype="dashed")+
	        stat_summary(fun.y=mean, geom="point",size=4,pch=21,stroke=1.5)+
	        geom_errorbar(aes(ymin=mean_BGE-prop_err, ymax=mean_BGE+prop_err), width=.3)+
	        geom_point(data=data,aes(x=Cu_level,y=BGE),alpha=1/3,size=3,color="#666666")+
        	facet_wrap(~Strain, scales="free_y")+
          ylim(0.2,0.7)+
	        guides(colour=FALSE)+
	        ylab("BGE")+
	        theme( strip.text = element_blank(),
	               axis.title.x = element_blank(),
				         axis.text = element_text(size=15),
				         axis.title.y = element_text(size=13))

# arranging plots for the final figure
 plot <-plot_grid(p1,p2,p3,p4, align = "v",nrow=4)
 
# saving as tiff
 save_plot (filename="Carb_metab.tiff", plot= plot, base_height= 9, base_width = 9)
