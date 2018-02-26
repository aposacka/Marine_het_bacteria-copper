#-----------------------------------
# Fig.4 Copper to carbon ratios
# of heteotrophic bacteria and
# eukaryotic algae
#-----------------------------------


library(tidyverse)
library(cowplot)
library(ggpubr)


dat <-read_csv("Data/05_Cu-quotas-lit-aquil-tidydata.csv") 

dat$Taxon<- as.factor(dat$Taxon)
dat$Species<- as.factor(dat$Species)
dat$Domain<- as.factor(dat$Domain)
dat$Strategy <- as.factor(dat$Strategy)

#levels(dat$Domain)
glimpse(dat)

p1 <- dat%>%
  filter(Domain %in% c("Euk_algae","Het_bacteria"))%>%
  ggboxplot(x= "Domain" ,y= "Cu_C" ,color= "Domain", palette = c("#00BA38", "#E7B800"), add = "jitter")+
  ylab("mol:mol")+
  stat_compare_means() 

p1
