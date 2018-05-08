#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~README~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script produces summary statistics (mean, stdev, stderror and number of 
# observations) for cell normalized macronutrients (fmol cell-1) and their molar ratios
# (mol:mol) at various Cu levels in 4 strains of marine henetrotrophic bacteria - 
# this data appears in in Table 2 of Frontiers in Marine Science supplement 
# The results are written to a csv file

library(tidyverse)

data <- read_csv("Data/02_Bact-CNSP-tidydata.csv")

data$Cu_level <- as.factor(data$Cu_level)
data$Strain<- as.factor(data$Strain)
data$Macronutrient <- as.factor(data$Macronutrient)
glimpse(data)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Descr_stats <-data %>%
	select(Strain,Macronutrient,Quota_num,Cu_level)%>%
	group_by(Macronutrient,Strain,Cu_level)%>%
	summarise(mean_quota =mean(Quota_num, na.rm= TRUE), 
						stdev=sd(Quota_num, na.rm= TRUE),
						SEM= stdev/sqrt(n()),
						(n()))

Descr_stats <- Descr_stats %>%
	arrange(Strain,Macronutrient)

write_csv(Descr_stats,"02_bact-macronutrients-summary-stats.csv")
