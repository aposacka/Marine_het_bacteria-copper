#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~README~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script produces summary statistics (mean, stdev, stderror and number of 
# observations) for cell normalized macronutrients (fmol cell-1) and their molar ratios
# (mol:mol) at various Cu levels in 4 strains of marine henetrotrophic bacteria - 
# this data appears in in Table 2 of Frontiers in Marine Science supplement 
# The results are written to a csv file

library(tidyverse)

data <- read.csv("Data/02_Bact-CNSP-tidydata.csv")
glimpse(data)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

head(data <- na.omit(data))

Descr_stats <-data %>%
	select(Strain,Macronutrient,Quota_fmol_cell,Cu_level)%>%
	group_by(Macronutrient,Strain,Cu_level)%>%
	summarise(mean_quota =mean(Quota_fmol_cell, na.rm= TRUE), 
						stdev=sd(Quota_fmol_cell, na.rm= TRUE),
						SEM= stdev/sqrt(n()),
						(n()))

Descr_stats <- Descr_stats %>%
	arrange(Strain,Macronutrient)

write_csv(Descr_stats,"02_bact-macronutrients-summary-stats.csv")
