#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~README~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script produces summary statistics (mean, stdev, stderror and number of 
# observations) for growth rates at various Cu levels 
# in 4 strains of marine henetrotrophic bacteria - this data appears in 
# in Fig.1 and Table 1 of Frontiers in Marine Science manuscript. 
# The results are written to a csv file


library(tidyverse)

# load the data
growth <-read_csv("Data/01_Bact-growth-rates-tidydata.csv")

growth$Strain <- as.factor(growth$Strain)
growth <- na.omit(growth)

# calculating summary statistis using commands from the dplyr package
Descr_stats <-growth %>%
	group_by(Strain,Cu_total)%>%
	summarise(mean_gr =mean(mu_day, na.rm= TRUE), 
						stdev=sd(mu_day, na.rm= TRUE),
						SEM= stdev/sqrt(n()),
						(n()))

# arranging the data in the order of Strain and Cu level
Descr_stats <- Descr_stats %>%
	arrange(Strain,Cu_total)

write_csv(Descr_stats,"01_bact-growth-descr-stats.csv")


