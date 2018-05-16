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

#----------------------------------------
# ANOVA
#----------------------------------------

library(purrr)
library(broom)

# load the data
growth <-read_csv("Data/01_Bact-growth-rates-tidydata.csv")
growth$Strain <- as.factor(growth$Strain)


# nesting for iterations
data_nested <- growth %>%
  group_by(Strain) %>%
  nest()

# checking what the new dataset looks like
# by examining the data in the first grouped element
# of the df
# data_nested[[1,"data"]]


# create a function for ANOVA analysis and Bartlett test for 
# equal variances in order to assess if ANOVA assumptions are held

ANOVA <- function(df) {
  aov(mu_day~Cu_total, data=df)
}

Bartlett <- function(df) {
  bartlett.test(mu_day~Cu_total, data=df)
}

# Applying ANOVA and Bartlett functions to the nested data
data_nested <-data_nested %>%
  mutate(nested_ANOVA=map(data,ANOVA))

data_nested <-data_nested %>%
  mutate(nested_Bartlett=map(data,Bartlett))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extracting F and p values using BROOM
# tidy(data_nested$nested_ANOVA[[1:3]])

data_nested <- data_nested %>%
  mutate(tidy_ANOVA= map(nested_ANOVA,tidy))

data_nested <- data_nested %>%
  mutate(tidy_Bartlett= map(nested_Bartlett,tidy))

# Creating a statistics tibble
ANOVA_results <- data_nested %>%
  select(Strain, tidy_ANOVA) %>%
  unnest(tidy_ANOVA)

Bartlett_results <-data_nested %>%
  select(Strain, tidy_Bartlett) %>%
  unnest(tidy_Bartlett)

ANOVA_results

write_csv(ANOVA_results,"01_bact-growth-anova-bartlett-results.csv")


