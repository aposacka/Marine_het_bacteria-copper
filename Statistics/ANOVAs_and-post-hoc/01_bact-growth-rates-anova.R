#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~README~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script produces runs one way Analysis of Variance to test the effect of
# Cu level in growth media on growth rates of 4 strains of marine henetrotrophic bacteria
#- this data appears in Table 4 of Frontiers in Marine Science manuscript. 
# The results are written to a csv file

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
