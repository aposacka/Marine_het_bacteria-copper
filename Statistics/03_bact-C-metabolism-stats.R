#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~README~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script is for statistical analysis of bacterial C metabolism
# - summary statistics (mean, stdev, stderror and number of 
# observations) 
# - one-way ANOVA
# - Post-hoc analysis

library(tidyverse)
library(broom)

data <-read.csv("Data/03_Bact_carb-metab-data.csv")

# glimpse(data)

#-----------------------
# Summary statistics
#-----------------------

decr_stats <- data%>%
  group_by(Strain,Cu_level)%>%
  summarize(mean_BRcarb=mean(BR_carb_day),std=sd(BR_carb_day),BRcarb_error=std/sqrt(n()),
            mean_GR=mean(Growth_rate),std2=sd(Growth_rate),GR_error=std2/sqrt(n()),
            mean_BRcell=mean(BR_cell_day),std3=sd(BR_cell_day),BRcell_error=std3/sqrt(n()),
            mean_BP=mean(BP),std4=sd(BP),BP_stderr=std4/sqrt(n()),
            mean_BCD=mean(BCD),BCDprop_err=sqrt(sum(BCD_error)^2)/length(BCD_error),
            mean_BGE=mean(BGE),BGEprop_err=sqrt(sum(BGE_error)^2)/length(BGE_error))


#save to csv
write_csv(decr_stats,"03_Bact-C-metabolism-descr-stats.csv")

#-----------------
# ANOVA analysis
#-----------------

# re-shape the data
stat_dat <- data %>%
  select(Strain, Cu_level,BR_cell_day,BR_carb_day,BP,BCD,BGE)

stat_dat <- stat_dat%>%
  gather(Metabolism,Rate,BR_cell_day:BGE)

# convert to factors
stat_dat$Cu_level <- as.factor(stat_dat$Cu_level)
stat_dat$Strain<- as.factor(stat_dat$Strain)
stat_dat$Metabolism<- as.factor(stat_dat$Metabolism)

#stat_dat%>% aov(Rate~Cu_level,data=stat_dat)

#-------------------------
# ANOVA and Bartlett tests
#-------------------------

# creating a nested dataframe for mutliple ANOVA analysis
# Grouping by Strain & metabolic rate
# As such ANOVA will be performed on each strain seperately.
# For each strain metabolic rate at different levels of
# Cu will be tested.

data_nested <- stat_dat %>%
  group_by(Strain,Metabolism) %>%
  nest()

ANOVA <- function(df) {
  aov(Rate~Cu_level, data=df)
}

Bartlett <- function(df) {
  bartlett.test(Rate~Cu_level, data=df)
}

data_nested <-data_nested %>%
  mutate(nested_ANOVA=map(data,ANOVA))

data_nested <-data_nested %>%
  mutate(nested_Bartlett=map(data,Bartlett))

#----------------------------------------
# Extracting F and p values using BROOM
# tidy(data_nested$nested_ANOVA[[1:3]])
#----------------------------------------

data_nested <- data_nested %>%
  mutate(tidy_ANOVA= map(nested_ANOVA,tidy))

data_nested <- data_nested %>%
  mutate(tidy_Bartlett= map(nested_Bartlett,tidy))

# Creating a statistics tibble
ANOVA_results <- data_nested %>%
  select(Strain,Metabolism, tidy_ANOVA) %>%
  unnest(tidy_ANOVA)

Bartlett_results <-data_nested %>%
  select(Strain,Metabolism, tidy_Bartlett) %>%
  unnest(tidy_Bartlett)

# saving data as csv
write_csv(ANOVA_results,"Carb_metab-ANOVA.csv")

#-----------------------------------------
# Post hod Bonferroni
#-----------------------------------------


data_nested <- stat_dat %>%
  group_by(Strain,Metabolism) %>%
  nest()

# apply nested pairwise t test

Bonferroni <- function(df) {
  pairwise.t.test(df$Rate,df$Cu_level,p.adj= "bonf",data=df)
}

data_nested<-data_nested %>%
  mutate(nested_Bonferroni=map(data,Bonferroni))

data_nested <- data_nested%>%
  mutate(tidy_Bonferroni= map(nested_Bonferroni,tidy))

Bonferroni_results <- data_nested%>%
  select(Strain,Metabolism,tidy_Bonferroni) %>%
  unnest(tidy_Bonferroni)

write_csv(Bonferroni_results,"Bact_carb-metab-post-hoc.csv")
write_csv(Bartlett_results,"Carb_metab-Bartlett.csv")