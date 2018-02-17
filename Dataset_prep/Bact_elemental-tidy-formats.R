# ----
# This script extracts metals normalized to P from the
# Bact_TMS-normalized-final.csv, removes
# missing values and converts the dataset into a tidy 
# format for plotting and statistical tests
# ----


# load the data from csv, the function for this "read_csv" is in the tidyverse package
library(tidyverse)
data <-read_csv("C:/Users/Ania/R_coding/Marine_het_bacteria-copper/Datasets/Bact_TMS-normalized-final.csv")

# to check what's in this file run the glimpse()
glimpse(data)


# for what is to be done next Cu level and strain have to be in the 
# factor data type, but for me they are imported as a character and a double 

data$Cu_level <- as.factor(data$Cu_level)
data$Strain<- as.factor(data$Strain)


# extracting metals normalized to phosphorous only, this is used for Fig.1 (Cu quotas)
# and for a seperate manuscript on extended elemental stoichiometry of bacteria and 
# phytoplankton

Phosph_metals <-data%>%
  select(Strain,Cu_level,Fe_P,Zn_P,Mn_P,Cu_P,Co_P)%>%
  gather(key="Me_P", value="Quota",
         Fe_P, Zn_P,Mn_P,Cu_P,Co_P)

# remove missing values from the dataset
Phosph_metals <- na.omit(Phosph_metals)

write.csv(Phosph_metals,"Bact_phosph_metals.csv")





