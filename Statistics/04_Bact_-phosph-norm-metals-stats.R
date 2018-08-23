# ----
# This script calculates summary statistics for each
# metal & strain from the
# 01_Bact_metals-P-norm-tidydat.csv, and prints it to a csv file
# ----


# load the data from csv, the function for this "read_csv" is in the tidyverse package
library(tidyverse)
data <-read_csv("Data/01_Bact-metals-P-norm-tidydata.csv")

# to check what's in this file run the glimpse()
glimpse(data)


# for what is to be done next Cu level and strain have to be in the 
# factor data type, but for me they are imported as a character and a double 

data$Cu_level <- as.factor(data$Cu_level)
data$Strain<- as.factor(data$Strain)
data$Me_P <- as.factor(data$Me_P)


#------------------------Metals-normalized-to-P----------------

Phosph_metals <-data%>%
  select(Strain,Cu_level,Fe_P,Zn_P,Mn_P,Cu_P,Co_P)%>%
  gather(key="Me_P", value="Quota",
         Fe_P, Zn_P,Mn_P,Cu_P,Co_P)

# remove missing values from the dataset
Phosph_metals <- na.omit(Phosph_metals)

# average ratios for each strain and at each copper level
Phosph_metals_summary <-data %>%
  group_by(Me_P,Strain,Cu_level)%>%
  summarise(mean_quota =mean(Quota), 
            stdev=sd(Quota),
            SEM= stdev/sqrt(n()),
            (n()))

write.csv(Phosph_metals_summary,"Phosph_metals_summary.csv")


