# ----
# This script extracts various variables from the
# Bact_TMS-normalized-final.csv, cleans up the
# missing values and prepares the dataset into a tidy 
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

# exracting C_P, N_P and S_P data and creating 2 columns, one for 
# ratio names and the other for the values.
# Mote: N:C ratios-this data is combined with more measurements 
# of C and N contents done seperately

Macros <-data%>%
	select(Strain,Cu_level,C_P,N_P,P_C,S_P,N_C,S_C)%>%
	gather(key="Macro_ratio", value="Quota",
				 C_P,N_P,P_C,S_P,N_C,S_C)

# remove missing values from the dataset
Macros <- na.omit(Macros)

# average ratios for each strain and at each copper level
Descr_stats <-Macros %>%
	group_by(Macro_ratio,Strain,Cu_level)%>%
	summarise(mean_quota =mean(Quota), 
						stdev=sd(Quota),
						SEM= stdev/sqrt(n()),
						(n()))

# save the output
# write.csv(Descr_stats,"Macro_ratios-from-TMs-final.csv")

#------------------------Metals-normalized-to-P----------------

Phosph_metals <-data%>%
  select(Strain,Cu_level,Fe_P,Zn_P,Mn_P,Cu_P,Co_P)%>%
  gather(key="Me_P", value="Quota",
         Fe_P, Zn_P,Mn_P,Cu_P,Co_P)

# remove missing values from the dataset
Phosph_metals <- na.omit(Phosph_metals)

# average ratios for each strain and at each copper level
Phosph_metals_summary <-Phosph_metals %>%
  group_by(Me_P,Strain,Cu_level)%>%
  summarise(mean_quota =mean(Quota), 
            stdev=sd(Quota),
            SEM= stdev/sqrt(n()),
            (n()))

# write.csv(Phosph_metals_summary,"Phosph_metals_summary.csv")

#-----------------------Metals-normalized-to-C-----------------

Carb_metals <-data%>%
  select(Strain,Cu_level,Fe_C,Zn_C,Mn_C,Cu_C,Co_C)%>%
  gather(key="Me_C", value="Quota",
         Fe_C,Zn_C,Mn_C,Cu_C,Co_C)

# remove missing values from the dataset
Carb_metals <- na.omit(Carb_metals)

# average ratios for each strain and at each copper level
Carb_metals_summary <-Carb_metals %>%
  group_by(Me_C,Strain,Cu_level)%>%
  summarise(mean_quota =mean(Quota), 
            stdev=sd(Quota),
            SEM= stdev/sqrt(n()),
            (n()))

# write.csv(Carb_metals_summary,"Carb_metals_summary.csv")
#----------------------Metals-normalized-to-cell-number--------

Cell_metals <-data%>%
  select(Strain,Cu_level,Cellular_Fe,Cellular_Zn, Cellular_Mn,Cellular_Cu, Cellular_Co)%>%
  gather(key="Me_cell", value="Quota",
         Cellular_Fe,Cellular_Zn, Cellular_Mn,Cellular_Cu, Cellular_Co)

# remove missing values from the dataset
Cell_metals <- na.omit(Cell_metals)

# average ratios for each strain and at each copper level
Cell_metals_summary <-Cell_metals %>%
  group_by(Me_cell,Strain,Cu_level)%>%
  summarise(mean_quota =mean(Quota), 
            stdev=sd(Quota),
            SEM= stdev/sqrt(n()),
            (n()))

# write.csv(Cell_metals_summary,"Cell_metals_summary.csv")
# ---------------------the end-------------------------------------

