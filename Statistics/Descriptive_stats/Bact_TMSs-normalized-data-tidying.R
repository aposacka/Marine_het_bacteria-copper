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
Phosph_Metals_summary <-Phosph_metals %>%
  group_by(Me_P,Strain,Cu_level)%>%
  summarise(mean_quota =mean(Quota), 
            stdev=sd(Quota),
            SEM= stdev/sqrt(n()),
            (n()))

# write.csv(Descr_stats,"Macro_ratios-from-TMs-final.csv")

#-----------------------Metals-normalized-to-C-----------------



#----------------------Metals-normalized-to-cell-number--------
