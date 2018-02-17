# Script for extracting data from the speadsheet: Elemental_stoichiometry-synthesis.csv and preparing 
# into a tidy format (variable names in one column and values in another)

# The output files are:
# Copper-to-carbon-synthesis-Aquil.csv, which contains Cu quotas for phytoplankton and bacteria from
# Aquil culture studies at a specific Cu' level
# Metals-to-carbon-synthesis-Aquil.csv, all metals normalized to C from Aquil studies
# Metals-to-phosph-synthesis-Aquil.csv, all metals normalzied to P from Aquil studies
# Metals-to-carbon-synthesis-all-studies.csv, metals normalized to C from various studies, culture and field
# load the data

# various functions from the tidiverse package are used here
library(tidyverse)

# load the data
data <-read.csv("C:/Users/Ania/Bact_Stoichiometry/Extended_ratios_manuscript/Excel_files/Elemental_stoichiometry-literature.csv") 

glimpse(data)


# creating a DF with the following cols: 1) Ref, 2) Domain,3)Taxon,4) Species,
# 5)Me_C (names of metals normalized to C) and 6) Quota (values corresponding to the metals-to-carbon)

metals_to_carbon <-data%>%
	select(Ref,Study,Domain,Taxon,Species,Cu_C,Fe_C,Mn_C,Zn_C,Co_C)%>%
	gather(key = "Me_C",value = "Quota",Cu_C,Fe_C,Mn_C,Zn_C,Co_C)

metals_to_carbon <-na.omit(metals_to_carbon)
write.csv(metals_to_carbon,"Metals_to-carbon-synthesis.csv")

# creating a DF with he following cols: 1) Ref, 2) Domain,3)Taxon,4) Species,
# 5)Me_P (names of metals normalized to C) and 6) Quota (values corresponding to the metals-to-carbon)

metals_to_phosph <-data%>%
	select(Ref,Study,Domain,Taxon,Species,Cu_P,Fe_P,Mn_P,Zn_P,Co_P)%>%
	gather(key = "Me_P",value = "Quota",Cu_P,Fe_P,Mn_P,Zn_P,Co_P)

metals_to_phosph <-na.omit(metals_to_phosph)
write.csv(metals_to_phosph,"Metals_to-phosph-synthesis.csv")

# creating a DF with he following cols: 1) Ref, 2) Domain,3)Taxon,4) Species,
# 5)Me_P (names of metals normalized to C) and 6) Quota (values corresponding to the metals-to-carbon)

macros<-data%>%
	select(Ref,Study,Domain,Taxon,Species,C_P,P_C,C_N,N_C,N_P,S_P,S_C)%>%
	gather(key = "Macro",value = "Ratio",C_P,P_C,C_N,N_C,N_P,S_P,S_C)


macros <- na.omit(macros)
write.csv(macros,"Macro_ratios-synthesis.csv")

#--------------------------------Extended-Redfield-calculations------------------------


