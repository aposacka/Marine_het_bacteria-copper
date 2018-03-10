#-------------------------------------------------------------------
# This scripts transforms the bacterial carbon metabolism data 
# into a tidy format so that it's compatiple with faceting in ggplot
#-------------------------------------------------------------------

# loading packages
library(tidyverse)
library(cowplot)

# loading data
data <-read_csv("Data/03_Bact_carb-metab-data.csv")
glimpse(data)

tidy_carbs <-data%>%
  select(Strain,Cu_level,BR_cell_day,BP, BCD, BGE)%>%
  gather(key="Category", value="Value",
         BR_cell_day,BP, BCD, BGE)

# remove missing values from the dataset
tidy_carbs <- na.omit(tidy_carbs)

write.csv(tidy_carbs,"03_Bact-carb-metab-tidydata.csv")