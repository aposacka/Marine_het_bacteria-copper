# bits of code for R beginner workshop


# converting certain variables to factors and changing the names (I think)
data$Strain<-factor(data$Strain, levels = c("Dokd_P16","Rugeria", "PAlt_P2","PAlt-P26"))
data$Cu_level <- factor(data$Cu_level, levels = c("0.6","2","10","25","50"))
data$Metal <- factor(data$Metal,levels = c("Fe_P","Zn_P","Cu_P","Mn_P", "Co_P"))

# check it's changed to what you wanted
levels(data$Me_P)

# For the workshop: ggplot stuff
#
# 1. Show how to change colors 
# 2. Show how to reorder yoour factors for better vis
# 3. Practices for writing code