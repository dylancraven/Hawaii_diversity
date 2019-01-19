###################################################
# QC: simulations of native & aliens species only #
###################################################

require(dplyr)

######################
# Scenario 2 #########
######################

scen2_HillN<-read.csv("Cleaned_Data/Scen2_Total_10plots_HillN.csv")

scen2_SACs<-read.csv("Cleaned_Data/Scen2_Total_10plots_curves_estimates.csv")

scen2_beta<-read.csv("Cleaned_Data/Scen2_Total_10plots_BetaPIE.csv")

scen2_RAD<-read.csv("Cleaned_Data/Scen2_Total_10plots_RADs.csv")

# Hill N
scen2_HillN_iter<-filter(scen2_HillN, order==0 & m == 10000)

islands_per_iter<-summarize(group_by(scen2_HillN_iter, iteration), islands_n=length(unique(geo_entity2)))

range(islands_per_iter$islands_n) # always 4 - so ok

length(unique(islands_per_iter$iteration)) # 100 - so ok

# RADs

scen2_RADs_iter<-summarize(group_by(scen2_RAD, iteration), islands_n= length(unique(geo_entity2)))

range(scen2_RADs_iter$islands_n) # always 4 - so ok

length(unique(scen2_RADs_iter$iteration)) # 100 - so ok

# beta diversity

scen2_beta_iter<-filter(scen2_beta, index=="beta_S")
scen2_beta_iterr<-summarize(group_by(scen2_beta_iter, Iteration), islands_n= length(unique(geo_entity2)))

range(scen2_beta_iterr$islands_n) # always 4 - so ok

length(unique(scen2_beta_iterr$Iteration)) # 100 - so ok

# SACs 
scen2_SACs_iter<-filter(scen2_SACs, method=="interpolated" & m == 1)

scen2_SACs_iterr<-summarize(group_by(scen2_SACs_iter, iteration), islands_n= length(unique(geo_entity2)))

range(scen2_SACs_iterr$islands_n) # always 4 - so ok

length(unique(scen2_SACs_iterr$iteration)) # 100 - so ok

# how many m per iteration per island?

scen2_SACs_iter2<-filter(scen2_SACs, method=="interpolated")
scen2_SACs_iterr<-summarize(group_by(scen2_SACs_iter2, iteration, geo_entity2), m_n= length(unique(m)))

range(scen2_SACs_iterr$m_n) # ok - this will depend on the selected plots per iteration/island

######################
# Scenario 3 #########
######################

scen3_HillN<-read.csv("Cleaned_Data/Scen3_Total_10plots_HillN.csv")

scen3_SACs<-read.csv("Cleaned_Data/Scen3_Total_10plots_curves_estimates.csv")

scen3_beta<-read.csv("Cleaned_Data/Scen3_Total_10plots_BetaPIE.csv")

scen3_RAD<-read.csv("Cleaned_Data/Scen3_Total_10plots_RAD.csv")

# Hill N

scen3_HillN_iter<-filter(scen3_HillN, m == 10000 & order == 0)

islands_per_iter3<-summarize(group_by(scen3_HillN_iter, iteration), islands_n=length(unique(geo_entity2)))

range(islands_per_iter3$islands_n) # always 4 - so ok

length(unique(islands_per_iter3$iteration)) # 100 - so ok

# RADs

scen3_RADs_iter<-summarize(group_by(scen3_RAD, iteration), islands_n= length(unique(geo_entity2)))

range(scen3_RADs_iter$islands_n) # always 4 - so ok

length(unique(scen3_RADs_iter$iteration)) # 100 - so ok

# beta diversity

scen3_beta_iter<-filter(scen3_beta, index=="beta_S")
scen3_beta_iter<-summarize(group_by(scen3_beta_iter, Iteration), islands_n= length(unique(geo_entity2)))

range(scen3_beta_iter$islands_n) # always 4 - so ok

length(unique(scen3_beta_iter$Iteration)) # 100 - so ok

# SACs 
scen3_SACs_iter<-filter(scen3_SACs, method=="interpolated" & m == 1)

scen3_SACs_iterr<-summarize(group_by(scen3_SACs_iter, iteration), islands_n= length(unique(geo_entity2)))

range(scen3_SACs_iterr$islands_n) # always 4 - so ok

length(unique(scen3_SACs_iterr$iteration)) # 100 - so ok

# how many m per iteration per island?

scen3_SACs_iter2<-filter(scen3_SACs, method=="interpolated")
scen3_SACs_iter2<-summarize(group_by(scen3_SACs_iter2, iteration, geo_entity2), m_n= length(unique(m)))

range(scen3_SACs_iter2$m_n) # ok - this will depend on the selected plots per iteration/island


  