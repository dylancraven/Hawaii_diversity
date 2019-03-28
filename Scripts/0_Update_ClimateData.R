########################
# Update climate data ##
########################
# Now using CHELSA Version 1.2
# Now using PET and Aridity v2 (based on WorldClim2)

require(dplyr)

#################
# Data ##########
#################

env<-read.csv("Data/Hawaiian_Env_Soil_1km.csv",sep=",",header=T)
colnames(env)[1]<-"PlotID"
envv<-select(env, PlotID, Long_Dec, Lat_Dec, Elev_m,PrecipSeasonality, TempSeasonality, HFP, HII, SubstrateAge_range, SubstrateAge_code)

new_env<-read.csv("Data/Hawaii_newclimate.csv",sep=",",header=T)
new_env<-arrange(new_env, PlotID)
new_env<-select(new_env,PlotID, Long_Dec, Lat_Dec, MAT=newMAT, MAP=newMAP, PET=newPET, AridInd)

env_v2<-left_join(envv, new_env, by.y=c("PlotID","Long_Dec","Lat_Dec"))

write.csv(env_v2,"Data/Hawaiian_Env_Soil_1km_v2.csv",row.names=FALSE)
