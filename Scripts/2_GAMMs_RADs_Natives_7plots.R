##################
# fit GAMMs to ###
# RADs ###########
##################
# Natives       ##
# 7 plots       ##
##################

require(dplyr)
require(ggplot2)
require(grid)
require(ggsci)
require(reshape2)
require(gamm4)


##########
# SCEN 3 #
##########

#################
# RAD data ######
#################

rad.tog<-read.csv("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/RAD_AreaHawaii_Scen3natives_7plots.csv",sep=",",header=T)


rad.tog$iteration<-as.factor(rad.tog$iteration)

rad.tog$logRelAbund<-log(rad.tog$RelAbund)


rad_MN<-filter(rad.tog,geo_entity2=="Maui Nui")
rad_MN$geo_entity2<-droplevels(rad_MN$geo_entity2)


rad_HA<-filter(rad.tog,geo_entity2=="Hawai'i Island")
rad_HA$geo_entity2<-droplevels(rad_HA$geo_entity2)

rad_KA<-filter(rad.tog,geo_entity2=="Kaua'i Island")
rad_KA$geo_entity2<-droplevels(rad_KA$geo_entity2)

rad_OA<-filter(rad.tog,geo_entity2=="O'ahu Island")
rad_OA$geo_entity2<-droplevels(rad_OA$geo_entity2)


############
# FIT GAMM #
############

##################
#for each island #
##################

# Maui Nui

fit.mn<-gamm4(Rank_N~s(logRelAbund), random=~(1|iteration),REML=TRUE,data=rad_MN)
rad_MN$pred<-predict(fit.mn$gam, rad_MN,type="response")

# Hawaii

fit.ha<-gamm4(Rank_N~s(logRelAbund), random=~(1|iteration),REML=TRUE,data=rad_HA)
rad_HA$pred<-predict(fit.ha$gam, rad_HA,type="response")

#Kauai

fit.ka<-gamm4(Rank_N~s(logRelAbund), random=~(1|iteration),REML=TRUE,data=rad_KA)
rad_KA$pred<-predict(fit.ka$gam, rad_KA,type="response")

# Oahu

fit.oa<-gamm4(Rank_N~s(logRelAbund), random=~(1|iteration),REML=TRUE,data=rad_OA)
rad_OA$pred<-predict(fit.oa$gam, rad_OA,type="response")

# merge

rad_tog<-rbind.data.frame(rad_MN,rad_HA, rad_KA, rad_OA)
rad_tog<-dplyr::select(rad_tog,-logRelAbund)
colnames(rad_tog)[7]<-"RankN_pred"

write.table(rad_tog,"/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/RAD_AreaHawaii_Scen3natives_7plots_NEW.csv",sep=",",row.names=T)

##############
# Scenario 2 #
##############

rad.tog<-read.csv("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/RAD_AreaHawaii_Sc2natives_7plots.csv",sep=",",header=T)

rad.tog$iteration<-as.factor(rad.tog$iteration)

rad.tog$logRelAbund<-log(rad.tog$RelAbund)


rad_MN<-filter(rad.tog,geo_entity2=="Maui Nui")
rad_MN$geo_entity2<-droplevels(rad_MN$geo_entity2)


rad_HA<-filter(rad.tog,geo_entity2=="Hawai'i Island")
rad_HA$geo_entity2<-droplevels(rad_HA$geo_entity2)

rad_KA<-filter(rad.tog,geo_entity2=="Kaua'i Island")
rad_KA$geo_entity2<-droplevels(rad_KA$geo_entity2)

rad_OA<-filter(rad.tog,geo_entity2=="O'ahu Island")
rad_OA$geo_entity2<-droplevels(rad_OA$geo_entity2)


############
# FIT GAMM #
############

##################
#for each island #
##################

# Maui Nui

fit.mn<-gamm4(Rank_N~s(logRelAbund), random=~(1|iteration),REML=TRUE,data=rad_MN)
rad_MN$pred<-predict(fit.mn$gam, rad_MN,type="response")

# Hawaii

fit.ha<-gamm4(Rank_N~s(logRelAbund), random=~(1|iteration),REML=TRUE,data=rad_HA)
rad_HA$pred<-predict(fit.ha$gam, rad_HA,type="response")

#Kauai

fit.ka<-gamm4(Rank_N~s(logRelAbund), random=~(1|iteration),REML=TRUE,data=rad_KA)
rad_KA$pred<-predict(fit.ka$gam, rad_KA,type="response")

# Oahu

fit.oa<-gamm4(Rank_N~s(logRelAbund), random=~(1|iteration),REML=TRUE,data=rad_OA)
rad_OA$pred<-predict(fit.oa$gam, rad_OA,type="response")

# merge

rad_tog<-rbind.data.frame(rad_MN,rad_HA, rad_KA, rad_OA)
rad_tog<-dplyr::select(rad_tog,-logRelAbund)
colnames(rad_tog)[7]<-"RankN_pred"

write.table(rad_tog,"/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/RAD_AreaHawaii_Scen2natives_7plots_NEW.csv",sep=",",row.names=T)

##############
# Scenario 1 #
##############

RAD_1<-read.csv("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/RAD_Scen1natives.csv")

#RAD_1$iteration<-as.factor(RAD_1$iteration)

RAD_1$logRelAbund<-log(RAD_1$RelAbund)

rad_MN<-filter(RAD_1,geo_entity2=="Maui Nui")
rad_MN$geo_entity2<-droplevels(rad_MN$geo_entity2)


rad_HA<-filter(RAD_1,geo_entity2=="Hawai'i Island")
rad_HA$geo_entity2<-droplevels(rad_HA$geo_entity2)

rad_KA<-filter(RAD_1,geo_entity2=="Kaua'i Island")
rad_KA$geo_entity2<-droplevels(rad_KA$geo_entity2)

rad_OA<-filter(RAD_1,geo_entity2=="O'ahu Island")
rad_OA$geo_entity2<-droplevels(rad_OA$geo_entity2)


############
# FIT GAMM #
############

##################
#for each island #
##################

# Maui Nui

fit.mn<-gamm4(Rank_N~s(logRelAbund),REML=TRUE,data=rad_MN)
rad_MN$pred<-predict(fit.mn$gam, rad_MN,type="response")

# Hawaii

fit.ha<-gamm4(Rank_N~s(logRelAbund), REML=TRUE,data=rad_HA)
rad_HA$pred<-predict(fit.ha$gam, rad_HA,type="response")

#Kauai

fit.ka<-gamm4(Rank_N~s(logRelAbund), REML=TRUE,data=rad_KA)
rad_KA$pred<-predict(fit.ka$gam, rad_KA,type="response")

# Oahu

fit.oa<-gamm4(Rank_N~s(logRelAbund), REML=TRUE,data=rad_OA)
rad_OA$pred<-predict(fit.oa$gam, rad_OA,type="response")

# merge

rad_tog<-rbind.data.frame(rad_MN,rad_HA, rad_KA, rad_OA)
rad_tog<-dplyr::select(rad_tog,-logRelAbund)
colnames(rad_tog)[6]<-"RankN_pred"

write.table(rad_tog,"/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/RAD_AreaHawaii_Scen1natives_7plots_NEW.csv",sep=",",row.names=T)

###############
# Scenario IV #
###############

rad.tog<-read.csv("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/RAD_AreaHawaii_Scen4.csv",sep=",",header=T)

colnames(rad.tog)[6]<-"iteration"
rad.tog$iteration<-as.factor(rad.tog$iteration)

rad.tog$logRelAbund<-log(rad.tog$RelAbund)

rad_MN<-filter(rad.tog,geo_entity2=="Maui Nui")
rad_MN$geo_entity2<-droplevels(rad_MN$geo_entity2)

rad_HA<-filter(rad.tog,geo_entity2=="Hawai'i Island")
rad_HA$geo_entity2<-droplevels(rad_HA$geo_entity2)

rad_KA<-filter(rad.tog,geo_entity2=="Kaua'i Island")
rad_KA$geo_entity2<-droplevels(rad_KA$geo_entity2)

#rad_OA<-filter(rad.tog,geo_entity2=="O'ahu Island")
#rad_OA$geo_entity2<-droplevels(rad_OA$geo_entity2)


############
# FIT GAMM #
############

##################
#for each island #
##################

# Maui Nui

fit.mn<-gamm4(Rank_N~s(logRelAbund), random=~(1|iteration),REML=TRUE,data=rad_MN)
rad_MN$pred<-predict(fit.mn$gam, rad_MN,type="response")

# Hawaii

fit.ha<-gamm4(Rank_N~s(logRelAbund), random=~(1|iteration),REML=TRUE,data=rad_HA)
rad_HA$pred<-predict(fit.ha$gam, rad_HA,type="response")

#Kauai

fit.ka<-gam(Rank_N~s(logRelAbund),REML=TRUE,data=rad_KA)
rad_KA$pred<-predict(fit.ka, rad_KA,type="response")

# Oahu

#fit.oa<-gamm4(Rank_N~s(logRelAbund), random=~(1|iteration),REML=TRUE,data=rad_OA)
#rad_OA$pred<-predict(fit.oa$gam, rad_OA,type="response")

# merge

rad_tog<-rbind.data.frame(rad_MN,rad_HA, rad_KA)
rad_tog<-dplyr::select(rad_tog,-logRelAbund)
colnames(rad_tog)[7]<-"RankN_pred"

write.table(rad_tog,"/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/RAD_AreaHawaii_Scen4natives_7plots_NEW.csv",sep=",",row.names=T)

