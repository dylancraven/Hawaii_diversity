##################
# fit GAMMs to ###
# RADs ###########
##################
# Natives       ##
# 7 plots       ##
# BIG trees     ##
##################

require(dplyr)
require(reshape2)
require(gamm4)

##############
# Scenario 1 #
##############

load("Cleaned_Data/Scen1_natives_BIG.RData")

radd$logRelAbund<-log(radd$RelAbund)

rad_MN<-filter(radd,geo_entity2=="Maui Nui")
rad_MN$geo_entity2<-droplevels(rad_MN$geo_entity2)


rad_HA<-filter(radd,geo_entity2=="Hawai'i Island")
rad_HA$geo_entity2<-droplevels(rad_HA$geo_entity2)

rad_KA<-filter(radd,geo_entity2=="Kaua'i Island")
rad_KA$geo_entity2<-droplevels(rad_KA$geo_entity2)

rad_OA<-filter(radd,geo_entity2=="O'ahu Island")
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

rad_tog1<-rbind.data.frame(rad_MN,rad_HA, rad_KA, rad_OA)
rad_tog1<-dplyr::select(rad_tog1,-logRelAbund)
colnames(rad_tog1)[6]<-"RankN_pred"

##############
# Scenario 2 #
##############

load("Cleaned_Data/Scen2_natives_BIG.RData")

Scen2_rad.tog$iteration<-as.factor(Scen2_rad.tog$iteration)

Scen2_rad.tog$logRelAbund<-log(Scen2_rad.tog$RelAbund)

rad_MN<-filter(Scen2_rad.tog,geo_entity2=="Maui Nui")
rad_MN$geo_entity2<-droplevels(rad_MN$geo_entity2)

rad_HA<-filter(Scen2_rad.tog,geo_entity2=="Hawai'i Island")
rad_HA$geo_entity2<-droplevels(rad_HA$geo_entity2)

rad_KA<-filter(Scen2_rad.tog,geo_entity2=="Kaua'i Island")
rad_KA$geo_entity2<-droplevels(rad_KA$geo_entity2)

rad_OA<-filter(Scen2_rad.tog,geo_entity2=="O'ahu Island")
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

rad_tog2<-rbind.data.frame(rad_MN,rad_HA, rad_KA, rad_OA)
rad_tog2<-dplyr::select(rad_tog2,-logRelAbund)
colnames(rad_tog2)[7]<-"RankN_pred"


##########
# SCEN 3 #
##########

load("Cleaned_Data/Scen3_natives_BIG.RData")

Scen3_rad.tog$iteration<-as.factor(Scen3_rad.tog$iteration)

Scen3_rad.tog$logRelAbund<-log(Scen3_rad.tog$RelAbund)

rad_MN<-filter(Scen3_rad.tog,geo_entity2=="Maui Nui")
rad_MN$geo_entity2<-droplevels(rad_MN$geo_entity2)

rad_HA<-filter(Scen3_rad.tog,geo_entity2=="Hawai'i Island")
rad_HA$geo_entity2<-droplevels(rad_HA$geo_entity2)

rad_KA<-filter(Scen3_rad.tog,geo_entity2=="Kaua'i Island")
rad_KA$geo_entity2<-droplevels(rad_KA$geo_entity2)

rad_OA<-filter(Scen3_rad.tog,geo_entity2=="O'ahu Island")
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

rad_tog3<-rbind.data.frame(rad_MN,rad_HA, rad_KA, rad_OA)
rad_tog3<-dplyr::select(rad_tog3,-logRelAbund)
colnames(rad_tog3)[7]<-"RankN_pred"

####

save(rad_tog1,rad_tog2, rad_tog3, 
     file="Cleaned_Data/Scen123_Native_Big_RAD_GAMMestimates.RData")
