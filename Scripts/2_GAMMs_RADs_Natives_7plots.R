##################
# fit GAMMs to ###
# RADs ###########
##################
# Natives       ##
# 7 plots       ##
##################

require(dplyr)
require(reshape2)
require(gamm4)

##############
# Scenario 1 #
##############

RAD_1<-read.csv("Cleaned_Data/Scen1_Natives_RAD.csv")

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

write.table(rad_tog,"Cleaned_Data/Scen1_Natives_RAD_GAMMestimates.csv",sep=",",row.names=T)

##############
# Scenario 2 #
##############

rad.tog2<-read.csv("Cleaned_Data/Scen2_Natives_7plots_RADs.csv",sep=",",header=T)

rad.tog2$iteration<-as.factor(rad.tog2$iteration)

rad.tog2$logRelAbund<-log(rad.tog2$RelAbund)


rad_MN<-filter(rad.tog2,geo_entity2=="Maui Nui")
rad_MN$geo_entity2<-droplevels(rad_MN$geo_entity2)


rad_HA<-filter(rad.tog2,geo_entity2=="Hawai'i Island")
rad_HA$geo_entity2<-droplevels(rad_HA$geo_entity2)

rad_KA<-filter(rad.tog2,geo_entity2=="Kaua'i Island")
rad_KA$geo_entity2<-droplevels(rad_KA$geo_entity2)

rad_OA<-filter(rad.tog2,geo_entity2=="O'ahu Island")
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

write.table(rad_tog2,"Cleaned_Data/Scen2_Natives_RAD_GAMMestimates.csv",sep=",",row.names=T)


##########
# SCEN 3 #
##########

rad.tog3<-read.csv("Cleaned_Data/Scen3_Natives_7plots_RAD.csv",sep=",",header=T)

rad.tog3$iteration<-as.factor(rad.tog3$iteration)

rad.tog3$logRelAbund<-log(rad.tog3$RelAbund)

rad_MN<-filter(rad.tog3,geo_entity2=="Maui Nui")
rad_MN$geo_entity2<-droplevels(rad_MN$geo_entity2)

rad_HA<-filter(rad.tog3,geo_entity2=="Hawai'i Island")
rad_HA$geo_entity2<-droplevels(rad_HA$geo_entity2)

rad_KA<-filter(rad.tog3,geo_entity2=="Kaua'i Island")
rad_KA$geo_entity2<-droplevels(rad_KA$geo_entity2)

rad_OA<-filter(rad.tog3,geo_entity2=="O'ahu Island")
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

write.table(rad_tog3,"Cleaned_Data/Scen3_Natives_RAD_GAMMestimates.csv",sep=",",row.names=T)
