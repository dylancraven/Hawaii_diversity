###############
# fit GAMMs ###
# to SACs per #
# iteration ###
###############
# Natives ##### 
# 7 plots #####
# #############

require(dplyr)
require(ggplot2)
require(reshape2)
require(gamm4)

##############
# Scenario 1 #
##############

sac.tog<-read.csv("Cleaned_Data/Scen1_Natives_zoom_curves_estimates.csv",sep=",",header=T)

sac_MN<-filter(sac.tog,geo_entity2=="Maui Nui")
sac_MN$geo_entity2<-droplevels(sac_MN$geo_entity2)

sac_HA<-filter(sac.tog,geo_entity2=="Hawai'i Island")
sac_HA$geo_entity2<-droplevels(sac_HA$geo_entity2)

sac_KA<-filter(sac.tog,geo_entity2=="Kaua'i Island")
sac_KA$geo_entity2<-droplevels(sac_KA$geo_entity2)

sac_OA<-filter(sac.tog,geo_entity2=="O'ahu Island")
sac_OA$geo_entity2<-droplevels(sac_OA$geo_entity2)

############
# FIT GAMM #
############

##################
#for each island #
##################

# Maui Nui

fit.mn<-gam(qD~s(m),REML=TRUE,data=sac_MN)
sac_MN$pred<-predict(fit.mn, sac_MN,type="response")

# Hawaii

fit.ha<-gam(qD~s(m),REML=TRUE,data=sac_HA)
sac_HA$pred<-predict(fit.ha, sac_HA,type="response")

#Kauai

fit.ka<-gam(qD~s(m),REML=TRUE,data=sac_KA)
sac_KA$pred<-predict(fit.ka, sac_KA,type="response")

# Oahu

fit.oa<-gam(qD~s(m),REML=TRUE,data=sac_OA)
sac_OA$pred<-predict(fit.oa, sac_OA,type="response")

# merge

sac_togg1<-rbind.data.frame(sac_MN,sac_HA, sac_KA, sac_OA)
colnames(sac_togg1)[11]<-"qD_pred"

write.table(sac_togg1,"Cleaned_Data/Scen1_Natives_SACs_GAMMestimates.csv",sep=",",row.names=T)

##############
# Scenario 2 #
##############

sac.tog<-read.csv("Cleaned_Data/Scen2_Natives_7plots_curves_estimates.csv")

sac.tog$iteration<-as.factor(sac.tog$iteration)

sac_MN<-filter(sac.tog,geo_entity2=="Maui Nui")
sac_MN$geo_entity2<-droplevels(sac_MN$geo_entity2)

sac_HA<-filter(sac.tog,geo_entity2=="Hawai'i Island")
sac_HA$geo_entity2<-droplevels(sac_HA$geo_entity2)

sac_KA<-filter(sac.tog,geo_entity2=="Kaua'i Island")
sac_KA$geo_entity2<-droplevels(sac_KA$geo_entity2)

sac_OA<-filter(sac.tog,geo_entity2=="O'ahu Island")
sac_OA$geo_entity2<-droplevels(sac_OA$geo_entity2)

############
# FIT GAMM #
############

##################
#for each island #
##################

# Maui Nui

fit.mn<-gamm4(qD~s(m), random=~(1|iteration),REML=TRUE,data=sac_MN)
sac_MN$pred<-predict(fit.mn$gam, sac_MN,type="response")

# Hawaii

fit.ha<-gamm4(qD~s(m), random=~(1|iteration),REML=TRUE,data=sac_HA)
sac_HA$pred<-predict(fit.ha$gam, sac_HA,type="response")

#Kauai

fit.ka<-gamm4(qD~s(m), random=~(1|iteration),REML=TRUE,data=sac_KA)
sac_KA$pred<-predict(fit.ka$gam, sac_KA,type="response")

# Oahu

fit.oa<-gamm4(qD~s(m), random=~(1|iteration),REML=TRUE,data=sac_OA)
sac_OA$pred<-predict(fit.oa$gam, sac_OA,type="response")

# merge

sac_togg<-rbind.data.frame(sac_MN,sac_HA, sac_KA, sac_OA)
colnames(sac_togg)[12]<-"qD_pred"

write.table(sac_togg,"Cleaned_Data/Scen2_Natives_SACs_GAMMestimates.csv",sep=",",row.names=T)

##########
# SCEN 3 #
##########

sac.tog<-read.csv("Cleaned_Data/Scen3_Natives_7plots_curves_estimates.csv")

sac.tog$iteration<-as.factor(sac.tog$iteration)

sac_MN<-filter(sac.tog,geo_entity2=="Maui Nui")
sac_MN$geo_entity2<-droplevels(sac_MN$geo_entity2)

sac_HA<-filter(sac.tog,geo_entity2=="Hawai'i Island")
sac_HA$geo_entity2<-droplevels(sac_HA$geo_entity2)

sac_KA<-filter(sac.tog,geo_entity2=="Kaua'i Island")
sac_KA$geo_entity2<-droplevels(sac_KA$geo_entity2)

sac_OA<-filter(sac.tog,geo_entity2=="O'ahu Island")
sac_OA$geo_entity2<-droplevels(sac_OA$geo_entity2)

############
# FIT GAMM #
############

##################
#for each island #
##################

# Maui Nui

fit.mn<-gamm4(qD~s(m), random=~(1|iteration),REML=TRUE,data=sac_MN)
sac_MN$pred<-predict(fit.mn$gam, sac_MN,type="response")

# Hawaii

fit.ha<-gamm4(qD~s(m), random=~(1|iteration),REML=TRUE,data=sac_HA)
sac_HA$pred<-predict(fit.ha$gam, sac_HA,type="response")

#Kauai

fit.ka<-gamm4(qD~s(m), random=~(1|iteration),REML=TRUE,data=sac_KA)
sac_KA$pred<-predict(fit.ka$gam, sac_KA,type="response")

# Oahu

fit.oa<-gamm4(qD~s(m), random=~(1|iteration),REML=TRUE,data=sac_OA)
sac_OA$pred<-predict(fit.oa$gam, sac_OA,type="response")

# merge

sac_togg<-rbind.data.frame(sac_MN,sac_HA, sac_KA, sac_OA)
colnames(sac_togg)[12]<-"qD_pred"

write.table(sac_togg,"Cleaned_Data/Scen3_Natives_SACs_GAMMestimates.csv",sep=",",row.names=T)