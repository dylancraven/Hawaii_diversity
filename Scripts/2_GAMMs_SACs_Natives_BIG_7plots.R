###############
# fit GAMMs ###
# to SACs per #
# iteration ###
###############
# Natives ##### 
# 7 plots #####
# BIG trees ###
# #############

require(dplyr)
require(ggplot2)
require(reshape2)
require(gamm4)

##############
# Scenario 1 #
##############

load("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Cleaned_Data/Scen1_natives_BIG.RData")

curves3$geo_entity2<-as.factor(curves3$geo_entity2)
curves3<-filter(curves3, m %in% c(1,100,1000,2000,3000,4000,5000, 6000,7000,8000,9000,10000))

sac_MN<-filter(curves3,geo_entity2=="Maui Nui")
sac_MN$geo_entity2<-droplevels(sac_MN$geo_entity2)

sac_HA<-filter(curves3,geo_entity2=="Hawai'i Island")
sac_HA$geo_entity2<-droplevels(sac_HA$geo_entity2)

sac_KA<-filter(curves3,geo_entity2=="Kaua'i Island")
sac_KA$geo_entity2<-droplevels(sac_KA$geo_entity2)

sac_OA<-filter(curves3,geo_entity2=="O'ahu Island")
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


##############
# Scenario 2 #
##############

load("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Cleaned_Data/Scen2_natives_BIG.RData")

Scen2_curves.tog$geo_entity2<-as.factor(Scen2_curves.tog$geo_entity2)
Scen2_curves.tog<-filter(Scen2_curves.tog, m %in% c(1,100,200,300,400,500, 600,700,800,900,1000))

Scen2_curves.tog$iteration<-as.factor(Scen2_curves.tog$iteration)

sac_MN<-filter(Scen2_curves.tog,geo_entity2=="Maui Nui")
sac_MN$geo_entity2<-droplevels(sac_MN$geo_entity2)

sac_HA<-filter(Scen2_curves.tog,geo_entity2=="Hawai'i Island")
sac_HA$geo_entity2<-droplevels(sac_HA$geo_entity2)

sac_KA<-filter(Scen2_curves.tog,geo_entity2=="Kaua'i Island")
sac_KA$geo_entity2<-droplevels(sac_KA$geo_entity2)

sac_OA<-filter(Scen2_curves.tog,geo_entity2=="O'ahu Island")
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

sac_togg2<-rbind.data.frame(sac_MN,sac_HA, sac_KA, sac_OA)
colnames(sac_togg2)[12]<-"qD_pred"


##########
# SCEN 3 #
##########

load("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Cleaned_Data/Scen3_natives_BIG.RData")

Scen3_curves.tog$geo_entity2<-as.factor(Scen3_curves.tog$geo_entity2)
Scen3_curves.tog<-filter(Scen3_curves.tog, m %in% c(1,100,200,300,400,500, 600,700,800,900,1000))

Scen3_curves.tog$iteration<-as.factor(Scen3_curves.tog$iteration)

sac_MN<-filter(Scen3_curves.tog,geo_entity2=="Maui Nui")
sac_MN$geo_entity2<-droplevels(sac_MN$geo_entity2)

sac_HA<-filter(Scen3_curves.tog,geo_entity2=="Hawai'i Island")
sac_HA$geo_entity2<-droplevels(sac_HA$geo_entity2)

sac_KA<-filter(Scen3_curves.tog,geo_entity2=="Kaua'i Island")
sac_KA$geo_entity2<-droplevels(sac_KA$geo_entity2)

sac_OA<-filter(Scen3_curves.tog,geo_entity2=="O'ahu Island")
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

sac_togg3<-rbind.data.frame(sac_MN,sac_HA, sac_KA, sac_OA)
colnames(sac_togg3)[12]<-"qD_pred"

save(sac_togg1,sac_togg2, sac_togg3, file="/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Cleaned_Data/Scen123_Native_Big_GAMMestimates.RData")
