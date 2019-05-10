#################################
# Scenario 3                    # 
# Summarize species diversity   #
# 1) all species (native +alien)#
# 2) native species             #
#################################
# with anovas ###################
#################################

require(car)
require(broom)
require(dplyr)
require(tidyr)
require(ggeffects)

##########
# data  ##
##########

HillN_All<-read.csv("Cleaned_Data/Scen3_Total_10plots_HillN.csv")

HillN_All$geo_entity2<-as.character(HillN_All$geo_entity2)
HillN_All$geo_entity2<-ifelse(HillN_All$geo_entity2=="O'ahu Island (incl. Mokoli'i Islet)","O'ahu",HillN_All$geo_entity2)
HillN_All$geo_entity2<-ifelse(HillN_All$geo_entity2=="O'ahu Island","O'ahu",HillN_All$geo_entity2)
HillN_All$geo_entity2<-ifelse(HillN_All$geo_entity2=="Hawai'i Island","Hawai'i",HillN_All$geo_entity2)
HillN_All$geo_entity2<-ifelse(HillN_All$geo_entity2=="Kaua'i Island","Kaua'i",HillN_All$geo_entity2)

HillN_All$geo_entity2<-as.factor(HillN_All$geo_entity2)
HillN_All$geo_entity2<-factor(HillN_All$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

HillN_Natives<-read.csv("Cleaned_Data/Scen3_Natives_10plots_HillN.csv")

HillN_Natives$geo_entity2<-as.character(HillN_Natives$geo_entity2)
HillN_Natives$geo_entity2<-ifelse(HillN_Natives$geo_entity2=="O'ahu Island (incl. Mokoli'i Islet)","O'ahu",HillN_Natives$geo_entity2)
HillN_Natives$geo_entity2<-ifelse(HillN_Natives$geo_entity2=="O'ahu Island","O'ahu",HillN_Natives$geo_entity2)
HillN_Natives$geo_entity2<-ifelse(HillN_Natives$geo_entity2=="Hawai'i Island","Hawai'i",HillN_Natives$geo_entity2)
HillN_Natives$geo_entity2<-ifelse(HillN_Natives$geo_entity2=="Kaua'i Island","Kaua'i",HillN_Natives$geo_entity2)

HillN_Natives$geo_entity2<-as.factor(HillN_Natives$geo_entity2)
HillN_Natives$geo_entity2<-factor(HillN_Natives$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

#############
# All spp.  #
#############
# order 0

HillN_All_q0<-filter(HillN_All, m==10000 & order==0)

# visual normality check

qqnorm(HillN_All_q0$qD) # ok

# lm

lm_HillN_All_q0<-lm(qD~ geo_entity2, data=HillN_All_q0)

Scen3_AllSpp_q0_r2<-summary(lm_HillN_All_q0)$r.squared
Scen3_AllSpp_q0_modelsumm<-tidy(Anova(lm_HillN_All_q0,test.statistic="F"))
Scen3_AllSpp_q0_modelpred<-ggeffect(lm_HillN_All_q0, terms="geo_entity2", x.as.factor = T)

# q2

HillN_All_q2<-filter(HillN_All, m==10000 & order==2)

# visual normality check

qqnorm(HillN_All_q2$qD) # ok

# lm

lm_HillN_All_q2<-lm(qD~ geo_entity2, data=HillN_All_q2)

Scen3_AllSpp_q2_r2<-summary(lm_HillN_All_q2)$r.squared
Scen3_AllSpp_q2_modelsumm<-tidy(Anova(lm_HillN_All_q2,test.statistic="F"))
Scen3_AllSpp_q2_modelpred<-ggeffect(lm_HillN_All_q2, terms="geo_entity2", x.as.factor = T)

################
# just natives #
################
# order 0

HillN_Natives_q0<-filter(HillN_Natives, m==10000 & order==0)

# visual normality check

qqnorm(HillN_Natives_q0$qD) # ok

# lm

lm_HillN_Natives_q0<-lm(qD~ geo_entity2, data=HillN_Natives_q0)

Scen3_NativesSpp_q0_r2<-summary(lm_HillN_Natives_q0)$r.squared
Scen3_NativesSpp_q0_modelsumm<-tidy(Anova(lm_HillN_Natives_q0,test.statistic="F"))
Scen3_NativesSpp_q0_modelpred<-ggeffect(lm_HillN_Natives_q0, terms="geo_entity2", x.as.factor = T)

# q2

HillN_Natives_q2<-filter(HillN_Natives, m==10000 & order==2)

# visual normality check

qqnorm(HillN_Natives_q2$qD) # ok

# lm

lm_HillN_Natives_q2<-lm(qD~ geo_entity2, data=HillN_Natives_q2)

Scen3_NativesSpp_q2_r2<-summary(lm_HillN_Natives_q2)$r.squared
Scen3_NativesSpp_q2_modelsumm<-tidy(Anova(lm_HillN_Natives_q2,test.statistic="F"))
Scen3_NativesSpp_q2_modelpred<-ggeffect(lm_HillN_Natives_q2, terms="geo_entity2", x.as.factor = T)

#######
# out #
#######
save(Scen3_AllSpp_q0_r2,Scen3_AllSpp_q0_modelsumm,Scen3_AllSpp_q0_modelpred, 
     Scen3_AllSpp_q2_r2,Scen3_AllSpp_q2_modelsumm,Scen3_AllSpp_q2_modelpred, 
     Scen3_NativesSpp_q0_r2,Scen3_NativesSpp_q0_modelsumm,Scen3_NativesSpp_q0_modelpred, 
     Scen3_NativesSpp_q2_r2,Scen3_NativesSpp_q2_modelsumm,Scen3_NativesSpp_q2_modelpred, 
     file="Cleaned_Data/Scen3_All_Native_HillN_anova_summary.RData")