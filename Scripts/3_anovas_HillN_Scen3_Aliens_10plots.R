#################################
# Scenario 3                    # 
# Summarize species diversity   #
# 1) only aliens                #
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

HillN_Aliens<-read.csv("Cleaned_Data/Scen3_Aliens_10plots_HillN.csv")

HillN_Aliens$geo_entity2<-as.character(HillN_Aliens$geo_entity2)
HillN_Aliens$geo_entity2<-ifelse(HillN_Aliens$geo_entity2=="O'ahu Island (incl. Mokoli'i Islet)","O'ahu",HillN_Aliens$geo_entity2)
HillN_Aliens$geo_entity2<-ifelse(HillN_Aliens$geo_entity2=="O'ahu Island","O'ahu",HillN_Aliens$geo_entity2)
HillN_Aliens$geo_entity2<-ifelse(HillN_Aliens$geo_entity2=="Hawai'i Island","Hawai'i",HillN_Aliens$geo_entity2)
HillN_Aliens$geo_entity2<-ifelse(HillN_Aliens$geo_entity2=="Kaua'i Island","Kaua'i",HillN_Aliens$geo_entity2)

HillN_Aliens$geo_entity2<-as.factor(HillN_Aliens$geo_entity2)
HillN_Aliens$geo_entity2<-factor(HillN_Aliens$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

################
# Aliens spp.  #
################
# order 0

HillN_Aliens_q0<-filter(HillN_Aliens, m==10000 & order==0)

# visual normality check

qqnorm(HillN_Aliens_q0$qD) # ok

# lm

lm_HillN_Aliens_q0<-lm(qD~ geo_entity2, data=HillN_Aliens_q0)

Scen3_AliensSpp_q0_r2<-summary(lm_HillN_Aliens_q0)$r.squared
Scen3_AliensSpp_q0_modelsumm<-tidy(Anova(lm_HillN_Aliens_q0,test.statistic="F"))
Scen3_AliensSpp_q0_modelpred<-ggeffect(lm_HillN_Aliens_q0, terms="geo_entity2", x.as.factor = T)

# q2

HillN_Aliens_q2<-filter(HillN_Aliens, m==10000 & order==2)

# visual normality check

qqnorm(HillN_Aliens_q2$qD) # ok

# lm

lm_HillN_Aliens_q2<-lm(qD~ geo_entity2, data=HillN_Aliens_q2)

Scen3_AliensSpp_q2_r2<-summary(lm_HillN_Aliens_q2)$r.squared
Scen3_AliensSpp_q2_modelsumm<-tidy(Anova(lm_HillN_Aliens_q2,test.statistic="F"))
Scen3_AliensSpp_q2_modelpred<-ggeffect(lm_HillN_Aliens_q2, terms="geo_entity2", x.as.factor = T)


#######
# out #
#######
save(Scen3_AliensSpp_q0_r2,Scen3_AliensSpp_q0_modelsumm,Scen3_AliensSpp_q0_modelpred, 
     Scen3_AliensSpp_q2_r2,Scen3_AliensSpp_q2_modelsumm,Scen3_AliensSpp_q2_modelpred, 
     file="Cleaned_Data/Scen3_Aliens_HillN_anova_summary.RData")