#################################
# Scenario 2                    # 
# Summarize beta diversity      #
# just Beta S                   #
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

Beta_All<-read.csv("Cleaned_Data/Scen2_Total_10plots_BetaPIE.csv",sep=",",header=T)
Beta_All$Scenario<-"All"

Beta_All$geo_entity2<-as.character(Beta_All$geo_entity2)
Beta_All$geo_entity2<-ifelse(Beta_All$geo_entity2=="O'ahu Island (incl. Mokoli'i Islet)","O'ahu",Beta_All$geo_entity2)
Beta_All$geo_entity2<-ifelse(Beta_All$geo_entity2=="O'ahu Island","O'ahu",Beta_All$geo_entity2)
Beta_All$geo_entity2<-ifelse(Beta_All$geo_entity2=="Hawai'i Island","Hawai'i",Beta_All$geo_entity2)
Beta_All$geo_entity2<-ifelse(Beta_All$geo_entity2=="Kaua'i Island","Kaua'i",Beta_All$geo_entity2)

Beta_All$geo_entity2<-as.factor(Beta_All$geo_entity2)
Beta_All$geo_entity2<-factor(Beta_All$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

# take average of beta diversity per iteration
Beta_All<- Beta_All %>% group_by(Iteration,geo_entity2, index) %>%
          summarize(beta=mean(value))

Beta_N<-read.csv("Cleaned_Data/Scen2_Natives_10plots_BetaPIE.csv",sep=",",header=T)
Beta_N$Scenario<-"Native"

Beta_N$geo_entity2<-as.character(Beta_N$geo_entity2)
Beta_N$geo_entity2<-ifelse(Beta_N$geo_entity2=="O'ahu Island (incl. Mokoli'i Islet)","O'ahu",Beta_N$geo_entity2)
Beta_N$geo_entity2<-ifelse(Beta_N$geo_entity2=="O'ahu Island","O'ahu",Beta_N$geo_entity2)
Beta_N$geo_entity2<-ifelse(Beta_N$geo_entity2=="Hawai'i Island","Hawai'i",Beta_N$geo_entity2)
Beta_N$geo_entity2<-ifelse(Beta_N$geo_entity2=="Kaua'i Island","Kaua'i",Beta_N$geo_entity2)

Beta_N$geo_entity2<-as.factor(Beta_N$geo_entity2)
Beta_N$geo_entity2<-factor(Beta_N$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

# take average of beta diversity per iteration
Beta_N<- Beta_N %>% group_by(Iteration,geo_entity2, index) %>%
  summarize(beta=mean(value))

#############
# All spp.  #
#############

BetaS_All<-filter(Beta_All, index=="beta_S")
BetaS_All$index<-droplevels(BetaS_All$index)

# visual normality check

qqnorm(BetaS_All$beta) # ok

# lm

lm_BetaS_All<-lm(beta~ geo_entity2, data=BetaS_All)

Scen2_AllSpp_BetaS_r2<-summary(lm_BetaS_All)$r.squared
Scen2_AllSpp_BetaS_modelsumm<-tidy(Anova(lm_BetaS_All,test.statistic="F"))
Scen2_AllSpp_BetaS_modelpred<-ggeffect(lm_BetaS_All, terms="geo_entity2", x.as.factor = T)

################
# just natives #
################

BetaS_N<-filter(Beta_N, index=="beta_S")
BetaS_N$index<-droplevels(BetaS_N$index)

# visual normality check

qqnorm(BetaS_N$beta) # ok

# lm

lm_BetaN_All<-lm(beta~ geo_entity2, data=BetaS_N)

Scen2_NativesSpp_BetaS_r2<-summary(lm_BetaN_All)$r.squared
Scen2_NativesSpp_BetaS_modelsumm<-tidy(Anova(lm_BetaN_All,test.statistic="F"))
Scen2_NativesSpp_BetaS_modelpred<-ggeffect(lm_BetaN_All, terms="geo_entity2", x.as.factor = T)

#######
# out #
#######
save(Scen2_AllSpp_BetaS_r2,Scen2_AllSpp_BetaS_modelsumm,Scen2_AllSpp_BetaS_modelpred, 
     Scen2_NativesSpp_BetaS_r2,Scen2_NativesSpp_BetaS_modelsumm,Scen2_NativesSpp_BetaS_modelpred, 
     file="Cleaned_Data/Scen2_All_Native_BetaS_anova_summary.RData")