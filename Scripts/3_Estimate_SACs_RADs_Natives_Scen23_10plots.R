#####################
# SACs & RADs #######
# natives spp only ##
# 10 plots    #######
#####################

require(lme4)
require(MuMIn)
require(lmerTest)
require(ggeffects)

#################
# SACs ##########
#################
#################
# Scenario 2 ####
#################

sac.tog<-read.csv("Cleaned_Data/Scen2_Natives_10plots_curves_estimates.csv")

sac.tog$iteration<-as.factor(sac.tog$iteration)
sac.tog$geo_entity2<-as.factor(sac.tog$geo_entity2)

sac.tog$geo_entity2<-as.character(sac.tog$geo_entity2)
sac.tog$geo_entity2<-ifelse(sac.tog$geo_entity2=="Hawai'i Island","Hawai'i",sac.tog$geo_entity2)
sac.tog$geo_entity2<-ifelse(sac.tog$geo_entity2=="Kaua'i Island","Kaua'i",sac.tog$geo_entity2)
sac.tog$geo_entity2<-ifelse(sac.tog$geo_entity2=="O'ahu Island","O'ahu",sac.tog$geo_entity2)
sac.tog$geo_entity2<-as.factor(sac.tog$geo_entity2)

sac.tog$geo_entity2<-as.factor(sac.tog$geo_entity2)
sac.tog$geo_entity2<-factor(sac.tog$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

# fit model

a<-lmer(qD~log(m)+geo_entity2+log(m):geo_entity2+(1|geo_entity2:iteration),
        data=sac.tog,control=lmerControl(optimizer="Nelder_Mead"))

summ_sac_scen2<-broom::tidy(anova(a,ddf="Satterthwaite"))
sac_scen2_r2<-r.squaredGLMM(a,pj2014 = FALSE)

# validate assumptions

plot(fitted(a),residuals(a))
abline(h=0,col="red")

qqnorm(residuals(a))
qqline(residuals(a))
        
qqnorm(unlist(ranef(a)))
qqline(unlist(ranef(a)))

# make predictions

pred <- ggpredict(a, terms = c("m [exp]","geo_entity2"), type="fe")

pred$group<-as.character(pred$group)
pred$group<-ifelse(pred$group=="Hawai'i Island","Hawai'i",pred$group)
pred$group<-ifelse(pred$group=="Kaua'i Island","Kaua'i",pred$group)
pred$group<-ifelse(pred$group=="O'ahu Island","O'ahu",pred$group)
pred$group<-as.factor(pred$group)

pred$group<-as.factor(pred$group)
pred$group<-factor(pred$group,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

################
# Scenario 3 ###
################

sac.tog3<-read.csv("Cleaned_Data/Scen3_Natives_10plots_curves_estimates.csv")

sac.tog3$iteration<-as.factor(sac.tog3$iteration)
sac.tog3$geo_entity2<-as.factor(sac.tog3$geo_entity2)

sac.tog3$geo_entity2<-as.character(sac.tog3$geo_entity2)
sac.tog3$geo_entity2<-ifelse(sac.tog3$geo_entity2=="Hawai'i Island","Hawai'i",sac.tog3$geo_entity2)
sac.tog3$geo_entity2<-ifelse(sac.tog3$geo_entity2=="Kaua'i Island","Kaua'i",sac.tog3$geo_entity2)
sac.tog3$geo_entity2<-ifelse(sac.tog3$geo_entity2=="O'ahu Island","O'ahu",sac.tog3$geo_entity2)
sac.tog3$geo_entity2<-as.factor(sac.tog3$geo_entity2)

sac.tog3$geo_entity2<-as.factor(sac.tog3$geo_entity2)
sac.tog3$geo_entity2<-factor(sac.tog3$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

# fit model

b<-lmer(qD~log(m)+geo_entity2+log(m):geo_entity2+(1|geo_entity2:iteration),
        data=sac.tog3,control=lmerControl(optimizer="Nelder_Mead"))

summ_sac_scen3<-broom::tidy(anova(b,ddf="Satterthwaite"))
sac_scen2_r3<-r.squaredGLMM(b,pj2014 = FALSE)

plot(fitted(b),residuals(b))
abline(h=0,col="red")

qqnorm(residuals(b))
qqline(residuals(b))

qqnorm(unlist(ranef(b)))
qqline(unlist(ranef(b)))

# make predictions

pred2 <- ggpredict(b, terms = c("m [exp]","geo_entity2"),type="fe")

pred2$group<-as.character(pred2$group)
pred2$group<-ifelse(pred2$group=="Hawai'i Island","Hawai'i",pred2$group)
pred2$group<-ifelse(pred2$group=="Kaua'i Island","Kaua'i",pred2$group)
pred2$group<-ifelse(pred2$group=="O'ahu Island","O'ahu",pred2$group)
pred2$group<-as.factor(pred2$group)

pred2$group<-as.factor(pred2$group)
pred2$group<-factor(pred2$group,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

#############
# RADs ######
#############

rad.tog2<-read.csv("Cleaned_Data/Scen2_Natives_10plots_RADs.csv",sep=",",header=T)

rad.tog2$iteration<-as.factor(rad.tog2$iteration)

rad.tog2$logRelAbund<-log(rad.tog2$RelAbund)

# fit model

rad2<-lmer(Rank_N~ poly(logRelAbund,3)*geo_entity2+(1|geo_entity2:iteration),
           data=rad.tog2)

summ_rad_scen2<-broom::tidy(anova(rad2,ddf="Satterthwaite"))
rad_scen2_r2<-r.squaredGLMM(rad2,pj2014 = FALSE)

plot(fitted(rad2),residuals(rad2))
abline(h=0,col="red")

qqnorm(residuals(rad2))
qqline(residuals(rad2))

qqnorm(unlist(ranef(rad2)))
qqline(unlist(ranef(rad2)))

# make predictions
rad_pred2 <- ggpredict(rad2, terms = c("logRelAbund [n=8]","geo_entity2"),type="fe")

rad_pred2$group<-as.character(rad_pred2$group)
rad_pred2$group<-ifelse(rad_pred2$group=="Hawai'i Island","Hawai'i",rad_pred2$group)
rad_pred2$group<-ifelse(rad_pred2$group=="Kaua'i Island","Kaua'i",rad_pred2$group)
rad_pred2$group<-ifelse(rad_pred2$group=="O'ahu Island","O'ahu",rad_pred2$group)
rad_pred2$group<-as.factor(rad_pred2$group)

rad_pred2$group<-as.factor(rad_pred2$group)
rad_pred2$group<-factor(rad_pred2$group,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

################
# Scenario 3 ###
################

rad.tog3<-read.csv("Cleaned_Data/Scen3_Natives_10plots_RAD.csv",sep=",",header=T)

rad.tog3$iteration<-as.factor(rad.tog3$iteration)
rad.tog3$logRelAbund<-log(rad.tog3$RelAbund)

# fit model

rad3<-lmer(Rank_N~ poly(logRelAbund,3)*geo_entity2+(1|geo_entity2:iteration),
           data=rad.tog3)

summ_rad_scen3<-broom::tidy(anova(rad3,ddf="Satterthwaite"))
rad_scen3_r2<-r.squaredGLMM(rad3,pj2014 = FALSE)

plot(fitted(rad3),residuals(rad3))
abline(h=0,col="red")

qqnorm(residuals(rad3))
qqline(residuals(rad3))

qqnorm(unlist(ranef(rad3)))
qqline(unlist(ranef(rad3)))

# make predictions
rad_pred3 <- ggpredict(rad3, terms = c("logRelAbund [n=8.30]","geo_entity2"),type="fe")

rad_pred3$group<-as.character(rad_pred3$group)
rad_pred3$group<-ifelse(rad_pred3$group=="Hawai'i Island","Hawai'i",rad_pred3$group)
rad_pred3$group<-ifelse(rad_pred3$group=="Kaua'i Island","Kaua'i",rad_pred3$group)
rad_pred3$group<-ifelse(rad_pred3$group=="O'ahu Island","O'ahu",rad_pred3$group)
rad_pred3$group<-as.factor(rad_pred3$group)

rad_pred3$group<-as.factor(rad_pred3$group)
rad_pred3$group<-factor(rad_pred3$group,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

########################
# save data   ##########
########################

save(pred, pred2, rad_pred2, rad_pred3,file="Cleaned_Data/modelpredictions_SAC_RAD_Scen23_Native_10plots.RData")

save(sac_scen2_r2,sac_scen2_r3, summ_sac_scen2,summ_sac_scen3, 
     rad_scen2_r2,rad_scen3_r2, summ_rad_scen2,summ_rad_scen3,file="Cleaned_Data/model_summary_SAC_RAD_Native_10plots.RData")
