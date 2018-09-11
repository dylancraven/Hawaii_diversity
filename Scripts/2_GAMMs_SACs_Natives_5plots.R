###############
# fit GAMMs ###
# to SACs per #
# iteration ###
###############
# Natives ##### 
# 5 plots #####
# #############

require(dplyr)
require(ggplot2)
require(reshape2)
require(gamm4)

# now include code for 'proper' confidence intervals
# from: https://www.fromthebottomoftheheap.net/2016/12/15/simultaneous-interval-revisited/

rmvn <- function(n, mu, sig) { ## MVN random deviates
  L <- mroot(sig)
  m <- ncol(L)
  t(mu + L %*% matrix(rnorm(m*n), m, n))
}

##############
# Scenario 1 #
##############

# #sac.tog<-read.csv("Cleaned_Data/Scen1_Natives_zoom_curves_estimates.csv",sep=",",header=T)
# 
# sac.tog<-read.csv("Cleaned_Data/Scen1_Natives_curves_estimates.csv",sep=",",header=T)
# 
# sac_MN<-filter(sac.tog,geo_entity2=="Maui Nui")
# sac_MN$geo_entity2<-droplevels(sac_MN$geo_entity2)
# 
# sac_HA<-filter(sac.tog,geo_entity2=="Hawai'i Island")
# sac_HA$geo_entity2<-droplevels(sac_HA$geo_entity2)
# 
# sac_KA<-filter(sac.tog,geo_entity2=="Kaua'i Island")
# sac_KA$geo_entity2<-droplevels(sac_KA$geo_entity2)
# 
# sac_OA<-filter(sac.tog,geo_entity2=="O'ahu Island")
# sac_OA$geo_entity2<-droplevels(sac_OA$geo_entity2)
# 
# ############
# # FIT GAMM #
# ############
# 
# ##################
# #for each island #
# ##################
# 
# # Maui Nui
# 
# fit.mn<-gam(qD~s(m),REML=TRUE,data=sac_MN)
# sac_MN$pred<-predict(fit.mn, sac_MN,type="response")
# 
# # Hawaii
# 
# fit.ha<-gam(qD~s(m),REML=TRUE,data=sac_HA)
# sac_HA$pred<-predict(fit.ha, sac_HA,type="response")
# 
# #Kauai
# s
# fit.ka<-gam(qD~s(m),REML=TRUE,data=sac_KA)
# sac_KA$pred<-predict(fit.ka, sac_KA,type="response")
# 
# # Oahu
# 
# fit.oa<-gam(qD~s(m),REML=TRUE,data=sac_OA)
# sac_OA$pred<-predict(fit.oa, sac_OA,type="response")
# 
# # merge
# 
# sac_togg1<-rbind.data.frame(sac_MN,sac_HA, sac_KA, sac_OA)
# colnames(sac_togg1)[11]<-"qD_pred"
# 
# write.table(sac_togg1,"Cleaned_Data/Scen1_Natives_SACs_GAMMestimates.csv",sep=",",row.names=T)

##############
# Scenario 2 #
##############

sac.tog<-read.csv("Cleaned_Data/Scen2_Natives_5plots_curves_estimates.csv")

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
pred_mn<-cbind.data.frame(sac_MN,data.frame(predict(fit.mn$gam, sac_MN,se.fit=TRUE)))

# Simultaneous intervals
Vb <- vcov(fit.mn$gam)
se.fit<-pred_mn$se.fit
set.seed(42)
N <- 10000
BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
Cg <- predict(fit.mn$gam, sac_MN, type = "lpmatrix")
simDev <- Cg %*% t(BUdiff)
absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
masd <- apply(absDev, 2L, max)
crit <- quantile(masd, prob = 0.95, type = 8)

pred_mn$uprS<-pred_mn$fit+ (crit*pred_mn$se.fit)
pred_mn$lwrS<-pred_mn$fit- (crit*pred_mn$se.fit)

# Hawaii

fit.ha<-gamm4(qD~s(m), random=~(1|iteration),REML=TRUE,data=sac_HA)
pred_ha<-cbind.data.frame(sac_HA,data.frame(predict(fit.ha$gam, sac_HA,se.fit=TRUE)))

# Simultaneous intervals
Vb <- vcov(fit.ha$gam)
se.fit<-pred_ha$se.fit
set.seed(43)
N <- 10000
BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
Cg <- predict(fit.ha$gam, sac_HA, type = "lpmatrix")
simDev <- Cg %*% t(BUdiff)
absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
masd <- apply(absDev, 2L, max)
crit <- quantile(masd, prob = 0.95, type = 8)

pred_ha$uprS<-pred_ha$fit+ (crit*pred_ha$se.fit)
pred_ha$lwrS<-pred_ha$fit- (crit*pred_ha$se.fit)

#Kauai

fit.ka<-gamm4(qD~s(m), random=~(1|iteration),REML=TRUE,data=sac_KA)
pred_ka<-cbind.data.frame(sac_KA,data.frame(predict(fit.ka$gam, sac_KA,se.fit=TRUE)))

# Simultaneous intervals
Vb <- vcov(fit.ka$gam)
se.fit<-pred_ka$se.fit
set.seed(44)
N <- 10000
BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
Cg <- predict(fit.ka$gam, sac_KA, type = "lpmatrix")
simDev <- Cg %*% t(BUdiff)
absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
masd <- apply(absDev, 2L, max)
crit <- quantile(masd, prob = 0.95, type = 8)

pred_ka$uprS<-pred_ka$fit+ (crit*pred_ka$se.fit)
pred_ka$lwrS<-pred_ka$fit- (crit*pred_ka$se.fit)

# Oahu

fit.oa<-gamm4(qD~s(m), random=~(1|iteration),REML=TRUE,data=sac_OA)
pred_oa<-cbind.data.frame(sac_OA,data.frame(predict(fit.oa$gam, sac_OA,se.fit=TRUE)))

# Simultaneous intervals
Vb <- vcov(fit.oa$gam)
se.fit<-pred_oa$se.fit
set.seed(45)
N <- 10000
BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
Cg <- predict(fit.oa$gam, sac_OA, type = "lpmatrix")
simDev <- Cg %*% t(BUdiff)
absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
masd <- apply(absDev, 2L, max)
crit <- quantile(masd, prob = 0.95, type = 8)

pred_oa$uprS<-pred_oa$fit+ (crit*pred_oa$se.fit)
pred_oa$lwrS<-pred_oa$fit- (crit*pred_oa$se.fit)

# merge

sac_togg<-rbind.data.frame(pred_ha,pred_mn, pred_ka, pred_oa)

write.table(sac_togg,"Cleaned_Data/Scen2_Natives_5plots_SACs_GAMMestimates.csv",sep=",",row.names=T)

##########
# SCEN 3 #
##########

sac.tog2<-read.csv("Cleaned_Data/Scen3_Natives_5plots_curves_estimates.csv")

sac.tog2$iteration<-as.factor(sac.tog2$iteration)

sac_MN2<-filter(sac.tog2,geo_entity2=="Maui Nui")
sac_MN2$geo_entity2<-droplevels(sac_MN2$geo_entity2)

sac_HA2<-filter(sac.tog2,geo_entity2=="Hawai'i Island")
sac_HA2$geo_entity2<-droplevels(sac_HA2$geo_entity2)

sac_KA2<-filter(sac.tog2,geo_entity2=="Kaua'i Island")
sac_KA2$geo_entity2<-droplevels(sac_KA2$geo_entity2)

sac_OA2<-filter(sac.tog2,geo_entity2=="O'ahu Island")
sac_OA2$geo_entity2<-droplevels(sac_OA2$geo_entity2)

############
# FIT GAMM #
############

##################
#for each island #
##################

# Maui Nui

fit.mn2<-gamm4(qD~s(m), random=~(1|iteration),REML=TRUE,data=sac_MN2)
pred_mn2<-cbind.data.frame(sac_MN2,data.frame(predict(fit.mn2$gam, sac_MN2,se.fit=TRUE)))

# Simultaneous intervals
Vb <- vcov(fit.mn2$gam)
se.fit<-pred_mn2$se.fit
set.seed(52)
N <- 10000
BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
Cg <- predict(fit.mn2$gam, sac_MN2, type = "lpmatrix")
simDev <- Cg %*% t(BUdiff)
absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
masd <- apply(absDev, 2L, max)
crit <- quantile(masd, prob = 0.95, type = 8)

pred_mn2$uprS<-pred_mn2$fit+ (crit*pred_mn2$se.fit)
pred_mn2$lwrS<-pred_mn2$fit- (crit*pred_mn2$se.fit)

# Hawaii

fit.ha2<-gamm4(qD~s(m), random=~(1|iteration),REML=TRUE,data=sac_HA2)
pred_ha2<-cbind.data.frame(sac_HA2,data.frame(predict(fit.ha2$gam, sac_HA2,se.fit=TRUE)))

# Simultaneous intervals
Vb <- vcov(fit.ha2$gam)
se.fit<-pred_ha2$se.fit
set.seed(53)
N <- 10000
BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
Cg <- predict(fit.ha2$gam, sac_HA2, type = "lpmatrix")
simDev <- Cg %*% t(BUdiff)
absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
masd <- apply(absDev, 2L, max)
crit <- quantile(masd, prob = 0.95, type = 8)

pred_ha2$uprS<-pred_ha2$fit+ (crit*pred_ha2$se.fit)
pred_ha2$lwrS<-pred_ha2$fit- (crit*pred_ha2$se.fit)

#Kauai

fit.ka2<-gamm4(qD~s(m), random=~(1|iteration),REML=TRUE,data=sac_KA2)
pred_ka2<-cbind.data.frame(sac_KA2,data.frame(predict(fit.ka2$gam, sac_KA2,se.fit=TRUE)))

# Simultaneous intervals
Vb <- vcov(fit.ka2$gam)
se.fit<-pred_ka2$se.fit
set.seed(54)
N <- 10000
BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
Cg <- predict(fit.ka2$gam, sac_KA2, type = "lpmatrix")
simDev <- Cg %*% t(BUdiff)
absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
masd <- apply(absDev, 2L, max)
crit <- quantile(masd, prob = 0.95, type = 8)

pred_ka2$uprS<-pred_ka2$fit+ (crit*pred_ka2$se.fit)
pred_ka2$lwrS<-pred_ka2$fit- (crit*pred_ka2$se.fit)

# Oahu

fit.oa2<-gamm4(qD~s(m), random=~(1|iteration),REML=TRUE,data=sac_OA2)
pred_oa2<-cbind.data.frame(sac_OA2,data.frame(predict(fit.oa2$gam, sac_OA2,se.fit=TRUE)))

# Simultaneous intervals
Vb <- vcov(fit.oa2$gam)
se.fit<-pred_oa2$se.fit
set.seed(55)
N <- 10000
BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
Cg <- predict(fit.oa2$gam, sac_OA2, type = "lpmatrix")
simDev <- Cg %*% t(BUdiff)
absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
masd <- apply(absDev, 2L, max)
crit <- quantile(masd, prob = 0.95, type = 8)

pred_oa2$uprS<-pred_oa2$fit+ (crit*pred_oa2$se.fit)
pred_oa2$lwrS<-pred_oa2$fit- (crit*pred_oa2$se.fit)

# merge

sac_togg2<-rbind.data.frame(pred_ha2,pred_mn2, pred_ka2, pred_oa2)

write.table(sac_togg2,"Cleaned_Data/Scen3_Natives_5plots_SACs_GAMMestimates.csv",sep=",",row.names=T)
