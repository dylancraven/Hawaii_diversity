##################
# fit GAMMs to ###
# RADs ###########
##################
# Natives       ##
# 5 plots       ##
##################

require(dplyr)
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
# Scenario 2 #
##############

rad.tog2<-read.csv("Cleaned_Data/Scen2_Natives_5plots_RADs.csv",sep=",",header=T)

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
pred_mn<-cbind.data.frame(rad_MN,data.frame(predict(fit.mn$gam, rad_MN,se.fit=TRUE)))

# Simultaneous intervals
Vb <- vcov(fit.mn$gam)
se.fit<-pred_mn$se.fit
set.seed(42)
N <- 10000
BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
Cg <- predict(fit.mn$gam, rad_MN, type = "lpmatrix")
simDev <- Cg %*% t(BUdiff)
absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
masd <- apply(absDev, 2L, max)
crit <- quantile(masd, prob = 0.95, type = 8)

pred_mn$uprS<-pred_mn$fit+ (crit*pred_mn$se.fit)
pred_mn$lwrS<-pred_mn$fit- (crit*pred_mn$se.fit)

# Hawaii

fit.ha<-gamm4(Rank_N~s(logRelAbund), random=~(1|iteration),REML=TRUE,data=rad_HA)
pred_ha<-cbind.data.frame(rad_HA,data.frame(predict(fit.ha$gam, rad_HA,se.fit=TRUE)))

# Simultaneous intervals
Vb <- vcov(fit.ha$gam)
se.fit<-pred_ha$se.fit
set.seed(43)
N <- 10000
BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
Cg <- predict(fit.ha$gam, rad_HA, type = "lpmatrix")
simDev <- Cg %*% t(BUdiff)
absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
masd <- apply(absDev, 2L, max)
crit <- quantile(masd, prob = 0.95, type = 8)

pred_ha$uprS<-pred_ha$fit+ (crit*pred_ha$se.fit)
pred_ha$lwrS<-pred_ha$fit- (crit*pred_ha$se.fit)

#Kauai

fit.ka<-gamm4(Rank_N~s(logRelAbund), random=~(1|iteration),REML=TRUE,data=rad_KA)
pred_ka<-cbind.data.frame(rad_KA,data.frame(predict(fit.ka$gam, rad_KA,se.fit=TRUE)))

# Simultaneous intervals
Vb <- vcov(fit.ka$gam)
se.fit<-pred_ka$se.fit
set.seed(44)
N <- 10000
BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
Cg <- predict(fit.ka$gam, rad_KA, type = "lpmatrix")
simDev <- Cg %*% t(BUdiff)
absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
masd <- apply(absDev, 2L, max)
crit <- quantile(masd, prob = 0.95, type = 8)

pred_ka$uprS<-pred_ka$fit+ (crit*pred_ka$se.fit)
pred_ka$lwrS<-pred_ka$fit- (crit*pred_ka$se.fit)

# Oahu

fit.oa<-gamm4(Rank_N~s(logRelAbund), random=~(1|iteration),REML=TRUE,data=rad_OA)
pred_oa<-cbind.data.frame(rad_OA,data.frame(predict(fit.oa$gam, rad_OA,se.fit=TRUE)))

# Simultaneous intervals
Vb <- vcov(fit.oa$gam)
se.fit<-pred_oa$se.fit
set.seed(45)
N <- 10000
BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
Cg <- predict(fit.oa$gam, rad_OA, type = "lpmatrix")
simDev <- Cg %*% t(BUdiff)
absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
masd <- apply(absDev, 2L, max)
crit <- quantile(masd, prob = 0.95, type = 8)

pred_oa$uprS<-pred_oa$fit+ (crit*pred_oa$se.fit)
pred_oa$lwrS<-pred_oa$fit- (crit*pred_oa$se.fit)

# merge

rad_tog2<-rbind.data.frame(pred_mn,pred_ha, pred_ka, pred_oa)
rad_tog2<-dplyr::select(rad_tog2,-logRelAbund)
#colnames(rad_tog2)[7]<-"RankN_pred"

write.table(rad_tog2,"Cleaned_Data/Scen2_Natives_5plots_RAD_GAMMestimates.csv",sep=",",row.names=T)

##########
# SCEN 3 #
##########

rad.tog3<-read.csv("Cleaned_Data/Scen3_Natives_5plots_RAD.csv",sep=",",header=T)

rad.tog3$iteration<-as.factor(rad.tog3$iteration)

rad.tog3$logRelAbund<-log(rad.tog3$RelAbund)

rad_MN2<-filter(rad.tog3,geo_entity2=="Maui Nui")
rad_MN2$geo_entity2<-droplevels(rad_MN2$geo_entity2)

rad_HA2<-filter(rad.tog3,geo_entity2=="Hawai'i Island")
rad_HA2$geo_entity2<-droplevels(rad_HA2$geo_entity2)

rad_KA2<-filter(rad.tog3,geo_entity2=="Kaua'i Island")
rad_KA2$geo_entity2<-droplevels(rad_KA2$geo_entity2)

rad_OA2<-filter(rad.tog3,geo_entity2=="O'ahu Island")
rad_OA2$geo_entity2<-droplevels(rad_OA2$geo_entity2)

############
# FIT GAMM #
############

##################
#for each island #
##################

# Maui Nui

fit.mn2<-gamm4(Rank_N~s(logRelAbund), random=~(1|iteration),REML=TRUE,data=rad_MN2)
pred_mn2<-cbind.data.frame(rad_MN2,data.frame(predict(fit.mn2$gam, rad_MN2,se.fit=TRUE)))

# Simultaneous intervals
Vb <- vcov(fit.mn2$gam)
se.fit<-pred_mn2$se.fit
set.seed(52)
N <- 10000
BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
Cg <- predict(fit.mn2$gam, rad_MN2, type = "lpmatrix")
simDev <- Cg %*% t(BUdiff)
absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
masd <- apply(absDev, 2L, max)
crit <- quantile(masd, prob = 0.95, type = 8)

pred_mn2$uprS<-pred_mn2$fit+ (crit*pred_mn2$se.fit)
pred_mn2$lwrS<-pred_mn2$fit- (crit*pred_mn2$se.fit)

# Hawaii

fit.ha2<-gamm4(Rank_N~s(logRelAbund), random=~(1|iteration),REML=TRUE,data=rad_HA2)
pred_ha2<-cbind.data.frame(rad_HA2,data.frame(predict(fit.ha2$gam, rad_HA2,se.fit=TRUE)))

# Simultaneous intervals
Vb <- vcov(fit.ha2$gam)
se.fit<-pred_ha2$se.fit
set.seed(53)
N <- 10000
BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
Cg <- predict(fit.ha2$gam, rad_HA2, type = "lpmatrix")
simDev <- Cg %*% t(BUdiff)
absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
masd <- apply(absDev, 2L, max)
crit <- quantile(masd, prob = 0.95, type = 8)

pred_ha2$uprS<-pred_ha2$fit+ (crit*pred_ha2$se.fit)
pred_ha2$lwrS<-pred_ha2$fit- (crit*pred_ha2$se.fit)

#Kauai

fit.ka2<-gamm4(Rank_N~s(logRelAbund), random=~(1|iteration),REML=TRUE,data=rad_KA2)
pred_ka2<-cbind.data.frame(rad_KA2,data.frame(predict(fit.ka2$gam, rad_KA2,se.fit=TRUE)))

# Simultaneous intervals
Vb <- vcov(fit.ka2$gam)
se.fit<-pred_ka2$se.fit
set.seed(54)
N <- 10000
BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
Cg <- predict(fit.ka2$gam, rad_KA2, type = "lpmatrix")
simDev <- Cg %*% t(BUdiff)
absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
masd <- apply(absDev, 2L, max)
crit <- quantile(masd, prob = 0.95, type = 8)

pred_ka2$uprS<-pred_ka2$fit+ (crit*pred_ka2$se.fit)
pred_ka2$lwrS<-pred_ka2$fit- (crit*pred_ka2$se.fit)

# Oahu

fit.oa2<-gamm4(Rank_N~s(logRelAbund), random=~(1|iteration),REML=TRUE,data=rad_OA2)
pred_oa2<-cbind.data.frame(rad_OA2,data.frame(predict(fit.oa2$gam, rad_OA2,se.fit=TRUE)))

# Simultaneous intervals
Vb <- vcov(fit.oa2$gam)
se.fit<-pred_oa2$se.fit
set.seed(55)
N <- 10000
BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
Cg <- predict(fit.oa2$gam, rad_OA2, type = "lpmatrix")
simDev <- Cg %*% t(BUdiff)
absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
masd <- apply(absDev, 2L, max)
crit <- quantile(masd, prob = 0.95, type = 8)

pred_oa2$uprS<-pred_oa2$fit+ (crit*pred_oa2$se.fit)
pred_oa2$lwrS<-pred_oa2$fit- (crit*pred_oa2$se.fit)

# merge

rad_tog3<-rbind.data.frame(pred_mn2,pred_ha2, pred_ka2, pred_oa2)
rad_tog3<-dplyr::select(rad_tog3,-logRelAbund)
# colnames(rad_tog3)[7]<-"RankN_pred"

write.table(rad_tog3,"Cleaned_Data/Scen3_Natives_5plots_RAD_GAMMestimates.csv",sep=",",row.names=T)
