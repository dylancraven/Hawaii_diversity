#####################
# SACs & RADs #######
# natives spp only ##
# 10 plots    #######
#####################

require(lme4)
require(MuMIn)
require(car)
require(ggplot2)
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

summ_sac_scen2<-broom::tidy(Anova(a))
sac_scen2_r2<-r.squaredGLMM(a,pj2014 = FALSE)

scatter.smooth(fitted(a),residuals(a, type="pearson"),
                       main="Shannon: Model Structure II",xlab="Fitted Values",ylab="Inner Residuals")
        abline(h=0,col="red")
        
qqnorm(residuals(a))
qqline(residuals(a))
        
qqnorm(unlist(ranef(a)))
qqline(unlist(ranef(a)))

# make predictions

pred <- ggpredict(a, terms = c("m [exp]","geo_entity2"))

pred$group<-as.character(pred$group)
pred$group<-ifelse(pred$group=="Hawai'i Island","Hawai'i",pred$group)
pred$group<-ifelse(pred$group=="Kaua'i Island","Kaua'i",pred$group)
pred$group<-ifelse(pred$group=="O'ahu Island","O'ahu",pred$group)
pred$group<-as.factor(pred$group)

pred$group<-as.factor(pred$group)
pred$group<-factor(pred$group,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

p_scen2<-ggplot(pred, aes(x, predicted,group=group,colour=group,fill=group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,fill=group), colour="transparent", alpha = .3)+
  scale_y_continuous(breaks=c(0,5, 10, 15, 20,25,30))+
  scale_x_continuous(breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000))+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_fill_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  coord_cartesian(ylim=c(0,32))+
  
  ylab("Species diversity")+xlab("Number of individuals")+
  guides(colour=guide_legend(title="Het+Age",title.position = "top"),fill="none")+
  
  theme_bw()+theme(legend.position="top", axis.title.y=element_text(colour="black",face="bold",size=6),
                   axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.text.x=element_text(colour=c("black"),face="bold",size=6),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank())

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

summ_sac_scen3<- broom::tidy(Anova(b))
sac_scen2_r3<-r.squaredGLMM(b,pj2014 = FALSE)

scatter.smooth(fitted(b),residuals(b, type="pearson"),
               main="Shannon: Model Structure II",xlab="Fitted Values",ylab="Inner Residuals")
abline(h=0,col="red")

qqnorm(residuals(b))
qqline(residuals(b))

qqnorm(unlist(ranef(b)))
qqline(unlist(ranef(b)))

# make predictions

pred2 <- ggpredict(b, terms = c("m [exp]","geo_entity2"))

pred2$group<-as.character(pred2$group)
pred2$group<-ifelse(pred2$group=="Hawai'i Island","Hawai'i",pred2$group)
pred2$group<-ifelse(pred2$group=="Kaua'i Island","Kaua'i",pred2$group)
pred2$group<-ifelse(pred2$group=="O'ahu Island","O'ahu",pred2$group)
pred2$group<-as.factor(pred2$group)

pred2$group<-as.factor(pred2$group)
pred2$group<-factor(pred2$group,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

p_scen3<-ggplot(pred2, aes(x, predicted,group=group,colour=group,fill=group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,fill=group), colour="transparent", alpha = .3)+
  scale_y_continuous(breaks=c(0,5, 10, 15, 20,25,30))+
  scale_x_continuous(breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000))+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_fill_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  coord_cartesian(ylim=c(0,32))+
  
  ylab("Species diversity")+xlab("Number of individuals")+
  guides(colour=guide_legend(title="Age",title.position = "top"),fill="none")+
  
  theme_bw()+theme(legend.position="top", axis.title.y=element_text(colour="black",face="bold",size=6),
                   axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.text.x=element_text(colour=c("black"),face="bold",size=6),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank())

#############
# RADs ######
#############

rad.tog2<-read.csv("Cleaned_Data/Scen2_Natives_10plots_RADs.csv",sep=",",header=T)

rad.tog2$iteration<-as.factor(rad.tog2$iteration)

rad.tog2$logRelAbund<-log(rad.tog2$RelAbund)

# fit model

rad2<-lmer(Rank_N~ poly(logRelAbund,3)*geo_entity2+(1|geo_entity2:iteration),
           data=rad.tog2)

summ_rad_scen2<-broom::tidy(Anova(rad2))
rad_scen2_r2<-r.squaredGLMM(rad2,pj2014 = FALSE)

plot(fitted(rad2),residuals(rad2))
abline(h=0,col="red")

qqnorm(residuals(rad2))
qqline(residuals(rad2))

qqnorm(unlist(ranef(rad2)))
qqline(unlist(ranef(rad2)))

# make predictions
pred2 <- ggpredict(rad2, terms = c("logRelAbund [n=8]","geo_entity2"))

pred2$group<-as.character(pred2$group)
pred2$group<-ifelse(pred2$group=="Hawai'i Island","Hawai'i",pred2$group)
pred2$group<-ifelse(pred2$group=="Kaua'i Island","Kaua'i",pred2$group)
pred2$group<-ifelse(pred2$group=="O'ahu Island","O'ahu",pred2$group)
pred2$group<-as.factor(pred2$group)

pred2$group<-as.factor(pred2$group)
pred2$group<-factor(pred2$group,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

rad_scen2<-ggplot(pred2, aes(x, predicted,group=group,colour=group,fill=group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,fill=group), colour="transparent", alpha = .3)+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_fill_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  coord_cartesian(ylim=c(0,1))+
  ylab("% Species")+xlab("% Abundance (log scale)")+
  #scale_color_npg()+
  guides(colour=guide_legend(title=""),fill="none")+
  theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_text(colour="black",face="bold",size=6),
                   axis.text.x=element_text(colour=c("black"),face="bold",size=6),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   legend.text=element_text(colour="black",face="bold",size=6),
                   legend.title.align = 0,
                   legend.position=c("none"),
                   legend.title=element_text(colour="black",face="bold",size=6),
                   legend.key.size = unit(2.5, 'lines'),
                   panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank())

################
# Scenario 3 ###
################

rad.tog3<-read.csv("Cleaned_Data/Scen3_Natives_10plots_RAD.csv",sep=",",header=T)

rad.tog3$iteration<-as.factor(rad.tog3$iteration)
rad.tog3$logRelAbund<-log(rad.tog3$RelAbund)

# fit model

rad3<-lmer(Rank_N~ poly(logRelAbund,3)*geo_entity2+(1|geo_entity2:iteration),
           data=rad.tog3)

summ_rad_scen3<-broom::tidy(Anova(rad3))
rad_scen3_r2<-r.squaredGLMM(rad3,pj2014 = FALSE)

plot(fitted(rad3),residuals(rad3))
abline(h=0,col="red")

qqnorm(residuals(rad3))
qqline(residuals(rad3))

qqnorm(unlist(ranef(rad3)))
qqline(unlist(ranef(rad3)))

# make predictions
pred3 <- ggpredict(rad3, terms = c("logRelAbund [n=8.30]","geo_entity2"))

pred3$group<-as.character(pred3$group)
pred3$group<-ifelse(pred3$group=="Hawai'i Island","Hawai'i",pred3$group)
pred3$group<-ifelse(pred3$group=="Kaua'i Island","Kaua'i",pred3$group)
pred3$group<-ifelse(pred3$group=="O'ahu Island","O'ahu",pred3$group)
pred3$group<-as.factor(pred3$group)

pred3$group<-as.factor(pred3$group)
pred3$group<-factor(pred3$group,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

rad_scen3<-ggplot(pred3, aes(x, predicted,group=group,colour=group,fill=group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,fill=group), colour="transparent", alpha = .3)+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_fill_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  coord_cartesian(ylim=c(0,1))+
  ylab("% Species")+xlab("% Abundance (log scale)")+
  #scale_color_npg()+
  guides(colour=guide_legend(title=""),fill="none")+
  theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_text(colour="black",face="bold",size=6),
                   axis.text.x=element_text(colour=c("black"),face="bold",size=6),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   legend.text=element_text(colour="black",face="bold",size=6),
                   legend.title.align = 0,
                   legend.position=c("none"),
                   legend.title=element_text(colour="black",face="bold",size=6),
                   legend.key.size = unit(2.5, 'lines'),
                   panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank())

########################
# combine all ##########
########################

require(cowplot)

# just SACs & RADs

togg2<-plot_grid(p_scen2,p_scen3,rad_scen2,rad_scen3,
                 labels=c("a)","b)","c)","d)"),label_size = 6,
                 ncol=2)

png(filename="Figures/SACs_RADs_Natives_10plots_Fig3.png", 
    units="in", 
    width=8, 
    height=6, 
    pointsize=2, 
    res=400)

togg2

dev.off()

save(sac_scen2_r2,sac_scen2_r3, summ_sac_scen2,summ_sac_scen3, 
     rad_scen2_r2,rad_scen3_r2, summ_rad_scen2,summ_rad_scen3,file="Cleaned_Data/model_summary_SAC_RAD_Native_10plots.RData")
