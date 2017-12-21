#################
# SACs  #########
#################
# all data      #
# four islands  #
#################

require(dplyr)
require(ggplot2)
require(reshape2)
require(scales)
require(gamm4)
require(grid)

##############
# data #######
##############

load("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Cleaned_Data/Scen1_natives_BIG.RData")

curves$geo_entity2<-as.factor(curves$geo_entity2)

sac_MN<-filter(curves,geo_entity2=="Maui Nui")
sac_MN$geo_entity2<-droplevels(sac_MN$geo_entity2)

sac_HA<-filter(curves,geo_entity2=="Hawai'i Island")
sac_HA$geo_entity2<-droplevels(sac_HA$geo_entity2)

sac_KA<-filter(curves,geo_entity2=="Kaua'i Island")
sac_KA$geo_entity2<-droplevels(sac_KA$geo_entity2)

sac_OA<-filter(curves,geo_entity2=="O'ahu Island")
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

####################
# Figure ###########
####################

sac_togg1$m<-as.factor(sac_togg1$m)

sac_togg1$geo_entity2<-as.character(sac_togg1$geo_entity2)
sac_togg1$geo_entity2<-ifelse(sac_togg1$geo_entity2=="Hawai'i Island","Hawai'i",sac_togg1$geo_entity2)
sac_togg1$geo_entity2<-ifelse(sac_togg1$geo_entity2=="Kaua'i Island","Kaua'i",sac_togg1$geo_entity2)
sac_togg1$geo_entity2<-ifelse(sac_togg1$geo_entity2=="O'ahu Island","O'ahu",sac_togg1$geo_entity2)
sac_togg1$geo_entity2<-as.factor(sac_togg1$geo_entity2)

sac_togg1$geo_entity2<-as.factor(sac_togg1$geo_entity2)
sac_togg1$geo_entity2<-factor(sac_togg1$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

sac_togg1$m<-as.numeric(as.character(sac_togg1$m))

SACs_11<-ggplot(sac_togg1,aes(x=m,y=qD,group=geo_entity2,color=geo_entity2))+ 
  #geom_point(position = position_jitter(w = 0.02, h = 0),size=0.5,alpha=0.3)+
  geom_line(size=0.5)+
  ylab("Species diversity")+xlab("Number of individuals")+
  scale_x_continuous(trans="log",breaks= c(0,10,100,1000,10000,100000,500000),labels=comma)+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  guides(colour=guide_legend(title="",title.position="top",nrow=1))+
  theme_bw()+theme(legend.position="top", axis.title.y=element_text(colour="black",face="bold",size=6),
                   axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.text.x=element_text(colour=c("black"),face="bold",size=6),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.key.size = unit(0.5,"cm"),
                   legend.box="horizontal",
                   legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())


png(filename="/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Figures/SACs_alldata_natives_BIG_SI.png", 
    units="in", 
    height=5, 
    width=7, 
    pointsize=2, 
    res=800)

SACs_11

dev.off()
