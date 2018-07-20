###############
# Beta ENSpie #
###############
# All natives #
# 7 plots #####
###############

require(dplyr)
require(ggplot2)
require(grid)
require(reshape2)
require(rms)

#############
# Data  #####
#############

Beta_1<-read.csv("Cleaned_Data/Scen1_Natives_BetaPIE.csv",sep=",",header=T)

Beta_2<-read.csv("Cleaned_Data/Scen2_Natives_7plots_BetaPIE.csv",sep=",",header=T)

Beta_3<-read.csv("Cleaned_Data/Scen3_Natives_7plots_BetaPIE.csv",sep=",",header=T)

#################
# Beta ENS_PIE  #
#################

##############
# Scenario 1 #
##############

Beta_1$geo_entity2<-as.character(Beta_1$geo_entity2)
Beta_1$geo_entity2<-ifelse(Beta_1$geo_entity2=="Hawai'i Island","Hawai'i",Beta_1$geo_entity2)
Beta_1$geo_entity2<-ifelse(Beta_1$geo_entity2=="Kaua'i Island","Kaua'i",Beta_1$geo_entity2)
Beta_1$geo_entity2<-ifelse(Beta_1$geo_entity2=="O'ahu Island","O'ahu",Beta_1$geo_entity2)
Beta_1$geo_entity2<-as.factor(Beta_1$geo_entity2)

Beta_1$geo_entity2<-factor(Beta_1$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

Beta_1<-filter(Beta_1, index=="beta_S_PIE")

Beta_1s<- Beta_1 %>%
  group_by(geo_entity2) %>%
  do(data.frame(rbind(smean.cl.boot(.$value, B=1000))))

colnames(Beta_1s)[2]<-"value"
colnames(Beta_1s)[3]<-"l.QD"
colnames(Beta_1s)[4]<-"h.QD"

Beta_1s$geo_entity2<-as.factor(Beta_1s$geo_entity2)
Beta_1s$geo_entity2<-factor(Beta_1s$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

Beta_Scen1<-ggplot(data=Beta_1, aes(x=geo_entity2,y=value,group=geo_entity2,color=geo_entity2))+
  geom_point(data=Beta_1, aes(x=geo_entity2,y=value,group=geo_entity2,color=geo_entity2),
             position = position_jitter(w = 0.02, h = 0),size=0.5,alpha=0.1)+
  
  geom_point(data=Beta_1s, aes(x=geo_entity2,y=value,group=geo_entity2, colour=geo_entity2),size=2)+
  geom_errorbar(data=Beta_1s,aes(ymin=l.QD,ymax=h.QD),width=0.1)+
  # scale_y_continuous(limits=c(0,8.5))+
  #scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  labs(x="",y=expression(bold(beta["ENSpie"])))+
  guides(colour=guide_legend(title="Area+Het+Age",title.position = "top"))+
  theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_text(colour="black",face="bold",size=6),
                   axis.text.x=element_text(colour=c("black"),face="bold",size=6),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.position=c("none"),
                   legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())

write.csv(Beta_1s,"Cleaned_Data/Summary_Beta_ENS_PIE_Scen1.csv",row.names=F)

## Scenario 2

Beta_2<-filter(Beta_2, index=="beta_S_PIE")

Beta_22<-dplyr::summarize(dplyr::group_by(Beta_2, geo_entity2, Iteration),beta_ENS_PIE=mean(value))

Beta_22$geo_entity2<-as.character(Beta_22$geo_entity2)
Beta_22$geo_entity2<-ifelse(Beta_22$geo_entity2=="Hawai'i Island","Hawai'i",Beta_22$geo_entity2)
Beta_22$geo_entity2<-ifelse(Beta_22$geo_entity2=="Kaua'i Island","Kaua'i",Beta_22$geo_entity2)
Beta_22$geo_entity2<-ifelse(Beta_22$geo_entity2=="O'ahu Island","O'ahu",Beta_22$geo_entity2)
Beta_22$geo_entity2<-as.factor(Beta_22$geo_entity2)

Beta_22$geo_entity2<-as.factor(Beta_22$geo_entity2)
Beta_22$geo_entity2<-factor(Beta_22$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

Beta_2s<- Beta_22 %>%
  group_by(geo_entity2) %>%
  do(data.frame(rbind(smean.cl.boot(.$beta_ENS_PIE, B=1000))))

colnames(Beta_2s)[2]<-"beta_ENS_PIE"
colnames(Beta_2s)[3]<-"l.QD"
colnames(Beta_2s)[4]<-"h.QD"

Beta_2s$geo_entity2<-as.factor(Beta_2s$geo_entity2)
Beta_2s$geo_entity2<-factor(Beta_2s$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

Beta_Scen2<-ggplot(data=Beta_22, aes(x=geo_entity2,y=beta_ENS_PIE,group=geo_entity2,color=geo_entity2))+
  geom_point(data=Beta_22, aes(x=geo_entity2,y=beta_ENS_PIE,group=geo_entity2,color=geo_entity2),
             position = position_jitter(w = 0.02, h = 0),size=0.5,alpha=0.1)+
  
  geom_point(data=Beta_2s, aes(x=geo_entity2,y=beta_ENS_PIE,group=geo_entity2, colour=geo_entity2),size=2)+
  geom_errorbar(data=Beta_2s,aes(ymin=l.QD,ymax=h.QD),width=0.1)+
  #scale_y_continuous(limits=c(0,8.5))+
  #scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  labs(x="",y=expression(bold(beta["ENSpie"])))+
  guides(colour=guide_legend(title="Het+Age",title.position = "top"))+
  theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_blank(),
                   axis.text.x=element_text(colour=c("black"),face="bold",size=6),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.position=c("none"),
                   legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())

write.csv(Beta_2s,"Cleaned_Data/Summary_Beta_ENS_PIE_Scen2.csv",row.names=F)

## Scenario 3

Beta_3<-filter(Beta_3, index=="beta_S_PIE")

Beta_33<-dplyr::summarize(dplyr::group_by(Beta_3, geo_entity2, Iteration),beta_ENS_PIE=mean(value))

Beta_33$geo_entity2<-as.character(Beta_33$geo_entity2)
Beta_33$geo_entity2<-ifelse(Beta_33$geo_entity2=="Hawai'i Island","Hawai'i",Beta_33$geo_entity2)
Beta_33$geo_entity2<-ifelse(Beta_33$geo_entity2=="Kaua'i Island","Kaua'i",Beta_33$geo_entity2)
Beta_33$geo_entity2<-ifelse(Beta_33$geo_entity2=="O'ahu Island","O'ahu",Beta_33$geo_entity2)
Beta_33$geo_entity2<-as.factor(Beta_33$geo_entity2)


Beta_33$geo_entity2<-as.factor(Beta_33$geo_entity2)
Beta_33$geo_entity2<-factor(Beta_33$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

Beta_3s<- Beta_33 %>%
  group_by(geo_entity2) %>%
  do(data.frame(rbind(smean.cl.boot(.$beta_ENS_PIE, B=1000))))

colnames(Beta_3s)[2]<-"beta_ENS_PIE"
colnames(Beta_3s)[3]<-"l.QD"
colnames(Beta_3s)[4]<-"h.QD"

Beta_3s$geo_entity2<-as.factor(Beta_3s$geo_entity2)
Beta_3s$geo_entity2<-factor(Beta_3s$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

Beta_Scen3<-ggplot(data=Beta_33, aes(x=geo_entity2,y=beta_ENS_PIE,group=geo_entity2,color=geo_entity2))+
  geom_point(data=Beta_33, aes(x=geo_entity2,y=beta_ENS_PIE,group=geo_entity2,color=geo_entity2),
             position = position_jitter(w = 0.02, h = 0),size=0.5,alpha=0.1)+
  
  geom_point(data=Beta_3s, aes(x=geo_entity2,y=beta_ENS_PIE,group=geo_entity2, colour=geo_entity2),size=2)+
  geom_errorbar(data=Beta_3s,aes(ymin=l.QD,ymax=h.QD),width=0.1)+
  #scale_y_continuous(limits=c(0,8.5))+
  #scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  labs(x="",y=expression(bold(beta["ENSpie"])))+
  guides(colour=guide_legend(title="Age",title.position = "top"))+
  theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_blank(),
                   axis.text.x=element_text(colour=c("black"),face="bold",size=6),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.position=c("none"),
                   legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())

write.csv(Beta_3s,"Cleaned_Data/Summary_Beta_ENS_PIE_Scen3.csv",row.names=F)

[stop here]
#############
# Beta S ####
#############

##############
# Scenario 1 #
##############

Beta_1$geo_entity2<-as.factor(Beta_1$geo_entity2)
Beta_1$geo_entity2<-factor(Beta_1$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

Beta_1ss<- Beta_1 %>%
  group_by(geo_entity2) %>%
  do(data.frame(rbind(smean.cl.boot(.$beta_S, B=1000))))

colnames(Beta_1ss)[2]<-"beta_S"
colnames(Beta_1ss)[3]<-"l.QD"
colnames(Beta_1ss)[4]<-"h.QD"

Beta_1ss$geo_entity2<-as.factor(Beta_1ss$geo_entity2)
Beta_1ss$geo_entity2<-factor(Beta_1ss$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

Beta_Scen1s<-ggplot(data=Beta_1, aes(x=geo_entity2,y=beta_S,group=geo_entity2,color=geo_entity2))+
  geom_point(data=Beta_1, aes(x=geo_entity2,y=beta_S,group=geo_entity2,color=geo_entity2),
             position = position_jitter(w = 0.02, h = 0),size=0.5,alpha=0.1)+
  
  geom_point(data=Beta_1ss, aes(x=geo_entity2,y=beta_S,group=geo_entity2, colour=geo_entity2),size=2)+
  geom_errorbar(data=Beta_1ss,aes(ymin=l.QD,ymax=h.QD),width=0.1)+
  #scale_y_continuous(limits=c(0,8.5))+
  #scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  labs(x="",y=expression(bold(beta["S"])))+
  guides(colour=guide_legend(title="Area+Het+Age",title.position = "top"))+
  theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_text(colour="black",face="bold",size=6),
                   axis.text.x=element_blank(),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.position=c("top"),
                   legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())

write.csv(Beta_1ss,"Cleaned_Data/Summary_Beta_S_Scen1.csv",row.names=F)

## Scenario 2

Beta_222<-dplyr::summarize(dplyr::group_by(Beta_2, geo_entity2, Iteration),beta_S=mean(beta_S))

Beta_222$geo_entity2<-as.character(Beta_222$geo_entity2)
Beta_222$geo_entity2<-ifelse(Beta_222$geo_entity2=="Hawai'i Island","Hawai'i",Beta_222$geo_entity2)
Beta_222$geo_entity2<-ifelse(Beta_222$geo_entity2=="Kaua'i Island","Kaua'i",Beta_222$geo_entity2)
Beta_222$geo_entity2<-ifelse(Beta_222$geo_entity2=="O'ahu Island","O'ahu",Beta_222$geo_entity2)
Beta_222$geo_entity2<-as.factor(Beta_222$geo_entity2)


Beta_222$geo_entity2<-as.factor(Beta_222$geo_entity2)
Beta_222$geo_entity2<-factor(Beta_222$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))


Beta_2ss<- Beta_222 %>%
  group_by(geo_entity2) %>%
  do(data.frame(rbind(smean.cl.boot(.$beta_S, B=1000))))

colnames(Beta_2ss)[2]<-"beta_S"
colnames(Beta_2ss)[3]<-"l.QD"
colnames(Beta_2ss)[4]<-"h.QD"

Beta_2ss$geo_entity2<-as.factor(Beta_2ss$geo_entity2)
Beta_2ss$geo_entity2<-factor(Beta_2ss$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))


Beta_Scen2s<-ggplot(data=Beta_222, aes(x=geo_entity2,y=beta_S,group=geo_entity2,color=geo_entity2))+
  geom_point(data=Beta_222, aes(x=geo_entity2,y=beta_S,group=geo_entity2,color=geo_entity2),
             position = position_jitter(w = 0.02, h = 0),size=0.5,alpha=0.1)+
  
  geom_point(data=Beta_2ss, aes(x=geo_entity2,y=beta_S,group=geo_entity2, colour=geo_entity2),size=2)+
  geom_errorbar(data=Beta_2ss,aes(ymin=l.QD,ymax=h.QD),width=0.1)+
  #cale_y_continuous(limits=c(0,8.5))+
  # scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  labs(x="",y=expression(bold(beta["S"])))+
  guides(colour=guide_legend(title="Het+Age",title.position = "top"))+
  theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_blank(),
                   axis.text.x=element_blank(),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.position=c("top"),
                   legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())

write.csv(Beta_2ss,"Cleaned_Data/Summary_Beta_S_Scen2.csv",row.names=F)

## Scenario 3

Beta_333<-dplyr::summarize(dplyr::group_by(Beta_3, geo_entity2, Iteration),beta_S=mean(beta_S))

Beta_333$geo_entity2<-as.character(Beta_333$geo_entity2)
Beta_333$geo_entity2<-ifelse(Beta_333$geo_entity2=="Hawai'i Island","Hawai'i",Beta_333$geo_entity2)
Beta_333$geo_entity2<-ifelse(Beta_333$geo_entity2=="Kaua'i Island","Kaua'i",Beta_333$geo_entity2)
Beta_333$geo_entity2<-ifelse(Beta_333$geo_entity2=="O'ahu Island","O'ahu",Beta_333$geo_entity2)
Beta_333$geo_entity2<-as.factor(Beta_333$geo_entity2)

Beta_333$geo_entity2<-as.factor(Beta_333$geo_entity2)
Beta_333$geo_entity2<-factor(Beta_333$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

Beta_3ss<- Beta_333 %>%
  group_by(geo_entity2) %>%
  do(data.frame(rbind(smean.cl.boot(.$beta_S, B=1000))))

colnames(Beta_3ss)[2]<-"beta_S"
colnames(Beta_3ss)[3]<-"l.QD"
colnames(Beta_3ss)[4]<-"h.QD"

Beta_3ss$geo_entity2<-as.factor(Beta_3ss$geo_entity2)
Beta_3ss$geo_entity2<-factor(Beta_3ss$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

Beta_Scen3s<-ggplot(data=Beta_333, aes(x=geo_entity2,y=beta_S,group=geo_entity2,color=geo_entity2))+
  geom_point(data=Beta_333, aes(x=geo_entity2,y=beta_S,group=geo_entity2,color=geo_entity2),
             position = position_jitter(w = 0.02, h = 0),size=0.5,alpha=0.1)+
  
  geom_point(data=Beta_3ss, aes(x=geo_entity2,y=beta_S,group=geo_entity2, colour=geo_entity2),size=2)+
  geom_errorbar(data=Beta_3ss,aes(ymin=l.QD,ymax=h.QD),width=0.1)+
  #scale_y_continuous(limits=c(0,100))+
  #scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  labs(x="",y=expression(bold(beta["S"])))+
  guides(colour=guide_legend(title="Age",title.position = "top"))+
  theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_blank(),
                   axis.text.x=element_blank(),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.position=c("top"),
                   legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())

write.csv(Beta_3ss,"Cleaned_Data/Summary_Beta_S_Scen3.csv",row.names=F)

###################
# merge figures  #
##################

require(cowplot)

togg2<-plot_grid(Beta_Scen1s, Beta_Scen2s, Beta_Scen3s,Beta_Scen1, Beta_Scen2, Beta_Scen3,
                 labels=c("a)","b)","c)","d)","e)","f)"),label_size = 6,
                 ncol=3)


png(filename="Figures/Beta_ENSpie_S_FigSX_natives_7plots.png", 
    units="in", 
    width=8, 
    height=6, 
    pointsize=2, 
    res=400)

togg2

dev.off()

