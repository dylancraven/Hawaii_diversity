###############
# All natives #
# 5 plots #####
###############

require(tidyr)
require(dplyr)
require(ggplot2)
require(grid)
require(reshape2)
library(rms)

#colors:  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))
#shapes: scale_shape_manual(values=c(15,16,17)

# ##################
# # Scenario 1 #####
# ##################
# 
# SACs_1<-read.csv("Cleaned_Data/Scen1_Natives_SACs_GAMMestimates.csv")
# 
# HillN_1<-read.csv("Cleaned_Data/Scen1_Natives_HillNumbers.csv")
# 
# RAD_1<-read.csv("Cleaned_Data/Scen1_Natives_RAD_GAMMestimates.csv")


##################
# Scenario 2 #####
##################

SACs_2<-read.csv("Cleaned_Data/Scen2_Natives_5plots_SACs_GAMMestimates.csv")

#HillN_2<-read.csv("Cleaned_Data/Scen2_Natives_7plots_HillN.csv")

RAD_2<-read.csv("Cleaned_Data/Scen2_Natives_5plots_RAD_GAMMestimates.csv")

##################
# Scenario 3 #####
##################

SACs_3<-read.csv("Cleaned_Data/Scen3_Natives_5plots_SACs_GAMMestimates.csv")

#HillN_3<-read.csv("Cleaned_Data/Scen3_Natives_7plots_HillN.csv")

RAD_3<-read.csv("Cleaned_Data/Scen3_Natives_5plots_RAD_GAMMestimates.csv")

##################
# SACs ###########
##################

# # Scenario 1
# 
# SACs_1$m<-as.factor(SACs_1$m)
# 
# SACs_1$geo_entity2<-as.character(SACs_1$geo_entity2)
# SACs_1$geo_entity2<-ifelse(SACs_1$geo_entity2=="Hawai'i Island","Hawai'i",SACs_1$geo_entity2)
# SACs_1$geo_entity2<-ifelse(SACs_1$geo_entity2=="Kaua'i Island","Kaua'i",SACs_1$geo_entity2)
# SACs_1$geo_entity2<-ifelse(SACs_1$geo_entity2=="O'ahu Island","O'ahu",SACs_1$geo_entity2)
# SACs_1$geo_entity2<-as.factor(SACs_1$geo_entity2)
# 
# SACs_1$geo_entity2<-as.factor(SACs_1$geo_entity2)
# SACs_1$geo_entity2<-factor(SACs_1$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))
# 
# SACs_11<-ggplot(SACs_1,aes(x=m,y=qD,group=geo_entity2,color=geo_entity2))+ 
#   geom_point(size=0.5)+
#   scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
#   scale_x_discrete(breaks=c(1,100,1000,2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000,10000))+
#   scale_y_continuous(limits=c(0,53),breaks=c(0,10, 20,30, 40,50))+
#   geom_line(size=1)+
#   ylab("Species diversity")+xlab("Number of individuals")+
#   #scale_color_npg()+
#   guides(colour=guide_legend(title="Area+Het+Age",title.position = "top"))+
#   theme_bw()+theme(legend.position="top", axis.title.y=element_text(colour="black",face="bold",size=6),
#                    axis.title.x=element_text(colour="black",face="bold",size=6),
#                    axis.text.x=element_text(colour=c("black"),face="bold",size=5),
#                    axis.text.y=element_text(colour=c("black"),face="bold",size=6),
#                    legend.text=element_text(colour=c("black"),face="bold",size=6),
#                    legend.title = element_text(colour=c("black"),face="bold",size=6),
#                    legend.title.align = 0.5,
#                    legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
#                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#Scenario 2

SACs_2$m<-as.factor(SACs_2$m)

SACs_2$geo_entity2<-as.character(SACs_2$geo_entity2)
SACs_2$geo_entity2<-ifelse(SACs_2$geo_entity2=="Hawai'i Island","Hawai'i",SACs_2$geo_entity2)
SACs_2$geo_entity2<-ifelse(SACs_2$geo_entity2=="Kaua'i Island","Kaua'i",SACs_2$geo_entity2)
SACs_2$geo_entity2<-ifelse(SACs_2$geo_entity2=="O'ahu Island","O'ahu",SACs_2$geo_entity2)
SACs_2$geo_entity2<-as.factor(SACs_2$geo_entity2)

SACs_2$geo_entity2<-as.factor(SACs_2$geo_entity2)
SACs_2$geo_entity2<-factor(SACs_2$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

SACs_22<-ggplot(SACs_2,aes(x=m,y=fit,group=geo_entity2,color=geo_entity2))+ 
  #geom_point(position = position_jitter(w = 0.02, h = 0),size=0.5,alpha=0.2)+
  geom_ribbon(data=SACs_2,aes(ymin=lwrS,ymax=uprS,fill=geo_entity2), alpha = 0.3,colour="transparent")+
  geom_line(data=SACs_2,aes(x=m,y=fit,group=geo_entity2,color=geo_entity2),size=1)+
  #geom_smooth(data=SACs_2,aes(x=m,y=qD_pred,group=geo_entity2,color=geo_entity2),size=1,se=FALSE,method="gam",formula=y~s(x))+
  scale_y_continuous(breaks=c(0,5, 10, 15, 20,25,30))+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_fill_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  coord_cartesian(ylim=c(0,30))+
  
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


#Scenario 3

SACs_3$m<-as.factor(SACs_3$m)

SACs_3$geo_entity2<-as.character(SACs_3$geo_entity2)
SACs_3$geo_entity2<-ifelse(SACs_3$geo_entity2=="Hawai'i Island","Hawai'i",SACs_3$geo_entity2)
SACs_3$geo_entity2<-ifelse(SACs_3$geo_entity2=="Kaua'i Island","Kaua'i",SACs_3$geo_entity2)
SACs_3$geo_entity2<-ifelse(SACs_3$geo_entity2=="O'ahu Island","O'ahu",SACs_3$geo_entity2)
SACs_3$geo_entity2<-as.factor(SACs_3$geo_entity2)

SACs_3$geo_entity2<-as.factor(SACs_3$geo_entity2)
SACs_3$geo_entity2<-factor(SACs_3$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))


SACs_33<-ggplot(SACs_3,aes(x=m,y=qD,group=geo_entity2,color=geo_entity2))+ 
  geom_ribbon(data=SACs_3,aes(ymin=lwrS,ymax=uprS,fill=geo_entity2), alpha = 0.3,colour="transparent")+
  geom_line(data=SACs_3,aes(x=m,y=fit,group=geo_entity2,color=geo_entity2),size=1)+
  #geom_smooth(data=SACs_2,aes(x=m,y=qD_pred,group=geo_entity2,color=geo_entity2),size=1,se=FALSE,method="gam",formula=y~s(x))+
  scale_y_continuous(breaks=c(0,5, 10, 15, 20,25,30))+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_fill_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  ylab("Species diversity")+xlab("Number of individuals")+
  #scale_color_npg()+
  coord_cartesian(ylim=c(0,30))+
  guides(colour=guide_legend(title="Age",title.position = "top"), fill="none")+
  theme_bw()+theme(legend.position="top", axis.title.y=element_text(colour="black",face="bold",size=6),
                   axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.text.x=element_text(colour=c("black"),face="bold",size=6),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank())

##################
# RADs ###########
##################

##############
#Scenario 1  #
##############
# 
# RAD_1$geo_entity2<-as.character(RAD_1$geo_entity2)
# RAD_1$geo_entity2<-ifelse(RAD_1$geo_entity2=="Hawai'i Island","Hawai'i",RAD_1$geo_entity2)
# RAD_1$geo_entity2<-ifelse(RAD_1$geo_entity2=="Kaua'i Island","Kaua'i",RAD_1$geo_entity2)
# RAD_1$geo_entity2<-ifelse(RAD_1$geo_entity2=="O'ahu Island","O'ahu",RAD_1$geo_entity2)
# RAD_1$geo_entity2<-as.factor(RAD_1$geo_entity2)
# 
# RAD_1$geo_entity2<-as.factor(RAD_1$geo_entity2)
# RAD_1$geo_entity2<-factor(RAD_1$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))
# 
# 
# raDs_1<-ggplot(RAD_1,aes(y=Rank_N,x=log(RelAbund),group=geo_entity2,color=geo_entity2))+ 
#   geom_point(position = position_jitter(w = 0, h = 0.02),size=0.5,alpha=0.9)+
#   geom_smooth(data=RAD_1, aes(y=RankN_pred,x=log(RelAbund), group=geo_entity2, color=geo_entity2),method="gam", formula=y~s(x),size=1,se=FALSE)+
#   scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
#   ylab("% Species")+xlab("% Abundance (log scale)")+
#   #scale_color_npg()+
#   guides(colour=guide_legend(title=""))+
#   theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=6),
#                    axis.title.y=element_text(colour="black",face="bold",size=6),
#                    axis.text.x=element_text(colour=c("black"),face="bold",size=6),
#                    axis.text.y=element_text(colour=c("black"),face="bold",size=6),
#                    legend.text=element_text(colour="black",face="bold",size=6),
#                    legend.title.align = 0,
#                    legend.position=c("none"),
#                    legend.title=element_text(colour="black",face="bold",size=6),
#                    legend.key.size = unit(2.5, 'lines'),
#                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#Scenario 2

RAD_2$geo_entity2<-as.character(RAD_2$geo_entity2)
RAD_2$geo_entity2<-ifelse(RAD_2$geo_entity2=="Hawai'i Island","Hawai'i",RAD_2$geo_entity2)
RAD_2$geo_entity2<-ifelse(RAD_2$geo_entity2=="Kaua'i Island","Kaua'i",RAD_2$geo_entity2)
RAD_2$geo_entity2<-ifelse(RAD_2$geo_entity2=="O'ahu Island","O'ahu",RAD_2$geo_entity2)
RAD_2$geo_entity2<-as.factor(RAD_2$geo_entity2)

RAD_2$geo_entity2<-as.factor(RAD_2$geo_entity2)
RAD_2$geo_entity2<-factor(RAD_2$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))


raDs_2<-ggplot(RAD_2,aes(y=fit,x=log(RelAbund),group=geo_entity2,color=geo_entity2))+ 
  geom_ribbon(data=RAD_2,aes(ymin=lwrS,ymax=uprS,fill=geo_entity2), alpha = 0.3,colour="transparent")+
  geom_line(data=RAD_2,aes(x=log(RelAbund),y=fit,group=geo_entity2,color=geo_entity2),size=1)+
  # geom_point(position = position_jitter(w = 0, h = 0.02),size=0.5,alpha=0.1)+
  # geom_smooth(data=RAD_2, aes(y=RankN_pred,x=log(RelAbund), group=geo_entity2, color=geo_entity2),method="gam", formula=y~s(x),size=1,se=FALSE)+
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


#Scenario 3

RAD_3$geo_entity2<-as.character(RAD_3$geo_entity2)
RAD_3$geo_entity2<-ifelse(RAD_3$geo_entity2=="Hawai'i Island","Hawai'i",RAD_3$geo_entity2)
RAD_3$geo_entity2<-ifelse(RAD_3$geo_entity2=="Kaua'i Island","Kaua'i",RAD_3$geo_entity2)
RAD_3$geo_entity2<-ifelse(RAD_3$geo_entity2=="O'ahu Island","O'ahu",RAD_3$geo_entity2)
RAD_3$geo_entity2<-as.factor(RAD_3$geo_entity2)

RAD_3$geo_entity2<-as.factor(RAD_3$geo_entity2)
RAD_3$geo_entity2<-factor(RAD_3$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

raDs_3<-ggplot(RAD_3,aes(y=fit,x=log(RelAbund),group=geo_entity2,color=geo_entity2))+ 
  geom_ribbon(data=RAD_3,aes(ymin=lwrS,ymax=uprS,fill=geo_entity2), alpha = 0.3,colour="transparent")+
  geom_line(data=RAD_3,aes(x=log(RelAbund),y=fit,group=geo_entity2,color=geo_entity2),size=1)+
  # geom_point(position = position_jitter(w = 0, h = 0.02),size=0.5,alpha=0.1)+
  # geom_smooth(data=RAD_3, aes(y=RankN_pred,x=log(RelAbund), group=geo_entity2, color=geo_entity2),method="gam", formula=y~s(x),size=1,se=FALSE)+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_fill_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  
  coord_cartesian(ylim=c(0,1))+
  guides(colour=guide_legend(title=""),fill="none")+
  ylab("% Species")+xlab("% Abundance (log scale)")+
  #scale_color_npg()+
  guides(colour=guide_legend(title=""))+
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

############
# merge ####
############

require(cowplot)

# just SACs & RADs

togg2<-plot_grid(SACs_22,SACs_33,raDs_2,raDs_3,
                 labels=c("a)","b)","c)","d)"),label_size = 6,
                 ncol=2)

png(filename="Figures/SACs_RADs_Natives_5plots_Fig2.png", 
    units="in", 
    width=8, 
    height=6, 
    pointsize=2, 
    res=400)

togg2

dev.off()

