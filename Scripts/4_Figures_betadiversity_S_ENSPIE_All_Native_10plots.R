###############
# Beta ENSpie #
###############
# All natives #
# 10 plots #####
###############

require(dplyr)
require(ggplot2)
require(grid)
require(reshape2)
require(rms)

#############
# Data  #####
#############

Beta_N<-read.csv("Cleaned_Data/Scen3_Natives_10plots_BetaPIE.csv",sep=",",header=T)
Beta_N$Scenario<-"Native"

Beta_All<-read.csv("Cleaned_Data/Scen3_Total_10plots_BetaPIE.csv",sep=",",header=T)
Beta_All$Scenario<-"All"

Beta_togg<-rbind.data.frame(Beta_N,Beta_All)

#################
# Beta ENS_PIE  #
#################

Beta_22<-filter(Beta_togg, index=="beta_S_PIE")

Beta_22<-dplyr::summarize(dplyr::group_by(Beta_22, geo_entity2, Scenario, Iteration),beta_ENS_PIE=mean(value))

Beta_22$geo_entity2<-as.character(Beta_22$geo_entity2)
Beta_22$geo_entity2<-ifelse(Beta_22$geo_entity2=="Hawai'i Island","Hawai'i",Beta_22$geo_entity2)
Beta_22$geo_entity2<-ifelse(Beta_22$geo_entity2=="Kaua'i Island","Kaua'i",Beta_22$geo_entity2)
Beta_22$geo_entity2<-ifelse(Beta_22$geo_entity2=="O'ahu Island","O'ahu",Beta_22$geo_entity2)
Beta_22$geo_entity2<-as.factor(Beta_22$geo_entity2)

Beta_22$geo_entity2<-as.factor(Beta_22$geo_entity2)
Beta_22$geo_entity2<-factor(Beta_22$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

Beta_2s<- Beta_22 %>%
  group_by(geo_entity2, Scenario) %>%
  do(data.frame(rbind(smean.cl.boot(.$beta_ENS_PIE, B=1000))))

colnames(Beta_2s)[3]<-"beta_ENS_PIE"
colnames(Beta_2s)[4]<-"l.QD"
colnames(Beta_2s)[5]<-"h.QD"

## figure

Beta_2s$geo_entity2<-as.factor(Beta_2s$geo_entity2)
Beta_2s$geo_entity2<-factor(Beta_2s$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

Beta_ENS_PIE<-ggplot(data=Beta_22, aes(x=geo_entity2,y=beta_ENS_PIE,group=geo_entity2))+
  geom_point(data=Beta_22, aes(x=geo_entity2,y=beta_ENS_PIE,group=geo_entity2),
             position = position_jitter(w = 0.02, h = 0),color="gray80",size=0.25,alpha=0.2)+
  
  geom_point(data=Beta_2s, aes(x=geo_entity2,y=beta_ENS_PIE,group=geo_entity2, colour=Scenario),size=1)+
  geom_errorbar(data=Beta_2s,aes(ymin=l.QD,ymax=h.QD,colour=Scenario),width=0.1)+
  scale_y_continuous(limits=c(0,6),breaks=c(0,1,2,3,4,5,6))+
  #scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("All"="#3C5488FF","Native"="#008975"))+
  labs(x="",y=expression(bold(beta["ENSpie"])))+
  guides(colour=guide_legend(title="",title.position = "top"))+
  theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_text(colour="black",face="bold",size=6),
                   axis.text.x=element_text(colour=c("black"),face="bold",size=6),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.position=c("top"),
                   legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank())

################
## beta_S_PIE  #
################

Beta_33<-filter(Beta_togg, index=="beta_S")

Beta_33<-dplyr::summarize(dplyr::group_by(Beta_33, geo_entity2, Scenario,Iteration),beta_S=mean(value))

Beta_33$geo_entity2<-as.character(Beta_33$geo_entity2)
Beta_33$geo_entity2<-ifelse(Beta_33$geo_entity2=="Hawai'i Island","Hawai'i",Beta_33$geo_entity2)
Beta_33$geo_entity2<-ifelse(Beta_33$geo_entity2=="Kaua'i Island","Kaua'i",Beta_33$geo_entity2)
Beta_33$geo_entity2<-ifelse(Beta_33$geo_entity2=="O'ahu Island","O'ahu",Beta_33$geo_entity2)
Beta_33$geo_entity2<-as.factor(Beta_33$geo_entity2)

Beta_33$geo_entity2<-as.factor(Beta_33$geo_entity2)
Beta_33$geo_entity2<-factor(Beta_33$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

Beta_3s<- Beta_33 %>%
  group_by(geo_entity2, Scenario) %>%
  do(data.frame(rbind(smean.cl.boot(.$beta_S, B=1000))))

colnames(Beta_3s)[3]<-"beta_S"
colnames(Beta_3s)[4]<-"l.QD"
colnames(Beta_3s)[5]<-"h.QD"

Beta_3s$geo_entity2<-as.factor(Beta_3s$geo_entity2)
Beta_3s$geo_entity2<-factor(Beta_3s$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

Beta_S<-ggplot(data=Beta_33, aes(x=geo_entity2,y=beta_S,group=geo_entity2))+
  geom_point(data=Beta_33, aes(x=geo_entity2,y=beta_S,group=geo_entity2),
             position = position_jitter(w = 0.02, h = 0),color="gray80",size=0.25,alpha=0.2)+
  
  geom_point(data=Beta_3s, aes(x=geo_entity2,y=beta_S,group=geo_entity2, colour=Scenario),size=1)+
  geom_errorbar(data=Beta_3s,aes(ymin=l.QD,ymax=h.QD,colour=Scenario),width=0.1)+
  scale_y_continuous(limits=c(0,22),breaks=c(0,5,10,15,20))+
  #scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("All"="#3C5488FF","Native"="#008975"))+
  labs(x="",y=expression(bold(beta["S"])))+
  guides(colour=guide_legend(title="",title.position = "top"))+
  theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_text(colour="black",face="bold",size=6),
                   axis.text.x=element_text(colour=c("black"),face="bold",size=6),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.position=c("top"),
                   legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank())

###################
# merge figures  #
##################

# require(cowplot)
# 
# beta_tog<-plot_grid(Beta_S+theme(legend.position="none", axis.text.x=element_blank()),
#                     Beta_ENS_PIE+theme(legend.position="none"),
#                     labels=c("a)","b)"),label_size = 6,ncol=1, align="vh")

# legend <- get_legend(Beta_S)
# 
# beta_togg<- plot_grid( legend,beta_tog, rel_heights = c(0.4, 5),ncol=1)


png(filename="Figures/Fig5_Beta_S_all_natives_10plots.png", 
    units="in", 
    width=4, 
    height=3, 
    pointsize=2, 
    res=500)


Beta_S

dev.off()
