###############
# All natives #
# 7 plots #####
###############

require(tidyr)
require(dplyr)
require(ggplot2)
require(grid)
require(reshape2)
library(rms)

#colors:  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))
#shapes: scale_shape_manual(values=c(15,16,17)

##################
# Scenario 1 #####
##################

SACs_1<-read.csv("Cleaned_Data/Scen1_Natives_SACs_GAMMestimates.csv")

HillN_1<-read.csv("Cleaned_Data/Scen1_Natives_HillNumbers.csv")

RAD_1<-read.csv("Cleaned_Data/Scen1_Natives_RAD_GAMMestimates.csv")


##################
# Scenario 2 #####
##################

SACs_2<-read.csv("Cleaned_Data/Scen2_Natives_SACs_GAMMestimates.csv")

HillN_2<-read.csv("Cleaned_Data/Scen2_Natives_7plots_HillN.csv")

RAD_2<-read.csv("Cleaned_Data/Scen2_Natives_RAD_GAMMestimates.csv")


##################
# Scenario 3 #####
##################

SACs_3<-read.csv("Cleaned_Data/Scen3_Natives_SACs_GAMMestimates.csv")

HillN_3<-read.csv("Cleaned_Data/Scen3_Natives_7plots_HillN.csv")

RAD_3<-read.csv("Cleaned_Data/Scen3_Natives_RAD_GAMMestimates.csv")

##################
# SACs ###########
##################

# Scenario 1

SACs_1$m<-as.factor(SACs_1$m)

SACs_1$geo_entity2<-as.character(SACs_1$geo_entity2)
SACs_1$geo_entity2<-ifelse(SACs_1$geo_entity2=="Hawai'i Island","Hawai'i",SACs_1$geo_entity2)
SACs_1$geo_entity2<-ifelse(SACs_1$geo_entity2=="Kaua'i Island","Kaua'i",SACs_1$geo_entity2)
SACs_1$geo_entity2<-ifelse(SACs_1$geo_entity2=="O'ahu Island","O'ahu",SACs_1$geo_entity2)
SACs_1$geo_entity2<-as.factor(SACs_1$geo_entity2)

SACs_1$geo_entity2<-as.factor(SACs_1$geo_entity2)
SACs_1$geo_entity2<-factor(SACs_1$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

SACs_11<-ggplot(SACs_1,aes(x=m,y=qD,group=geo_entity2,color=geo_entity2))+ 
  geom_point(size=0.5)+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_x_discrete(breaks=c(1,100,1000,2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000,10000))+
  scale_y_continuous(limits=c(0,53),breaks=c(0,10, 20,30, 40,50))+
  geom_line(size=1)+
  ylab("Species diversity")+xlab("Number of individuals")+
  #scale_color_npg()+
  guides(colour=guide_legend(title="Area+Het+Age",title.position = "top"))+
  theme_bw()+theme(legend.position="top", axis.title.y=element_text(colour="black",face="bold",size=6),
                   axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.text.x=element_text(colour=c("black"),face="bold",size=5),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#Scenario 2

SACs_2$m<-as.factor(SACs_2$m)

SACs_2$geo_entity2<-as.character(SACs_2$geo_entity2)
SACs_2$geo_entity2<-ifelse(SACs_2$geo_entity2=="Hawai'i Island","Hawai'i",SACs_2$geo_entity2)
SACs_2$geo_entity2<-ifelse(SACs_2$geo_entity2=="Kaua'i Island","Kaua'i",SACs_2$geo_entity2)
SACs_2$geo_entity2<-ifelse(SACs_2$geo_entity2=="O'ahu Island","O'ahu",SACs_2$geo_entity2)
SACs_2$geo_entity2<-as.factor(SACs_2$geo_entity2)

SACs_2$geo_entity2<-as.factor(SACs_2$geo_entity2)
SACs_2$geo_entity2<-factor(SACs_2$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

SACs_22<-ggplot(SACs_2,aes(x=m,y=qD,group=geo_entity2,color=geo_entity2))+ 
  geom_point(position = position_jitter(w = 0.02, h = 0),size=0.5,alpha=0.2)+
  geom_line(data=SACs_2,aes(x=m,y=qD_pred,group=geo_entity2,color=geo_entity2),size=1)+
  #geom_smooth(data=SACs_2,aes(x=m,y=qD_pred,group=geo_entity2,color=geo_entity2),size=1,se=FALSE,method="gam",formula=y~s(x))+
  scale_y_continuous(limits=c(0,53),breaks=c(0,10, 20,30, 40,50))+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  ylab("Species diversity")+xlab("Number of individuals")+
  #scale_color_npg()+
  guides(colour=guide_legend(title="Het+Age",title.position = "top"))+
  theme_bw()+theme(legend.position="top", axis.title.y=element_text(colour="black",face="bold",size=6),
                   axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.text.x=element_text(colour=c("black"),face="bold",size=6),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())


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
  geom_point(position = position_jitter(w = 0.02, h = 0),size=0.5,alpha=0.2)+
  geom_line(data=SACs_3,aes(x=m,y=qD_pred,group=geo_entity2,color=geo_entity2),size=1)+
  #geom_smooth(data=SACs_2,aes(x=m,y=qD_pred,group=geo_entity2,color=geo_entity2),size=1,se=FALSE,method="gam",formula=y~s(x))+
  scale_y_continuous(limits=c(0,53),breaks=c(0,10, 20,30, 40,50))+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  ylab("Species diversity")+xlab("Number of individuals")+
  #scale_color_npg()+
  guides(colour=guide_legend(title="Age",title.position = "top"))+
  theme_bw()+theme(legend.position="top", axis.title.y=element_text(colour="black",face="bold",size=6),
                   axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.text.x=element_text(colour=c("black"),face="bold",size=6),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##################
# RADs ###########
##################

##############
#Scenario 1  #
##############

RAD_1$geo_entity2<-as.character(RAD_1$geo_entity2)
RAD_1$geo_entity2<-ifelse(RAD_1$geo_entity2=="Hawai'i Island","Hawai'i",RAD_1$geo_entity2)
RAD_1$geo_entity2<-ifelse(RAD_1$geo_entity2=="Kaua'i Island","Kaua'i",RAD_1$geo_entity2)
RAD_1$geo_entity2<-ifelse(RAD_1$geo_entity2=="O'ahu Island","O'ahu",RAD_1$geo_entity2)
RAD_1$geo_entity2<-as.factor(RAD_1$geo_entity2)

RAD_1$geo_entity2<-as.factor(RAD_1$geo_entity2)
RAD_1$geo_entity2<-factor(RAD_1$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))


raDs_1<-ggplot(RAD_1,aes(y=Rank_N,x=log(RelAbund),group=geo_entity2,color=geo_entity2))+ 
  geom_point(position = position_jitter(w = 0, h = 0.02),size=0.5,alpha=0.9)+
  geom_smooth(data=RAD_1, aes(y=RankN_pred,x=log(RelAbund), group=geo_entity2, color=geo_entity2),method="gam", formula=y~s(x),size=1,se=FALSE)+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
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
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#Scenario 2

RAD_2$geo_entity2<-as.character(RAD_2$geo_entity2)
RAD_2$geo_entity2<-ifelse(RAD_2$geo_entity2=="Hawai'i Island","Hawai'i",RAD_2$geo_entity2)
RAD_2$geo_entity2<-ifelse(RAD_2$geo_entity2=="Kaua'i Island","Kaua'i",RAD_2$geo_entity2)
RAD_2$geo_entity2<-ifelse(RAD_2$geo_entity2=="O'ahu Island","O'ahu",RAD_2$geo_entity2)
RAD_2$geo_entity2<-as.factor(RAD_2$geo_entity2)

RAD_2$geo_entity2<-as.factor(RAD_2$geo_entity2)
RAD_2$geo_entity2<-factor(RAD_2$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))


raDs_2<-ggplot(RAD_2,aes(y=Rank_N,x=log(RelAbund),group=geo_entity2,color=geo_entity2))+ 
  geom_point(position = position_jitter(w = 0, h = 0.02),size=0.5,alpha=0.1)+
  geom_smooth(data=RAD_2, aes(y=RankN_pred,x=log(RelAbund), group=geo_entity2, color=geo_entity2),method="gam", formula=y~s(x),size=1,se=FALSE)+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
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
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#Scenario 3

RAD_3$geo_entity2<-as.character(RAD_3$geo_entity2)
RAD_3$geo_entity2<-ifelse(RAD_3$geo_entity2=="Hawai'i Island","Hawai'i",RAD_3$geo_entity2)
RAD_3$geo_entity2<-ifelse(RAD_3$geo_entity2=="Kaua'i Island","Kaua'i",RAD_3$geo_entity2)
RAD_3$geo_entity2<-ifelse(RAD_3$geo_entity2=="O'ahu Island","O'ahu",RAD_3$geo_entity2)
RAD_3$geo_entity2<-as.factor(RAD_3$geo_entity2)

RAD_3$geo_entity2<-as.factor(RAD_3$geo_entity2)
RAD_3$geo_entity2<-factor(RAD_3$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

raDs_3<-ggplot(RAD_3,aes(y=Rank_N,x=log(RelAbund),group=geo_entity2,color=geo_entity2))+ 
  geom_point(position = position_jitter(w = 0, h = 0.02),size=0.5,alpha=0.1)+
  geom_smooth(data=RAD_3, aes(y=RankN_pred,x=log(RelAbund), group=geo_entity2, color=geo_entity2),method="gam", formula=y~s(x),size=1,se=FALSE)+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
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
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())

############
# merge ####
############

require(cowplot)

# just SACs & RADs

togg2<-plot_grid(SACs_11,SACs_22,SACs_33,raDs_1,raDs_2,raDs_3,
                 labels=c("a)","b)","c)","d)","e)","f)"),label_size = 6,
                 ncol=3)

png(filename="Figures/SACs_RADs_Natives_7plots_Fig2.png", 
    units="in", 
    width=8, 
    height=6, 
    pointsize=2, 
    res=400)

togg2

dev.off()

#############
# Hill N ####
#############

HillN_1$Scenario<-"I"
HillN_1$iteration<-1

HillN_2$Scenario<-"II"

HillN_3$Scenario<-"III"

HillNN<-rbind.data.frame(HillN_1, HillN_2, HillN_3)

HillNN$geo_entity2<-as.character(HillNN$geo_entity2)
HillNN$geo_entity2<-ifelse(HillNN$geo_entity2=="O'ahu Island (incl. Mokoli'i Islet)","O'ahu",HillNN$geo_entity2)
HillNN$geo_entity2<-ifelse(HillNN$geo_entity2=="O'ahu Island","O'ahu",HillNN$geo_entity2)
HillNN$geo_entity2<-ifelse(HillNN$geo_entity2=="Hawai'i Island","Hawai'i",HillNN$geo_entity2)
HillNN$geo_entity2<-ifelse(HillNN$geo_entity2=="Kaua'i Island","Kaua'i",HillNN$geo_entity2)

HillNN$geo_entity2<-as.factor(HillNN$geo_entity2)
HillNN$geo_entity2<-factor(HillNN$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

HillNN$order<-as.factor(HillNN$order)

HillNN$geo_order<-paste(HillNN$geo_entity2,HillNN$order,sep="_")

HillNN$geo_order<-factor(HillNN$geo_order,levels=c("Hawai'i_0","Hawai'i_1","Hawai'i_2","Maui Nui_0","Maui Nui_1","Maui Nui_2",
                                                   "O'ahu_0","O'ahu_1","O'ahu_2","Kaua'i_0","Kaua'i_1","Kaua'i_2"))

###########################################
# summarize data (except for Scenario 1)   #
###########################################

HillNN_minus<-filter(HillNN,Scenario!="I")

HillNN_minus<- HillNN_minus %>%
  group_by(geo_entity2,Scenario, order) %>%
  do(data.frame(rbind(smean.cl.boot(.$qD, B=1000))))

colnames(HillNN_minus)[4]<-"qD"
colnames(HillNN_minus)[5]<-"qD.LCL"
colnames(HillNN_minus)[6]<-"qD.UCL"

Hill_Sc1<-filter(HillNN,Scenario=="I")
Hill_Sc1<-select(Hill_Sc1, geo_entity2, Scenario, order, qD, qD.LCL, qD.UCL)

HillNN_f<-rbind.data.frame(HillNN_minus,Hill_Sc1)

HillNN_f$geo_order<-paste(HillNN_f$geo_entity2,HillNN_f$order,sep="_")

HillNN_f$geo_order<-as.factor(HillNN_f$geo_order)

HillNN_f$geo_order<-factor(HillNN_f$geo_order,levels=c("Hawai'i_0","Hawai'i_1","Hawai'i_2","Maui Nui_0","Maui Nui_1","Maui Nui_2",
                                                       "O'ahu_0","O'ahu_1","O'ahu_2","Kaua'i_0","Kaua'i_1","Kaua'i_2"))


HillNN_f$geo_entity2<-factor(HillNN_f$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

HillNN_f<-ungroup(HillNN_f)

##############################
# order x island x scenario  #
##############################

HillNN_f$Scenario<-as.character(HillNN_f$Scenario)
HillNN_f$Scenario<-ifelse(HillNN_f$Scenario=="I","Area+Het+Age",HillNN_f$Scenario)
HillNN_f$Scenario<-ifelse(HillNN_f$Scenario=="II","Het+Age",HillNN_f$Scenario)
HillNN_f$Scenario<-ifelse(HillNN_f$Scenario=="III","Age",HillNN_f$Scenario)
HillNN_f$Scenario<-as.factor(HillNN_f$Scenario)

HillNN_f$Scenario<-factor(HillNN_f$Scenario,levels=c("Area+Het+Age","Het+Age","Age"))

HillNN_f<- HillNN_f%>%
  unite( "Scen_Order", c("Scenario","order"),remove=FALSE)

HillNN$Scenario<-as.character(HillNN$Scenario)
HillNN$Scenario<-ifelse(HillNN$Scenario=="I","Area+Het+Age",HillNN$Scenario)
HillNN$Scenario<-ifelse(HillNN$Scenario=="II","Het+Age",HillNN$Scenario)
HillNN$Scenario<-ifelse(HillNN$Scenario=="III","Age",HillNN$Scenario)
HillNN$Scenario<-as.factor(HillNN$Scenario)

HillNN$Scenario<-factor(HillNN$Scenario,levels=c("Area+Het+Age","Het+Age","Age"))

HillNN<- HillNN%>%
         unite( "Scen_Order", c("Scenario","order"),remove=FALSE)


# Scen1 + order 0

H_Sc1_Or0<-filter(HillNN, Scen_Order=="Area+Het+Age_0")

H_f_Sc1_Or0<-filter(HillNN_f, Scen_Order=="Area+Het+Age_0")

Hill_Sc1_Or0<-ggplot(H_Sc1_Or0, aes(x=geo_entity2,y=qD,group=geo_entity2,color=geo_entity2))+
  geom_point(data=H_Sc1_Or0, aes(x=geo_entity2,y=qD,color=geo_entity2),position = position_jitter(w = 0.02, h = 0),size=0.25,alpha=0.1)+
  
  geom_point(data=H_f_Sc1_Or0, aes(x=geo_entity2,y=qD,group=geo_entity2, colour=geo_entity2),size=1)+
  geom_errorbar(data=H_f_Sc1_Or0,aes(ymin=qD.LCL,ymax=qD.UCL,group=geo_entity2, colour=geo_entity2),width=0.1)+
  
  #scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_y_continuous(limits=c(0, 55),breaks=c(10,20,30,40,50,60),labels=c("10","20","30","40","50","60"))+
  # scale_shape_manual(values=c(17,15,16))+
  #facet_grid(order~Scenario,scales="free_y")+
  labs(x="",y="Species diversity (q = 0)")+
  guides(colour=guide_legend(title="Area+Het+Age",title.position = "top", hjust=0.5))+
  theme_bw()+theme(plot.title = element_text(colour="black",face="bold",size=7,hjust=0.5,vjust=0),
                  axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_text(colour="black",face="bold",size=6),
                   axis.text.x=element_blank(),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   strip.background = element_rect(fill="transparent",colour="black"),
                   strip.text=element_text(face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                  legend.margin=margin(t=0.00, r=0, b=0, l=0, unit="cm"),
                   legend.position=c("top"),
                   #legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Scen2 + order 0

H_Sc2_Or0<-filter(HillNN, Scen_Order=="Het+Age_0")

H_f_Sc2_Or0<-filter(HillNN_f, Scen_Order=="Het+Age_0")

Hill_Sc2_Or0<-ggplot(H_Sc2_Or0, aes(x=geo_entity2,y=qD,group=geo_entity2,color=geo_entity2))+
  geom_point(data=H_Sc2_Or0, aes(x=geo_entity2,y=qD,color=geo_entity2),position = position_jitter(w = 0.02, h = 0),size=0.25,alpha=0.1)+
  
  geom_point(data=H_f_Sc2_Or0, aes(x=geo_entity2,y=qD,group=geo_entity2, colour=geo_entity2),size=1)+
  geom_errorbar(data=H_f_Sc2_Or0,aes(ymin=qD.LCL,ymax=qD.UCL,group=geo_entity2, colour=geo_entity2),width=0.1)+
  
  #scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_y_continuous(limits=c(0, 55),breaks=c(10,20,30,40,50,60),labels=c("10","20","30","40","50","60"))+
  # scale_shape_manual(values=c(17,15,16))+
  #facet_grid(order~Scenario,scales="free_y")+
  labs(x="",y="Species diversity")+
  guides(colour=guide_legend(title="Het+Age",title.position = "top", hjust=0.5))+
  theme_bw()+theme(plot.title = element_text(colour="black",face="bold",size=7,hjust=0.5,vjust=0),
                   axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_blank(),
                   axis.text.x=element_blank(),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   strip.background = element_rect(fill="transparent",colour="black"),
                   strip.text=element_text(face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.margin=margin(t=0.00, r=0, b=0, l=0, unit="cm"),
                   legend.position=c("top"),
                   #legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# Scen3 + order 0

H_Sc3_Or0<-filter(HillNN, Scen_Order=="Age_0")

H_f_Sc3_Or0<-filter(HillNN_f, Scen_Order=="Age_0")

Hill_Sc3_Or0<-ggplot(H_Sc3_Or0, aes(x=geo_entity2,y=qD,group=geo_entity2,color=geo_entity2))+
  geom_point(data=H_Sc3_Or0, aes(x=geo_entity2,y=qD,color=geo_entity2),position = position_jitter(w = 0.02, h = 0),size=0.25,alpha=0.1)+
  
  geom_point(data=H_f_Sc3_Or0, aes(x=geo_entity2,y=qD,group=geo_entity2, colour=geo_entity2),size=1)+
  geom_errorbar(data=H_f_Sc3_Or0,aes(ymin=qD.LCL,ymax=qD.UCL,group=geo_entity2, colour=geo_entity2),width=0.1)+
  
  #scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_y_continuous(limits=c(0, 55),breaks=c(10,20,30,40,50,60),labels=c("10","20","30","40","50","60"))+
  # scale_shape_manual(values=c(17,15,16))+
  #facet_grid(order~Scenario,scales="free_y")+
  labs(x="",y="Species diversity")+
  guides(colour=guide_legend(title="Age",title.position = "top", hjust=0.5))+
  theme_bw()+theme(plot.title = element_text(colour="black",face="bold",size=7,hjust=0.5,vjust=0),
                   axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_blank(),
                   axis.text.x=element_blank(),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   strip.background = element_rect(fill="transparent",colour="black"),
                   strip.text=element_text(face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.margin=margin(t=0.00, r=0, b=0, l=0, unit="cm"),
                   legend.position=c("top"),
                   #legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Scen1 + order 1

H_Sc1_Or1<-filter(HillNN, Scen_Order=="Area+Het+Age_1")

H_f_Sc1_Or1<-filter(HillNN_f, Scen_Order=="Area+Het+Age_1")

Hill_Sc1_Or1<-ggplot(H_Sc1_Or1, aes(x=geo_entity2,y=qD,group=geo_entity2,color=geo_entity2))+
  geom_point(data=H_Sc1_Or1, aes(x=geo_entity2,y=qD,color=geo_entity2),position = position_jitter(w = 0.02, h = 0),size=0.25,alpha=0.1)+
  
  geom_point(data=H_f_Sc1_Or1, aes(x=geo_entity2,y=qD,group=geo_entity2, colour=geo_entity2),size=1)+
  geom_errorbar(data=H_f_Sc1_Or1,aes(ymin=qD.LCL,ymax=qD.UCL,group=geo_entity2, colour=geo_entity2),width=0.1)+
  
  #scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_y_continuous(limits=c(0, 15),breaks=c(5,10,15),labels=c("5","10","15"))+
  # scale_shape_manual(values=c(17,15,16))+
  #facet_grid(order~Scenario,scales="free_y")+
  labs(x="",y="Species diversity (q = 1)")+
  guides(colour=guide_legend(title="",title.position = "top", hjust=0.5))+
  theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_text(colour="black",face="bold",size=6),
                   axis.text.x=element_blank(),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   strip.background = element_rect(fill="transparent",colour="black"),
                   strip.text=element_text(face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.margin=margin(t=0.05, r=0, b=0, l=0, unit="cm"),
                   legend.position=c("none"),
                   #legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Scen2 + order 1

H_Sc2_Or1<-filter(HillNN, Scen_Order=="Het+Age_1")

H_f_Sc2_Or1<-filter(HillNN_f, Scen_Order=="Het+Age_1")

Hill_Sc2_Or1<-ggplot(H_Sc2_Or1, aes(x=geo_entity2,y=qD,group=geo_entity2,color=geo_entity2))+
  geom_point(data=H_Sc2_Or1, aes(x=geo_entity2,y=qD,color=geo_entity2),position = position_jitter(w = 0.02, h = 0),size=0.25,alpha=0.1)+
  
  geom_point(data=H_f_Sc2_Or1, aes(x=geo_entity2,y=qD,group=geo_entity2, colour=geo_entity2),size=1)+
  geom_errorbar(data=H_f_Sc2_Or1,aes(ymin=qD.LCL,ymax=qD.UCL,group=geo_entity2, colour=geo_entity2),width=0.1)+
  
  #scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_y_continuous(limits=c(0, 15),breaks=c(5,10,15),labels=c("5","10","15"))+
  # scale_shape_manual(values=c(17,15,16))+
  #facet_grid(order~Scenario,scales="free_y")+
  labs(x="",y="Species diversity")+
  guides(colour=guide_legend(title="",title.position = "top", hjust=0.5))+
  theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_blank(),
                   axis.text.x=element_blank(),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   strip.background = element_rect(fill="transparent",colour="black"),
                   strip.text=element_text(face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.margin=margin(t=0.05, r=0, b=0, l=0, unit="cm"),
                   legend.position=c("none"),
                   #legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Scen3 + order 1

H_Sc3_Or1<-filter(HillNN, Scen_Order=="Age_1")

H_f_Sc3_Or1<-filter(HillNN_f, Scen_Order=="Age_1")

Hill_Sc3_Or1<-ggplot(H_Sc3_Or1, aes(x=geo_entity2,y=qD,group=geo_entity2,color=geo_entity2))+
  geom_point(data=H_Sc3_Or1, aes(x=geo_entity2,y=qD,color=geo_entity2),position = position_jitter(w = 0.02, h = 0),size=0.25,alpha=0.1)+
  
  geom_point(data=H_f_Sc3_Or1, aes(x=geo_entity2,y=qD,group=geo_entity2, colour=geo_entity2),size=1)+
  geom_errorbar(data=H_f_Sc3_Or1,aes(ymin=qD.LCL,ymax=qD.UCL,group=geo_entity2, colour=geo_entity2),width=0.1)+
  
  #scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_y_continuous(limits=c(0, 16),breaks=c(5,10,15),labels=c("5","10","15"))+
  # scale_shape_manual(values=c(17,15,16))+
  #facet_grid(order~Scenario,scales="free_y")+
  labs(x="",y="Species diversity")+
  guides(colour=guide_legend(title="",title.position = "top", hjust=0.5))+
  theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_blank(),
                   axis.text.x=element_blank(),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   strip.background = element_rect(fill="transparent",colour="black"),
                   strip.text=element_text(face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.margin=margin(t=0.05, r=0, b=0, l=0, unit="cm"),
                   legend.position=c("none"),
                   #legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Scen1 + order 2

H_Sc1_Or2<-filter(HillNN, Scen_Order=="Area+Het+Age_2")

H_f_Sc1_Or2<-filter(HillNN_f, Scen_Order=="Area+Het+Age_2")

Hill_Sc1_Or2<-ggplot(H_Sc1_Or2, aes(x=geo_entity2,y=qD,group=geo_entity2,color=geo_entity2))+
  geom_point(data=H_Sc1_Or2, aes(x=geo_entity2,y=qD,color=geo_entity2),position = position_jitter(w = 0.02, h = 0),size=0.25,alpha=0.1)+
  
  geom_point(data=H_f_Sc1_Or2, aes(x=geo_entity2,y=qD,group=geo_entity2, colour=geo_entity2),size=1)+
  geom_errorbar(data=H_f_Sc1_Or2,aes(ymin=qD.LCL,ymax=qD.UCL,group=geo_entity2, colour=geo_entity2),width=0.1)+
  
  #scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_y_continuous(limits=c(0, 10),breaks=c(0,5,10),labels=c("1","5","10"))+
  # scale_shape_manual(values=c(17,15,16))+
  #facet_grid(order~Scenario,scales="free_y")+
  labs(x="",y="Species diversity (q = 2)")+
  guides(colour=guide_legend(title="",title.position = "top", hjust=0.5))+
  theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_text(colour="black",face="bold",size=6),
                   axis.text.x=element_text(colour="black",face="bold",size=6),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   strip.background = element_rect(fill="transparent",colour="black"),
                   strip.text=element_text(face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.margin=margin(t=0.05, r=0, b=0, l=0, unit="cm"),
                   legend.position=c("none"),
                   #legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Scen2 + order 2

H_Sc2_Or2<-filter(HillNN, Scen_Order=="Het+Age_2")

H_f_Sc2_Or2<-filter(HillNN_f, Scen_Order=="Het+Age_2")

Hill_Sc2_Or2<-ggplot(H_Sc2_Or2, aes(x=geo_entity2,y=qD,group=geo_entity2,color=geo_entity2))+
  geom_point(data=H_Sc2_Or2, aes(x=geo_entity2,y=qD,color=geo_entity2),position = position_jitter(w = 0.02, h = 0),size=0.25,alpha=0.1)+
  
  geom_point(data=H_f_Sc2_Or2, aes(x=geo_entity2,y=qD,group=geo_entity2, colour=geo_entity2),size=1)+
  geom_errorbar(data=H_f_Sc2_Or2,aes(ymin=qD.LCL,ymax=qD.UCL,group=geo_entity2, colour=geo_entity2),width=0.1)+
  
  #scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_y_continuous(limits=c(0, 10),breaks=c(0,5,10),labels=c("1","5","10"))+
  # scale_shape_manual(values=c(17,15,16))+
  #facet_grid(order~Scenario,scales="free_y")+
  labs(x="",y="Species diversity")+
  guides(colour=guide_legend(title="",title.position = "top", hjust=0.5))+
  theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_blank(),
                   axis.text.x=element_text(colour="black",face="bold",size=6),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   strip.background = element_rect(fill="transparent",colour="black"),
                   strip.text=element_text(face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.margin=margin(t=0.05, r=0, b=0, l=0, unit="cm"),
                   legend.position=c("none"),
                   #legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Scen3 + order 2

H_Sc3_Or2<-filter(HillNN, Scen_Order=="Age_2")

H_f_Sc3_Or2<-filter(HillNN_f, Scen_Order=="Age_2")

Hill_Sc3_Or2<-ggplot(H_Sc3_Or2, aes(x=geo_entity2,y=qD,group=geo_entity2,color=geo_entity2))+
  geom_point(data=H_Sc3_Or2, aes(x=geo_entity2,y=qD,color=geo_entity2),position = position_jitter(w = 0.02, h = 0),size=0.25,alpha=0.1)+
  
  geom_point(data=H_f_Sc3_Or2, aes(x=geo_entity2,y=qD,group=geo_entity2, colour=geo_entity2),size=1)+
  geom_errorbar(data=H_f_Sc3_Or2,aes(ymin=qD.LCL,ymax=qD.UCL,group=geo_entity2, colour=geo_entity2),width=0.1)+
  
  #scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_y_continuous(limits=c(0, 10),breaks=c(0,5,10),labels=c("1","5","10"))+
  # scale_shape_manual(values=c(17,15,16))+
  #facet_grid(order~Scenario,scales="free_y")+
  labs(x="",y="Species diversity")+
  guides(colour=guide_legend(title="",title.position = "top", hjust=0.5))+
  theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_blank(),
                   axis.text.x=element_text(colour="black",face="bold",size=6),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   strip.background = element_rect(fill="transparent",colour="black"),
                   strip.text=element_text(face="bold",size=6),
                   legend.text=element_text(colour=c("black"),face="bold",size=6),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.margin=margin(t=0.05, r=0, b=0, l=0, unit="cm"),
                   legend.position=c("none"),
                   #legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#################
# put together ##
#################

require(cowplot)

hill_tog<-plot_grid(Hill_Sc1_Or0, Hill_Sc2_Or0, Hill_Sc3_Or0, Hill_Sc1_Or1, Hill_Sc2_Or1, Hill_Sc3_Or1,Hill_Sc1_Or2, Hill_Sc2_Or2, Hill_Sc3_Or2,
                    labels=c("a)","b)","c)","d)","e)","f)","g)","h)","i)"),label_size = 6, ncol=3,rel_heights = c(1.2,1,1))

png(filename="Figures/HillN_Natives_7plots_Fig3.png", 
    units="in", 
    width=7, 
    height=7, 
    pointsize=2, 
    res=500)

hill_tog

dev.off()

