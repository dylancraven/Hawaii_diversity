#############
# Hill N ####
#############
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

##########
# data  ##
##########

HillN_2<-read.csv("Cleaned_Data/Scen2_Natives_7plots_HillN.csv")
HillN_3<-read.csv("Cleaned_Data/Scen3_Natives_7plots_HillN.csv")

#############
# organize  #
#############

HillN_2$Scenario<-"II"
HillN_3$Scenario<-"III"

HillNN<-rbind.data.frame(HillN_2, HillN_3)

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

############################################
# summarize data                           #
############################################

HillNN_minus<- HillNN %>%
  group_by(geo_entity2,Scenario, order) %>%
  do(data.frame(rbind(smean.cl.boot(.$qD, B=1000))))

colnames(HillNN_minus)[4]<-"qD"
colnames(HillNN_minus)[5]<-"qD.LCL"
colnames(HillNN_minus)[6]<-"qD.UCL"

HillNN_f<-rbind.data.frame(HillNN_minus)

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
HillNN_f$Scenario<-ifelse(HillNN_f$Scenario=="II","Het+Age",HillNN_f$Scenario)
HillNN_f$Scenario<-ifelse(HillNN_f$Scenario=="III","Age",HillNN_f$Scenario)
HillNN_f$Scenario<-as.factor(HillNN_f$Scenario)

HillNN_f$Scenario<-factor(HillNN_f$Scenario,levels=c("Het+Age","Age"))

HillNN_f<- HillNN_f%>%
  unite( "Scen_Order", c("Scenario","order"),remove=FALSE)

HillNN$Scenario<-as.character(HillNN$Scenario)
HillNN$Scenario<-ifelse(HillNN$Scenario=="II","Het+Age",HillNN$Scenario)
HillNN$Scenario<-ifelse(HillNN$Scenario=="III","Age",HillNN$Scenario)
HillNN$Scenario<-as.factor(HillNN$Scenario)

HillNN$Scenario<-factor(HillNN$Scenario,levels=c("Het+Age","Age"))

HillNN<- HillNN%>%
  unite( "Scen_Order", c("Scenario","order"),remove=FALSE)

###################
# Scen2 + order 0 #
###################

H_Sc2_Or0<-filter(HillNN, Scen_Order=="Het+Age_0")

H_f_Sc2_Or0<-filter(HillNN_f, Scen_Order=="Het+Age_0")

Hill_Sc2_Or0<-ggplot(H_Sc2_Or0, aes(x=geo_entity2,y=qD,group=geo_entity2,color=geo_entity2))+
  geom_point(data=H_Sc2_Or0, aes(x=geo_entity2,y=qD,color=geo_entity2),position = position_jitter(w = 0.02, h = 0),size=0.25,alpha=0.2)+
  
  geom_point(data=H_f_Sc2_Or0, aes(x=geo_entity2,y=qD,group=geo_entity2, colour=geo_entity2),size=1)+
  geom_errorbar(data=H_f_Sc2_Or0,aes(ymin=qD.LCL,ymax=qD.UCL,group=geo_entity2, colour=geo_entity2),width=0.1)+
  
  #scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_y_continuous(limits=c(0, 40),breaks=c(0,10,20,30,40),labels=c("0","10","20","30","40"))+
  # scale_shape_manual(values=c(17,15,16))+
  #facet_grid(order~Scenario,scales="free_y")+
  labs(x="",y="Species diversity (q = 0)")+
  guides(colour=guide_legend(title="Het+Age",title.position = "top", hjust=0.5))+
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
                   panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank())


# Scen3 + order 0

H_Sc3_Or0<-filter(HillNN, Scen_Order=="Age_0")

H_f_Sc3_Or0<-filter(HillNN_f, Scen_Order=="Age_0")

Hill_Sc3_Or0<-ggplot(H_Sc3_Or0, aes(x=geo_entity2,y=qD,group=geo_entity2,color=geo_entity2))+
  geom_point(data=H_Sc3_Or0, aes(x=geo_entity2,y=qD,color=geo_entity2),position = position_jitter(w = 0.02, h = 0),size=0.25,alpha=0.2)+
  
  geom_point(data=H_f_Sc3_Or0, aes(x=geo_entity2,y=qD,group=geo_entity2, colour=geo_entity2),size=1)+
  geom_errorbar(data=H_f_Sc3_Or0,aes(ymin=qD.LCL,ymax=qD.UCL,group=geo_entity2, colour=geo_entity2),width=0.1)+
  
  #scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_y_continuous(limits=c(0, 40),breaks=c(0,10,20,30,40),labels=c("0","10","20","30","40"))+
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
                   panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank())


# Scen2 + order 1

H_Sc2_Or1<-filter(HillNN, Scen_Order=="Het+Age_1")

H_f_Sc2_Or1<-filter(HillNN_f, Scen_Order=="Het+Age_1")

Hill_Sc2_Or1<-ggplot(H_Sc2_Or1, aes(x=geo_entity2,y=qD,group=geo_entity2,color=geo_entity2))+
  geom_point(data=H_Sc2_Or1, aes(x=geo_entity2,y=qD,color=geo_entity2),position = position_jitter(w = 0.02, h = 0),size=0.25,alpha=0.2)+
  
  geom_point(data=H_f_Sc2_Or1, aes(x=geo_entity2,y=qD,group=geo_entity2, colour=geo_entity2),size=1)+
  geom_errorbar(data=H_f_Sc2_Or1,aes(ymin=qD.LCL,ymax=qD.UCL,group=geo_entity2, colour=geo_entity2),width=0.1)+
  
  #scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_y_continuous(limits=c(0, 10),breaks=c(0,5,10),labels=c("0","5","10"))+
  # scale_shape_manual(values=c(17,15,16))+
  #facet_grid(order~Scenario,scales="free_y")+
  labs(x="",y="Species diversity  (q = 1)")+
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
                   panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank())

# Scen3 + order 1

H_Sc3_Or1<-filter(HillNN, Scen_Order=="Age_1")

H_f_Sc3_Or1<-filter(HillNN_f, Scen_Order=="Age_1")

Hill_Sc3_Or1<-ggplot(H_Sc3_Or1, aes(x=geo_entity2,y=qD,group=geo_entity2,color=geo_entity2))+
  geom_point(data=H_Sc3_Or1, aes(x=geo_entity2,y=qD,color=geo_entity2),position = position_jitter(w = 0.02, h = 0),size=0.25,alpha=0.2)+
  
  geom_point(data=H_f_Sc3_Or1, aes(x=geo_entity2,y=qD,group=geo_entity2, colour=geo_entity2),size=1)+
  geom_errorbar(data=H_f_Sc3_Or1,aes(ymin=qD.LCL,ymax=qD.UCL,group=geo_entity2, colour=geo_entity2),width=0.1)+
  
  #scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_y_continuous(limits=c(0, 10),breaks=c(0,5,10),labels=c("0","5","10"))+
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
                   panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank())


# Scen2 + order 2

H_Sc2_Or2<-filter(HillNN, Scen_Order=="Het+Age_2")

H_f_Sc2_Or2<-filter(HillNN_f, Scen_Order=="Het+Age_2")

Hill_Sc2_Or2<-ggplot(H_Sc2_Or2, aes(x=geo_entity2,y=qD,group=geo_entity2,color=geo_entity2))+
  geom_point(data=H_Sc2_Or2, aes(x=geo_entity2,y=qD,color=geo_entity2),position = position_jitter(w = 0.02, h = 0),size=0.25,alpha=0.2)+
  
  geom_point(data=H_f_Sc2_Or2, aes(x=geo_entity2,y=qD,group=geo_entity2, colour=geo_entity2),size=1)+
  geom_errorbar(data=H_f_Sc2_Or2,aes(ymin=qD.LCL,ymax=qD.UCL,group=geo_entity2, colour=geo_entity2),width=0.1)+
  
  #scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_y_continuous(limits=c(0, 8.2),breaks=c(0,5,10),labels=c("0","5","10"))+
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
                   panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank())

# Scen3 + order 2

H_Sc3_Or2<-filter(HillNN, Scen_Order=="Age_2")

H_f_Sc3_Or2<-filter(HillNN_f, Scen_Order=="Age_2")

Hill_Sc3_Or2<-ggplot(H_Sc3_Or2, aes(x=geo_entity2,y=qD,group=geo_entity2,color=geo_entity2))+
  geom_point(data=H_Sc3_Or2, aes(x=geo_entity2,y=qD,color=geo_entity2),position = position_jitter(w = 0.02, h = 0),size=0.25,alpha=0.2)+
  
  geom_point(data=H_f_Sc3_Or2, aes(x=geo_entity2,y=qD,group=geo_entity2, colour=geo_entity2),size=1)+
  geom_errorbar(data=H_f_Sc3_Or2,aes(ymin=qD.LCL,ymax=qD.UCL,group=geo_entity2, colour=geo_entity2),width=0.1)+
  
  #scale_color_d3(palette="category20c")+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_y_continuous(limits=c(0, 8.2),breaks=c(0,5,10),labels=c("0","5","10"))+
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
                   panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank())


# write out 95 CIs

HillNN_f<-arrange(HillNN_f, order, Scenario)

write.csv(HillNN_f,"Cleaned_Data/summaryHillNumbers_7plots.csv",row.names = FALSE)

#################
# put together ##
#################

require(cowplot)

hill_tog<-plot_grid(Hill_Sc2_Or0, Hill_Sc3_Or0, Hill_Sc2_Or1, Hill_Sc3_Or1,Hill_Sc2_Or2, Hill_Sc3_Or2,
                    labels=c("a)","b)","c)","d)","e)","f)"),label_size = 6, ncol=2,rel_heights = c(1.2,1,1))

png(filename="Figures/HillN_Natives_7plots_Fig4.png", 
    units="in", 
    width=7, 
    height=7, 
    pointsize=2, 
    res=500)

hill_tog

dev.off()
