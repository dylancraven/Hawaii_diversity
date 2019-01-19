#############
# Hill N ####
#############
# Scenario 3: 
# Compare all vs natives
###############
# All natives #
# 10 plots #####
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

HillN_All<-read.csv("Cleaned_Data/Scen3_Total_10plots_HillN.csv")
HillN_Natives<-read.csv("Cleaned_Data/Scen3_Natives_10plots_HillN.csv")

#############
# organize  #
#############

HillN_All$Scenario<-"All"
HillN_Natives$Scenario<-"Native"

HillNN<-rbind.data.frame(HillN_All, HillN_Natives)

HillNN$geo_entity2<-as.character(HillNN$geo_entity2)
HillNN$geo_entity2<-ifelse(HillNN$geo_entity2=="O'ahu Island (incl. Mokoli'i Islet)","O'ahu",HillNN$geo_entity2)
HillNN$geo_entity2<-ifelse(HillNN$geo_entity2=="O'ahu Island","O'ahu",HillNN$geo_entity2)
HillNN$geo_entity2<-ifelse(HillNN$geo_entity2=="Hawai'i Island","Hawai'i",HillNN$geo_entity2)
HillNN$geo_entity2<-ifelse(HillNN$geo_entity2=="Kaua'i Island","Kaua'i",HillNN$geo_entity2)

HillNN$geo_entity2<-as.factor(HillNN$geo_entity2)
HillNN$geo_entity2<-factor(HillNN$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

HillNN$geo_order<-paste(HillNN$geo_entity2,HillNN$order,sep="_")

HillNN$geo_order<-factor(HillNN$geo_order,levels=c("Hawai'i_0","Hawai'i_1","Hawai'i_2","Maui Nui_0","Maui Nui_1","Maui Nui_2",
                                                   "O'ahu_0","O'ahu_1","O'ahu_2","Kaua'i_0","Kaua'i_1","Kaua'i_2"))

############################################
# summarize data                           #
############################################

HillNN<-filter(HillNN, m==10000)

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

HillNN_f$Scenario<-factor(HillNN_f$Scenario,levels=c("All","Native"))

HillNN_f<- HillNN_f%>%
  unite( "Scen_Order", c("Scenario","order"),remove=FALSE)

HillNN$Scenario<-factor(HillNN$Scenario,levels=c("All","Native"))

HillNN<- HillNN%>%
  unite( "Scen_Order", c("Scenario","order"),remove=FALSE)

###################
# Order 0         #
# Species Richness#
###################

H_Sc2_Or0<-filter(HillNN, order==0)

H_f_Sc2_Or0<-filter(HillNN_f, order==0)

Hill_Or0<-ggplot(H_Sc2_Or0, aes(x=geo_entity2,y=qD,group=geo_entity2))+
  geom_point(data=H_Sc2_Or0, aes(x=geo_entity2,y=qD),position = position_jitter(w = 0.02, h = 0),color="gray80",size=0.25,alpha=0.2)+
  
  geom_point(data=H_f_Sc2_Or0, aes(x=geo_entity2,y=qD,group=geo_entity2, colour=Scenario),size=1)+
  geom_errorbar(data=H_f_Sc2_Or0,aes(ymin=qD.LCL,ymax=qD.UCL,group=geo_entity2, colour=Scenario),width=0.1)+
   scale_colour_manual(values=c("All"="#3C5488FF","Native"="#008975"))+
  
  scale_y_continuous(limits=c(0, 57),breaks=c(0,10,20,30,40,50),labels=c("0","10","20","30","40","50"))+
  labs(x="",y="Species richness (q = 0)")+
  guides(colour=guide_legend(title="",title.position = "top", hjust=0.5))+
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

#####################
# Order 2           #
# Simpson diversity #
#####################

H_Sc2_Or2<-filter(HillNN, order==2)

H_f_Sc2_Or2<-filter(HillNN_f, order==2)

Hill_Or2<-ggplot(H_Sc2_Or2, aes(x=geo_entity2,y=qD,group=geo_entity2))+
  geom_point(data=H_Sc2_Or2, aes(x=geo_entity2,y=qD),position = position_jitter(w = 0.02, h = 0),color="gray80",size=0.25,alpha=0.2)+
  
  geom_point(data=H_f_Sc2_Or2, aes(x=geo_entity2,y=qD,group=geo_entity2, colour=Scenario),size=1)+
  geom_errorbar(data=H_f_Sc2_Or2,aes(ymin=qD.LCL,ymax=qD.UCL,group=geo_entity2, colour=Scenario),width=0.1)+
  scale_colour_manual(values=c("All"="#3C5488FF","Native"="#008975"))+
  
  scale_y_continuous(limits=c(0, 12),breaks=c(0,5,10),labels=c("0","5","10"))+
  # scale_shape_manual(values=c(17,15,16))+
  #facet_grid(order~Scenario,scales="free_y")+
  labs(x="",y="Simpson diversity (q = 2)")+
  guides(colour=guide_legend(title="",title.position = "top", hjust=0.5))+
  theme_bw()+theme(plot.title = element_text(colour="black",face="bold",size=7,hjust=0.5,vjust=0),
                   axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_text(colour="black",face="bold",size=6),
                   axis.text.x=element_text(colour="black",face="bold",size=6),
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

#################
# put together ##
#################

require(cowplot)

hill_tog<-plot_grid(Hill_Or0+theme(legend.position="none"),
                    Hill_Or2+theme(legend.position="none"),
                    labels=c("a)","b)"),label_size = 6,ncol=1, align="vh")

legend <- get_legend(Hill_Or0)

hill_togg<- plot_grid( legend,hill_tog, rel_heights = c(0.2, 5),ncol=1)

png(filename="Figures/HillN_All_Natives_10plots_Fig4.png", 
    units="in", 
    width=5, 
    height=7, 
    pointsize=2, 
    res=500)

hill_togg

dev.off()
