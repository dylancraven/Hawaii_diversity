#############
# Hill N ####
##################
# Scenario 3:    #
##################
# All natives    #
# Elevation range# 
# 10 plots   #####
##################

require(tidyr)
require(dplyr)
require(ggplot2)
#require(grid)
require(reshape2)
require(viridis)

##########
# data  ##
##########

load("Cleaned_Data/Scen3_NativesElevR_HillN_anova_summary.RData")
HillNN<-read.csv("Cleaned_Data/Scen3_Natives_ElevR_10plots_HillN.csv")

#############
# organize  #
#############

HillNN$geo_entity2<-as.character(HillNN$geo_entity2)
HillNN$geo_entity2<-ifelse(HillNN$geo_entity2=="O'ahu Island (incl. Mokoli'i Islet)","O'ahu",HillNN$geo_entity2)
HillNN$geo_entity2<-ifelse(HillNN$geo_entity2=="O'ahu Island","O'ahu",HillNN$geo_entity2)
HillNN$geo_entity2<-ifelse(HillNN$geo_entity2=="Hawai'i Island","Hawai'i",HillNN$geo_entity2)
HillNN$geo_entity2<-ifelse(HillNN$geo_entity2=="Kaua'i Island","Kaua'i",HillNN$geo_entity2)

HillNN$geo_entity2<-as.factor(HillNN$geo_entity2)
HillNN$geo_entity2<-factor(HillNN$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

##############################
# organize model summaries   #
# and combine                #  
##############################

Scen3_NativesElevR_q0_modelpred$Order<-0

Scen3_NativesElevR_q0_modelpred<-select(Scen3_NativesElevR_q0_modelpred, Order, geo_entity2=x, qD=predicted,
                                     qD.LCL=conf.low, qD.UCL=conf.high)

Scen3_NativesElevR_q2_modelpred$Order<-2
Scen3_NativesElevR_q2_modelpred<-select(Scen3_NativesElevR_q2_modelpred,Order,geo_entity2=x, qD=predicted,
                                     qD.LCL=conf.low, qD.UCL=conf.high)

HillNN_f<-rbind.data.frame(Scen3_NativesElevR_q0_modelpred,Scen3_NativesElevR_q2_modelpred)

HillNN_f$geo_entity2<-as.factor(HillNN_f$geo_entity2)
HillNN_f$geo_entity2<-factor(HillNN_f$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

###################
#  order 0        #
###################

H_Sc3_Or0<-filter(HillNN, order==0)

H_f_Sc3_Or0<-filter(HillNN_f, Order==0)

Hill_Or0<-ggplot(H_Sc3_Or0) +
  
  # geom_vridgeline(aes(x = x_axis, y = qD,  width = ..density.., group=Isl_Scen,
  #                     fill=Scenario,colour=Scenario),
  #                stat = "ydensity",trim = FALSE, alpha = 0.2, scale = 1) +
  
  geom_point(data=H_Sc3_Or0,aes(x = geo_entity2, y = qD, group=geo_entity2, colour=geo_entity2), 
             position=position_dodge(0.2),alpha=0.05, shape=20, size=0.5) +
  
  geom_pointrange(data=H_f_Sc3_Or0, aes(x=geo_entity2,y = qD,ymin=qD.LCL,ymax=qD.UCL,
                                        group=geo_entity2, colour=geo_entity2),fatten=0.25,size=1)+
  
  scale_color_viridis_d(name="", option="D")+
  scale_fill_viridis_d(name="",option="D")+
  
  #scale_x_continuous( breaks=c(1,2,3,4),labels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))+
  
  labs(x="",y="Species richness (q = 0)")+
  
  guides(colour = guide_legend(override.aes = list(size = 0.5,fill="transparent",linetype=0)))+
  
  theme_bw()+theme(plot.title = element_text(colour="black",face="bold",size=7,hjust=0.5,vjust=0),
                   axis.title.x=element_text(colour="black",face="bold",size=8,family="sans"),
                   axis.title.y=element_text(colour="black",face="bold",size=7,family="sans"),
                   axis.text.x=element_blank(),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6,family="sans"),
                   #legend.key = element_rect(fill=NA),
                   legend.text=element_text(colour=c("black"),face="bold",size=7,family="sans"),
                   legend.title = element_text(colour=c("black"),face="bold",size=7,family="sans"),
                   legend.title.align = 0.5,
                   legend.margin=margin(t=0.00, r=0, b=0, l=0, unit="cm"),
                   legend.position=c("top"),
                   #legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank())

#####################
# Order 2           #
# Simpson diversity #
#####################

H_Sc3_Or2<-filter(HillNN, order==2)

H_f_Sc3_Or2<-filter(HillNN_f, Order==2)

Hill_Or2<-ggplot(H_Sc3_Or2)+
  
  # geom_vridgeline(aes(x = x_axis, y = qD,  width = ..density.., group=Isl_Scen,
  #                     fill=Scenario,colour=Scenario),
  #                 stat = "ydensity",trim = FALSE, alpha = 0.2, scale = 1) +
  
  geom_point(data=H_Sc3_Or2,aes(x = geo_entity2, y = qD, group=geo_entity2, colour=geo_entity2), 
             position=position_dodge(0.2),alpha=0.05, shape=20,size=0.5) +
  
  geom_pointrange(data=H_f_Sc3_Or2, aes(x=geo_entity2,y = qD,ymin=qD.LCL,ymax=qD.UCL,
                                        group=geo_entity2, colour=geo_entity2),fatten=0.25,size=1)+
  
  scale_color_viridis_d(name="", option="D")+
  scale_fill_viridis_d(name="",option="D")+
  
  #scale_x_continuous( breaks=c(1,2,3,4),labels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))+
  
  labs(x="",y="Simpson diversity (q = 2)")+
  guides(colour = guide_legend(override.aes = list(size = 1,fill="transparent",linetype=0)))+
  
  theme_bw()+theme(plot.title = element_text(colour="black",face="bold",size=7,hjust=0.5,vjust=0),
                   axis.title.x=element_text(colour="black",face="bold",size=8,family="sans"),
                   axis.title.y=element_text(colour="black",face="bold",size=7,family="sans"),
                   axis.text.x=element_text(colour="black",face="bold",size=8,family="sans"),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6,family="sans"),
                   legend.text=element_text(colour=c("black"),face="bold",size=7,family="sans"),
                   legend.title = element_text(colour=c("black"),face="bold",size=7,family="sans"),
                   legend.title.align = 0.5,
                   legend.margin=margin(t=0.00, r=0, b=0, l=0, unit="cm"),
                   legend.position=c("top"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank())

#################
# put together ##
#################

require(cowplot)

hill_tog<-plot_grid(Hill_Or0+theme(legend.position="none"),
                    Hill_Or2+theme(legend.position="none"),
                    labels=c("A","B"),label_size = 6,ncol=1,label_fontfamily ="sans",
                    align="vh")

ggsave(filename = file.path("Figures", "HillN_NativeElevR_10plots_FigSX.png"), 
       width    = 8.7, 
       height   = 11, 
       units    = "cm",dpi=900)

hill_tog

dev.off()