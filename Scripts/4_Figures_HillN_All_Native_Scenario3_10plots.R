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
#require(ggridges)
#require(grid)
require(reshape2)

##########
# data  ##
##########

load("Cleaned_Data/Scen3_All_Native_HillN_anova_summary.RData")

HillN_All<-read.csv("Cleaned_Data/Scen3_Total_10plots_HillN.csv")
HillN_Natives<-read.csv("Cleaned_Data/Scen3_Natives_10plots_HillN.csv")

#############
# organize  #
#############

HillN_All$Scenario<-"All species"
HillN_Natives$Scenario<-"Native species"

HillNN<-rbind.data.frame(HillN_All, HillN_Natives)

HillNN$geo_entity2<-as.character(HillNN$geo_entity2)
HillNN$geo_entity2<-ifelse(HillNN$geo_entity2=="O'ahu Island (incl. Mokoli'i Islet)","O'ahu",HillNN$geo_entity2)
HillNN$geo_entity2<-ifelse(HillNN$geo_entity2=="O'ahu Island","O'ahu",HillNN$geo_entity2)
HillNN$geo_entity2<-ifelse(HillNN$geo_entity2=="Hawai'i Island","Hawai'i",HillNN$geo_entity2)
HillNN$geo_entity2<-ifelse(HillNN$geo_entity2=="Kaua'i Island","Kaua'i",HillNN$geo_entity2)

HillNN$geo_entity2<-as.factor(HillNN$geo_entity2)
HillNN$geo_entity2<-factor(HillNN$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

HillNN<- HillNN%>%
  unite( "Isl_Scen", c("geo_entity2","Scenario"),remove=FALSE)

HillNN$geo_entity2<-as.character(HillNN$geo_entity2)
HillNN$x_axis<-ifelse(HillNN$geo_entity2=="Hawai'i" & HillNN$Scenario=="All species", 0.90, NA)
HillNN$x_axis<-ifelse(HillNN$geo_entity2=="Hawai'i" & HillNN$Scenario=="Native species", 1.10, HillNN$x_axis)
HillNN$x_axis<-ifelse(HillNN$geo_entity2=="Maui Nui" & HillNN$Scenario=="All species", 1.90, HillNN$x_axis)
HillNN$x_axis<-ifelse(HillNN$geo_entity2=="Maui Nui" & HillNN$Scenario=="Native species", 2.10, HillNN$x_axis)
HillNN$x_axis<-ifelse(HillNN$geo_entity2=="O'ahu" & HillNN$Scenario=="All species", 2.90, HillNN$x_axis)
HillNN$x_axis<-ifelse(HillNN$geo_entity2=="O'ahu" & HillNN$Scenario=="Native species", 3.10, HillNN$x_axis)
HillNN$x_axis<-ifelse(HillNN$geo_entity2=="Kaua'i" & HillNN$Scenario=="All species", 3.90, HillNN$x_axis)
HillNN$x_axis<-ifelse(HillNN$geo_entity2=="Kaua'i" & HillNN$Scenario=="Native species", 4.10, HillNN$x_axis)

HillNN$Scenario<-factor(HillNN$Scenario,levels=c("All species","Native species"))

##############################
# organize model summaries   #
# and combine                #  
##############################

Scen3_AllSpp_q0_modelpred$Scenario<-"All species"
Scen3_AllSpp_q0_modelpred$Order<-0

Scen3_AllSpp_q0_modelpred<-select(Scen3_AllSpp_q0_modelpred, Scenario,Order, geo_entity2=x, qD=predicted,
                                  qD.LCL=conf.low, qD.UCL=conf.high)

Scen3_NativesSpp_q0_modelpred$Scenario<-"Native species"
Scen3_NativesSpp_q0_modelpred$Order<-0
Scen3_NativesSpp_q0_modelpred<-select(Scen3_NativesSpp_q0_modelpred,Scenario,Order,geo_entity2=x, qD=predicted,
                                      qD.LCL=conf.low, qD.UCL=conf.high)

Scen3_AllSpp_q2_modelpred$Scenario<-"All species"
Scen3_AllSpp_q2_modelpred$Order<-2

Scen3_AllSpp_q2_modelpred<-select(Scen3_AllSpp_q2_modelpred, Scenario,Order, geo_entity2=x, qD=predicted,
                                  qD.LCL=conf.low, qD.UCL=conf.high)

Scen3_NativesSpp_q2_modelpred$Scenario<-"Native species"
Scen3_NativesSpp_q2_modelpred$Order<-2
Scen3_NativesSpp_q2_modelpred<-select(Scen3_NativesSpp_q2_modelpred,Scenario,Order,geo_entity2=x, qD=predicted,
                                      qD.LCL=conf.low, qD.UCL=conf.high)

# join
HillNN_f<-rbind.data.frame(Scen3_AllSpp_q0_modelpred,Scen3_NativesSpp_q0_modelpred,
                           Scen3_AllSpp_q2_modelpred,Scen3_NativesSpp_q2_modelpred)

HillNN_f$geo_entity2<-as.character(HillNN_f$geo_entity2)
HillNN_f$x_axis<-ifelse(HillNN_f$geo_entity2=="Hawai'i" & HillNN_f$Scenario=="All species", 0.90, NA)
HillNN_f$x_axis<-ifelse(HillNN_f$geo_entity2=="Hawai'i" & HillNN_f$Scenario=="Native species", 1.10, HillNN_f$x_axis)
HillNN_f$x_axis<-ifelse(HillNN_f$geo_entity2=="Maui Nui" & HillNN_f$Scenario=="All species", 1.90, HillNN_f$x_axis)
HillNN_f$x_axis<-ifelse(HillNN_f$geo_entity2=="Maui Nui" & HillNN_f$Scenario=="Native species", 2.10, HillNN_f$x_axis)
HillNN_f$x_axis<-ifelse(HillNN_f$geo_entity2=="O'ahu" & HillNN_f$Scenario=="All species", 2.90, HillNN_f$x_axis)
HillNN_f$x_axis<-ifelse(HillNN_f$geo_entity2=="O'ahu" & HillNN_f$Scenario=="Native species", 3.10, HillNN_f$x_axis)
HillNN_f$x_axis<-ifelse(HillNN_f$geo_entity2=="Kaua'i" & HillNN_f$Scenario=="All species", 3.90, HillNN_f$x_axis)
HillNN_f$x_axis<-ifelse(HillNN_f$geo_entity2=="Kaua'i" & HillNN_f$Scenario=="Native species", 4.10, HillNN_f$x_axis)

HillNN_f$Scenario<-factor(HillNN_f$Scenario,levels=c("All species","Native species"))

HillNN_f<- HillNN_f%>%
  unite( "Isl_Scen", c("geo_entity2","Scenario"),remove=FALSE)

###################
# Order 0         #
# Species Richness#
###################

H_Sc3_Or0<-filter(HillNN, order==0)

H_f_Sc3_Or0<-filter(HillNN_f, Order==0)

Hill_Or0<-ggplot(H_Sc3_Or0) +
  
  # geom_vridgeline(aes(x = x_axis, y = qD,  width = ..density.., group=Isl_Scen,
  #                     fill=Scenario,colour=Scenario),
  #                stat = "ydensity",trim = FALSE, alpha = 0.2, scale = 1) +
  
  geom_point(data=H_Sc3_Or0,aes(x = x_axis, y = qD, group=Isl_Scen, colour=Scenario), 
             position=position_dodge(0.2),alpha=0.05, shape=20, size=0.5) +
  
  geom_pointrange(data=H_f_Sc3_Or0, aes(x=x_axis,y = qD,ymin=qD.LCL,ymax=qD.UCL,
                                        group=Isl_Scen, colour=Scenario),fatten=0.25,size=1)+
  
  scale_colour_manual(name="",values=c("All species"="#0571b0","Native species"="#008837"))+
  scale_fill_manual(name="",values=c("All species"="#0571b0","Native species"="#008837"))+
  
  scale_x_continuous( breaks=c(1,2,3,4),labels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))+
  
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
  
  geom_point(data=H_Sc3_Or2,aes(x = x_axis, y = qD, group=Isl_Scen, colour=Scenario), 
             position=position_dodge(0.2),alpha=0.05, shape=20,size=0.5) +
  
  geom_pointrange(data=H_f_Sc3_Or2, aes(x=x_axis,y = qD,ymin=qD.LCL,ymax=qD.UCL,
                                        group=Isl_Scen, colour=Scenario),fatten=0.25,size=1)+
  
  scale_colour_manual(name="",values=c("All species"="#0571b0","Native species"="#008837"))+
  scale_fill_manual(name="",values=c("All species"="#0571b0","Native species"="#008837"))+
  
  scale_x_continuous( breaks=c(1,2,3,4),labels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))+
  
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
                    labels=c("A","B"),label_size = 6,label_fontfamily ="sans",
                    ncol=1,align="vh")

legend <- get_legend(Hill_Or0)

hill_togg<- plot_grid( legend,hill_tog, rel_heights = c(0.15, 5),ncol=1)

#
ggsave(filename = file.path("Figures", "HillN_All_Natives_10plots_Fig4.png"), 
       width    = 8.7, 
       height   = 11, 
       units    = "cm",dpi=900)

hill_togg

dev.off()

# as pdf

ggsave(filename = file.path("Figures", "HillN_All_Natives_10plots_Fig4.pdf"), 
       width    = 8.7, 
       height   = 11, 
       units    = "cm",dpi=900)

hill_togg

dev.off()