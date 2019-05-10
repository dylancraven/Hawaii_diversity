################
## beta_S_PIE  #
################
# 10 plots #####
###############
# only Scenario II: "Het + Age"
# just Beta S

require(tidyr)
require(dplyr)
require(ggplot2)
#require(ggridges)
#require(grid)
require(reshape2)

#############
# Data  #####
#############

load("Cleaned_Data/Scen2_All_Native_BetaS_anova_summary.RData")

Beta_All<-read.csv("Cleaned_Data/Scen2_Total_10plots_BetaPIE.csv",sep=",",header=T)

Beta_All$geo_entity2<-as.character(Beta_All$geo_entity2)
Beta_All$geo_entity2<-ifelse(Beta_All$geo_entity2=="O'ahu Island (incl. Mokoli'i Islet)","O'ahu",Beta_All$geo_entity2)
Beta_All$geo_entity2<-ifelse(Beta_All$geo_entity2=="O'ahu Island","O'ahu",Beta_All$geo_entity2)
Beta_All$geo_entity2<-ifelse(Beta_All$geo_entity2=="Hawai'i Island","Hawai'i",Beta_All$geo_entity2)
Beta_All$geo_entity2<-ifelse(Beta_All$geo_entity2=="Kaua'i Island","Kaua'i",Beta_All$geo_entity2)

Beta_All$geo_entity2<-as.factor(Beta_All$geo_entity2)
Beta_All$geo_entity2<-factor(Beta_All$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

# take average of beta diversity per iteration
Beta_All<- Beta_All %>% group_by(Iteration,geo_entity2, index) %>%
  summarize(beta=mean(value)) 
Beta_All<-filter(Beta_All, index=="beta_S")
Beta_All$Scenario<-"All species"

Beta_N<-read.csv("Cleaned_Data/Scen2_Natives_10plots_BetaPIE.csv",sep=",",header=T)

Beta_N$geo_entity2<-as.character(Beta_N$geo_entity2)
Beta_N$geo_entity2<-ifelse(Beta_N$geo_entity2=="O'ahu Island (incl. Mokoli'i Islet)","O'ahu",Beta_N$geo_entity2)
Beta_N$geo_entity2<-ifelse(Beta_N$geo_entity2=="O'ahu Island","O'ahu",Beta_N$geo_entity2)
Beta_N$geo_entity2<-ifelse(Beta_N$geo_entity2=="Hawai'i Island","Hawai'i",Beta_N$geo_entity2)
Beta_N$geo_entity2<-ifelse(Beta_N$geo_entity2=="Kaua'i Island","Kaua'i",Beta_N$geo_entity2)

Beta_N$geo_entity2<-as.factor(Beta_N$geo_entity2)
Beta_N$geo_entity2<-factor(Beta_N$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

# take average of beta diversity per iteration
Beta_N<- Beta_N %>% group_by(Iteration,geo_entity2, index) %>%
  summarize(beta=mean(value))
Beta_N<-filter(Beta_N, index=="beta_S")
Beta_N$Scenario<-"Native species"

Beta_SS<-rbind.data.frame(Beta_All, Beta_N)

Beta_SS<- Beta_SS%>%
  unite( "Isl_Scen", c("geo_entity2","Scenario"),remove=FALSE)

Beta_SS$Scenario<-factor(Beta_SS$Scenario,levels=c("All species","Native species"))

Beta_SS$geo_entity2<-as.character(Beta_SS$geo_entity2)
Beta_SS$x_axis<-ifelse(Beta_SS$geo_entity2=="Hawai'i" & Beta_SS$Scenario=="All species", 0.90, NA)
Beta_SS$x_axis<-ifelse(Beta_SS$geo_entity2=="Hawai'i" & Beta_SS$Scenario=="Native species", 1.10, Beta_SS$x_axis)
Beta_SS$x_axis<-ifelse(Beta_SS$geo_entity2=="Maui Nui" & Beta_SS$Scenario=="All species", 1.90, Beta_SS$x_axis)
Beta_SS$x_axis<-ifelse(Beta_SS$geo_entity2=="Maui Nui" & Beta_SS$Scenario=="Native species", 2.10, Beta_SS$x_axis)
Beta_SS$x_axis<-ifelse(Beta_SS$geo_entity2=="O'ahu" & Beta_SS$Scenario=="All species", 2.90, Beta_SS$x_axis)
Beta_SS$x_axis<-ifelse(Beta_SS$geo_entity2=="O'ahu" & Beta_SS$Scenario=="Native species", 3.10, Beta_SS$x_axis)
Beta_SS$x_axis<-ifelse(Beta_SS$geo_entity2=="Kaua'i" & Beta_SS$Scenario=="All species", 3.90, Beta_SS$x_axis)
Beta_SS$x_axis<-ifelse(Beta_SS$geo_entity2=="Kaua'i" & Beta_SS$Scenario=="Native species", 4.10, Beta_SS$x_axis)

# join models

Scen2_AllSpp_BetaS_modelpred$Scenario<-"All species"

Scen2_AllSpp_BetaS_modelpred<-select(Scen2_AllSpp_BetaS_modelpred, Scenario,geo_entity2=x, beta=predicted,
                                     beta.LCL=conf.low, beta.UCL=conf.high)

Scen2_NativesSpp_BetaS_modelpred$Scenario<-"Native species"
Scen2_NativesSpp_BetaS_modelpred<-select(Scen2_NativesSpp_BetaS_modelpred,Scenario,geo_entity2=x, beta=predicted,
                                         beta.LCL=conf.low, beta.UCL=conf.high)

BetaS_f<-rbind.data.frame(Scen2_AllSpp_BetaS_modelpred,Scen2_NativesSpp_BetaS_modelpred)

BetaS_f$geo_entity2<-as.character(BetaS_f$geo_entity2)
BetaS_f$x_axis<-ifelse(BetaS_f$geo_entity2=="Hawai'i" & BetaS_f$Scenario=="All species", 0.90, NA)
BetaS_f$x_axis<-ifelse(BetaS_f$geo_entity2=="Hawai'i" & BetaS_f$Scenario=="Native species", 1.10, BetaS_f$x_axis)
BetaS_f$x_axis<-ifelse(BetaS_f$geo_entity2=="Maui Nui" & BetaS_f$Scenario=="All species", 1.90, BetaS_f$x_axis)
BetaS_f$x_axis<-ifelse(BetaS_f$geo_entity2=="Maui Nui" & BetaS_f$Scenario=="Native species", 2.10, BetaS_f$x_axis)
BetaS_f$x_axis<-ifelse(BetaS_f$geo_entity2=="O'ahu" & BetaS_f$Scenario=="All species", 2.90, BetaS_f$x_axis)
BetaS_f$x_axis<-ifelse(BetaS_f$geo_entity2=="O'ahu" & BetaS_f$Scenario=="Native species", 3.10, BetaS_f$x_axis)
BetaS_f$x_axis<-ifelse(BetaS_f$geo_entity2=="Kaua'i" & BetaS_f$Scenario=="All species", 3.90, BetaS_f$x_axis)
BetaS_f$x_axis<-ifelse(BetaS_f$geo_entity2=="Kaua'i" & BetaS_f$Scenario=="Native species", 4.10, BetaS_f$x_axis)

BetaS_f$Scenario<-factor(BetaS_f$Scenario,levels=c("All species","Native species"))

BetaS_f<- BetaS_f%>%
  unite( "Isl_Scen", c("geo_entity2","Scenario"),remove=FALSE)

################
## figure      #
################

p_BetaS<-ggplot(Beta_SS) +
  # geom_vridgeline(aes(x = x_axis, y = qD,  width = ..density.., group=Isl_Scen,
  #                     fill=Scenario,colour=Scenario),
  #                stat = "ydensity",trim = FALSE, alpha = 0.2, scale = 1) +
  
  geom_point(data=Beta_SS,aes(x = x_axis, y = beta, group=Isl_Scen, colour=Scenario), 
             position=position_dodge(0.2),alpha=0.05, shape=20, size=0.5) +
  
  geom_pointrange(data=BetaS_f, aes(x=x_axis,y = beta,ymin=beta.LCL,ymax=beta.UCL,
                                        group=Isl_Scen, colour=Scenario),fatten=0.25,size=1)+
  
  scale_colour_manual(name="",values=c("All species"="#0571b0","Native species"="#008837"))+
  scale_fill_manual(name="",values=c("All species"="#0571b0","Native species"="#008837"))+
  
  scale_x_continuous( breaks=c(1,2,3,4),labels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))+
  
  labs(x="",y=expression(bold(beta["S"])))+
  
  guides(colour = guide_legend(override.aes = list(size = 0.5,fill="transparent",linetype=0)))+
  
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

# write out 

ggsave(filename = file.path("Figures", "FigS5_Beta_S_all_natives_Scenario2_10plots.png"), 
       width    = 8.7, 
       height   = 6.7, 
       units    = "cm", dpi=900)

p_BetaS

dev.off()