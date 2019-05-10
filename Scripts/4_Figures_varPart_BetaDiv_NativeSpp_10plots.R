###################################
# Figure: variance partitioning   #
# of beta diversity               #
# Native spp. & both scenarios    #
###################################
# code adapted from  https://scriptsandstatistics.wordpress.com/2018/04/26/how-to-plot-venn-diagrams-using-r-ggplot2-and-ggforce/

require(tidyverse)
require(ggplot2)
require(viridis)

# Data

BetaS_vp_Scen2<-read.csv("Cleaned_Data/Scen2_natives_VarPart_summ.csv")
BetaS_vp_Scen2$Scenario<-"Het + Age"
BetaS_vp_Scen3<-read.csv("Cleaned_Data/Scen3_natives_VarPart_summ.csv")
BetaS_vp_Scen3$Scenario<-"Age"

# combine

BetaS_vpp<-rbind.data.frame(BetaS_vp_Scen2, BetaS_vp_Scen3)

# BetaS_vpp<-pivot_longer(BetaS_vp, cols= c("adjr2_env","adjr2_space","adjr2_env_space"), 
#              names_to = "VarPart", values_to = "AdjR2")

BetaS_vpp$geo_entity2<-as.character(BetaS_vpp$geo_entity2)
BetaS_vpp$geo_entity2<-ifelse(BetaS_vpp$geo_entity2=="O'ahu Island (incl. Mokoli'i Islet)","O'ahu",BetaS_vpp$geo_entity2)
BetaS_vpp$geo_entity2<-ifelse(BetaS_vpp$geo_entity2=="O'ahu Island","O'ahu",BetaS_vpp$geo_entity2)
BetaS_vpp$geo_entity2<-ifelse(BetaS_vpp$geo_entity2=="Hawai'i Island","Hawai'i",BetaS_vpp$geo_entity2)
BetaS_vpp$geo_entity2<-ifelse(BetaS_vpp$geo_entity2=="Kaua'i Island","Kaua'i",BetaS_vpp$geo_entity2)

BetaS_vpp$geo_entity2<-as.factor(BetaS_vpp$geo_entity2)
BetaS_vpp$geo_entity2<-factor(BetaS_vpp$geo_entity2,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

BetaS_vpp$Partition<-as.character(BetaS_vpp$Partition)
BetaS_vpp$Partition<-ifelse(BetaS_vpp$Partition=="adjr2_env","Env.",BetaS_vpp$Partition)
BetaS_vpp$Partition<-ifelse(BetaS_vpp$Partition=="adjr2_space","Space",BetaS_vpp$Partition)
BetaS_vpp$Partition<-ifelse(BetaS_vpp$Partition=="adjr2_env_space","Env. + Space",BetaS_vpp$Partition)

BetaS_vpp$Partition<-as.factor(BetaS_vpp$Partition)
BetaS_vpp$Partition<-factor(BetaS_vpp$Partition,levels=c("Env.","Space","Env. + Space"))

BetaS_vpp<-BetaS_vpp %>% unite("Partition_Scen",c("Partition","Scenario"),remove=FALSE)

# Figure

my.labels <- c("Env.","Space","Env. +\nSpace") # first create labels, add \n where appropriate.

vp_pnts<-ggplot(data=BetaS_vpp) +
  geom_pointrange(aes(x=Partition,y = Mean,ymin=Lower,ymax=Upper,
                                        group=Partition, colour=Partition),fatten=0.25,size=1)+
  facet_grid(Scenario~geo_entity2) +
  scale_color_viridis_d(option="E", name= "") + scale_fill_viridis_d(option="E",name= "")+
  scale_x_discrete(labels=my.labels)+
  labs(x="", y=expression(bold(paste("Explained variation (adj. R" ^2,")"))))+
  
  theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=8,family="sans"),
                   axis.title.y=element_text(colour="black",face="bold",size=7,family="sans"),
                   axis.text.x=element_text(colour="black",face="bold",size=6,family="sans"),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6,family="sans"),
                   strip.text = element_text(colour="black", face="bold",size=6, family="sans"),
                   strip.background = element_rect(fill="transparent",colour="black"),
                   legend.text=element_text(colour=c("black"),face="bold",size=7,family="sans"),
                   legend.title = element_text(colour=c("black"),face="bold",size=7,family="sans"),
                   legend.title.align = 0.5,
                   legend.margin=margin(t=0.00, r=0, b=0, l=0, unit="cm"),
                   legend.position=c("none"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank())
  
ggsave(filename = file.path("Figures", "FigS5_varpart_natives_Scen23.png"), 
       width    = 11.4, 
       height   = 11.4, 
       units    = "cm", dpi=900)

vp_pnts

dev.off()