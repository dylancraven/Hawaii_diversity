require(ggplot2)
require(dplyr)
require(reshape2)
require(ggsci)

############################
# Macro-ecology diversity  #
############################

#########
# data ##
#########

macroo<-read.csv("Data/Hawaii_Div_Macro_SIE.csv",sep=",",header=T)

macroo$geo_entity2<-as.factor(macroo$geo_entity2)
macroo$geo_entity2<-factor(macroo$geo_entity2,levels=c("Hawai'i Island","Maui Nui","O'ahu Island","Kaua'i Island"))

macroo$species_group<-as.character(macroo$species_group)
macroo$species_group<-ifelse(macroo$species_group=="nSIE","Single Island Endemic",macroo$species_group)
macroo$species_group<-ifelse(macroo$species_group=="all","Total Woody Spp.",macroo$species_group)
macroo$species_group<-ifelse(macroo$species_group=="native","Native Woody Spp.",macroo$species_group)
macroo$species_group<-ifelse(macroo$species_group=="exotic","Alien Woody Spp.",macroo$species_group)

macroo$species_group<-as.factor(macroo$species_group)
macroo$species_group<-factor(macroo$species_group,levels=c("Single Island Endemic","Native Woody Spp.", "Alien Woody Spp.","Total Woody Spp."))

macroo2<-filter(macroo, species_group!="Total Woody Spp.")

###########
# figure ##
###########

macro_g<-ggplot(macroo2,aes(x=geo_entity2,y=SppN,colour=species_group, fill=species_group))+ 
  geom_bar(stat="identity",position=position_dodge(width=0.8),width=0.7)+
  scale_x_discrete(labels=c("Hawai'i Island"="Hawai'i","Maui Nui"="Maui Nui","O'ahu Island"="O'ahu","Kaua'i Island"="Kaua'i"))+
  #scale_color_npg() + scale_fill_npg()+
  
  scale_fill_manual(values=c("#abd9e9","#2c7bb6","#d7191c","#3C5488FF"))+
  scale_colour_manual(values=c("#abd9e9","#2c7bb6","#d7191c","#3C5488FF"))+
  # 
  scale_y_continuous(breaks=c(0,100, 200, 300,400, 500,600))+
  guides(colour=FALSE,fill=guide_legend(title="",title.position = "top"))+
  labs(x="",y="Species richness")+
  theme_bw()+theme(legend.position="top", 
                   axis.title.y=element_text(colour="black",face="bold",size=8),
                   axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.text.x=element_text(colour=c("black"),face="bold",size=8),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=8),
                   legend.text=element_text(colour=c("black"),face="bold",size=7),
                   legend.title = element_text(colour=c("black"),face="bold",size=6),
                   legend.title.align = 0.5,
                   legend.key.size = unit(0.25,"cm"),
                   legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank())

png(filename="Figures/Fig2_MacroEcolDiversity.png", 
    units="in", 
    width=4.8, 
    height=4, 
    pointsize=2, 
    res=500)

macro_g

dev.off()
