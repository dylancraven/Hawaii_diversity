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

macro<-read.csv("Data/Hawaii_Div_Macro_SIE.csv",sep=",",header=T)

macroo<-melt(macro, id.vars="geo_entity2", measure.vars=c("nativeSIE","native_woody_spp","total_SppN"),variable.name="Div",value.name="Div2")

macroo$geo_entity2<-as.factor(macroo$geo_entity2)
macroo$geo_entity2<-factor(macroo$geo_entity2,levels=c("Hawai'i Island","Maui Nui","O'ahu Island","Kaua'i Island"))

macroo$Div<-as.character(macroo$Div)
macroo$Div<-ifelse(macroo$Div=="nativeSIE","Single Island Endemic",macroo$Div)
macroo$Div<-ifelse(macroo$Div=="total_SppN","Total Woody Spp.",macroo$Div)
macroo$Div<-ifelse(macroo$Div=="native_woody_spp","Native Woody Spp.",macroo$Div)

macroo$Div<-as.factor(macroo$Div)
macroo$Div<-factor(macroo$Div,levels=c("Single Island Endemic","Native Woody Spp.","Total Woody Spp."))

macroo2<-filter(macroo, Div!="Total Woody Spp.")

###########
# figure ##
###########

macro_g<-ggplot(macroo2,aes(x=geo_entity2,y=Div2,colour=Div, fill=Div))+ 
  geom_bar(stat="identity",position=position_dodge(),width=0.5)+
  scale_x_discrete(labels=c("Hawai'i Island"="Hawai'i","Maui Nui"="Maui Nui","O'ahu Island"="O'ahu","Kaua'i Island"="Kaua'i"))+
  scale_fill_manual(values=c("#00BF9A","#008975"))+
  scale_colour_manual(values=c("#00BF9A","#008975"))+
  
  scale_y_continuous(breaks=c(0,50, 100, 150, 200, 250, 300))+
  guides(colour=FALSE,fill=guide_legend(title="",title.position = "top"))+
  labs(x="",y="Species diversity")+
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
    width=3.5, 
    height=3.5, 
    pointsize=2, 
    res=500)

macro_g

dev.off()
