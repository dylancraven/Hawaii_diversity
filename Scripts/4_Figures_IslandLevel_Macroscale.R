############################
# Macro-ecology diversity  #
############################

require(ggplot2)
require(dplyr)
require(reshape2)

#########
# data ##
#########

macroo<-read.csv("Data/Hawaii_Div_Macro_SIE.csv",sep=",",header=T)

macroo$geo_entity2<-as.factor(macroo$geo_entity2)
macroo$geo_entity2<-factor(macroo$geo_entity2,levels=c("Hawai'i Island","Maui Nui",
                                                       "O'ahu Island","Kaua'i Island"))

macroo$species_group<-as.character(macroo$species_group)
macroo$species_group<-ifelse(macroo$species_group=="nSIE","Single Island Endemic \nWoody Spp."
                             ,macroo$species_group)
macroo$species_group<-ifelse(macroo$species_group=="all","Total Woody Spp.",
                             macroo$species_group)
macroo$species_group<-ifelse(macroo$species_group=="native","Native Woody \nSpp.",
                             macroo$species_group)
macroo$species_group<-ifelse(macroo$species_group=="exotic","Alien Woody \nSpp.",
                             macroo$species_group)

macroo$species_group<-as.factor(macroo$species_group)
macroo$species_group<-factor(macroo$species_group,levels=c("Single Island Endemic \nWoody Spp.",
                              "Native Woody \nSpp.", "Alien Woody \nSpp.","Total Woody Spp."))

macroo2<-filter(macroo, species_group!="Total Woody Spp.")
macroo2$species_group<-droplevels(macroo2$species_group)

###########
# figure ##
###########

# colorblind-friendly colors selected from: http://jfly.iam.u-tokyo.ac.jp/color/

macro_g<-ggplot(macroo2,aes(x=geo_entity2,y=SppN,colour=species_group, fill=species_group))+ 
  geom_bar(stat="identity",position=position_dodge(width=0.8),width=0.7)+
  scale_x_discrete(labels=c("Hawai'i Island"="Hawai'i","Maui Nui"="Maui Nui",
                            "O'ahu Island"="O'ahu","Kaua'i Island"="Kaua'i"))+
   scale_fill_manual(values=c("#56B4E9","#0072B2","#D55E00"))+
  scale_colour_manual(values=c("#56B4E9","#0072B2","#D55E00"))+
  # 
  scale_y_continuous(breaks=c(0,100, 200, 300,400, 500,600))+
  guides(colour=FALSE,fill=guide_legend(title="",title.position = "top",label.hjust=0.5))+
  labs(x="",y="Species richness")+
  theme_bw()+theme(legend.position="top", 
                   axis.title.y=element_text(colour="black",face="bold",size=12,family="sans"),
                   axis.title.x=element_text(colour="black",face="bold",size=12,family="sans"),
                   axis.text.x=element_text(colour=c("black"),face="bold",size=10,family="sans"),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=8,family="sans"),
                   legend.text=element_text(colour=c("black"),face="bold",size=6,family="sans"),
                   legend.title = element_text(colour=c("black"),face="bold",size=6,family="sans"),
                   legend.title.align = 0,
                   legend.key.size = unit(0.25,"cm"),
                   legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank())

# print out
ggsave(filename = file.path("Figures", "Fig2_MacroEcolDiversity.pdf"), 
       width    = 8.7, 
       height   = 6.7, 
       units    = "cm", dpi=900)

macro_g

dev.off()

ggsave(filename = file.path("Figures", "Fig2_MacroEcolDiversity.png"), 
       width    = 8.7, 
       height   = 6.7, 
       units    = "cm",dpi=900)

macro_g

dev.off()
