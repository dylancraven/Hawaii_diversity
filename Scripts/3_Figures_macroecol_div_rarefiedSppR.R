
require(dplyr)
require(ggplot2)
require(grid)
require(ggsci)
require(reshape2)

####################


macro<-read.csv("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Data/Hawaii_Div_Macro_SIE.csv",sep=",",header=T)

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

macro_g<-ggplot(macroo2,aes(x=geo_entity2,y=Div2,colour=Div, fill=Div))+ 
  geom_bar(stat="identity",position=position_dodge())+
  scale_x_discrete(labels=c("Hawai'i Island"="Hawai'i","Maui Nui"="Maui Nui","O'ahu Island"="O'ahu","Kaua'i Island"="Kaua'i"))+
  #scale_color_d3(palette="category20c")+ scale_fill_d3(palette="category20c")+
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
                   legend.key.size = unit(1,"line"),
                   legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())

### 

g<-ggplot_build(macro_g)

unique(g$data[[1]]["fill"])

##############################################
# bar graph with rarefied species richness   #
##############################################

SppR<-read.csv("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/SppRarefy_alldata.csv",header=T)

SppR$Rarefy<-factor(SppR$Rarefy, levels=c("N100","N1000","N10000","all"))
SppR$Location<-factor(SppR$Location,levels=c("Hawai'i Island","Maui Nui","O'ahu Island","Kaua'i Island"))

SppR$Loc_Rare<-as.factor(paste(SppR$Location,SppR$Rarefy,sep="_"))


SppR$Loc_Rare<-factor(SppR$Loc_Rare, levels=c("Hawai'i Island_N100","Hawai'i Island_N1000","Hawai'i Island_N10000","Hawai'i Island_all",
                                              "Maui Nui_N100","Maui Nui_N1000","Maui Nui_N10000","Maui Nui_all" ,
                                              "O'ahu Island_N100","O'ahu Island_N1000","O'ahu Island_N10000","O'ahu Island_all",
                                              "Kaua'i Island_N100","Kaua'i Island_N1000","Kaua'i Island_N10000","Kaua'i Island_all"))

SppR$Rarefy<-as.character(SppR$Rarefy)
SppR$Rarefy2<-ifelse(SppR$Rarefy=="N100","100",NA)
SppR$Rarefy2<-ifelse(SppR$Rarefy=="N1000","1,000",SppR$Rarefy2)
SppR$Rarefy2<-ifelse(SppR$Rarefy=="N10000","10,000",SppR$Rarefy2)
SppR$Rarefy2<-ifelse(SppR$Rarefy=="all","all",SppR$Rarefy2)
SppR$Rarefy2<-as.factor(SppR$Rarefy2)
SppR$Rarefy2<-factor(SppR$Rarefy2, levels=c("100","1,000","10,000","all"))


All_bar<-ggplot(SppR,aes(x=Loc_Rare,y=S,colour=Rarefy2, fill=Rarefy2))+ 
  geom_bar(stat="identity",position=position_dodge())+
  geom_errorbar(data=SppR,aes(ymin=S-se,ymax =S+se), width=0.6) +
  scale_x_discrete(labels=c("Hawai'i Island_N100"="","Hawai'i Island_N1000"="Hawai'i","Hawai'i Island_N10000"="","Hawai'i Island_all"="",
                            "Maui Nui_N100"="","Maui Nui_N1000"="Maui Nui","Maui Nui_N10000"="","Maui Nui_all"="" ,
                            "O'ahu Island_N100"="","O'ahu Island_N1000"="O'ahu","O'ahu Island_N10000"="","O'ahu Island_all"="",
                            "Kaua'i Island_N100"="","Kaua'i Island_N1000"="Kaua'i","Kaua'i Island_N10000"="","Kaua'i Island_all"=""))+
  
  scale_fill_manual(values=c("#C5CAE9","#7986CB","#3949AB","#1A2373"))+
  scale_colour_manual(values=c("#C5CAE9","#7986CB","#3949AB","#1A2373"))+
  
  scale_y_continuous(breaks=c(0, 10, 20, 30, 40, 50, 60))+
  guides(colour=FALSE,fill=guide_legend(title="Sample size (individuals)",title.position = "top"))+
  labs(x="",y="Species diversity")+
  theme_bw()+theme(legend.position="top", 
                   axis.title.y=element_text(colour="black",face="bold",size=8),
                   axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.text.x=element_text(colour=c("black"),face="bold",size=8, hjust=c(0.2,0,0,0)),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=8),
                   legend.text=element_text(colour=c("black"),face="bold",size=7),
                   legend.title = element_text(colour="black",face="bold",size=7),
                   legend.title.align = 0.5,
                   legend.key.size = unit(1,"line"),
                   legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())



#################

require(cowplot)



png(filename="/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/HawaiiDiversity_SppDiv_Fig1.png", 
    units="in", 
    width=4, 
    height=8, 
    pointsize=2, 
    res=400)

plot_grid(macro_g, All_bar,labels=c("a)","b)"),label_size=7,ncol=1)


dev.off()


