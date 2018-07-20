require(dplyr)
require(tidyr)
require(ggplot2)


# Scenario 2  
#summarize

rad.tog2<-read.csv("Cleaned_Data/Scen2_Natives_7plots_RADs.csv",sep=",",header=T)

Scen2_rad.tog<-rad.tog2
Scen2_rad.tog$iteration<-as.factor(Scen2_rad.tog$iteration)

Scen2_rad.tog$geo_entity2<-as.factor(Scen2_rad.tog$geo_entity2)
Scen2_rad.tog$geo_entity2<-factor(Scen2_rad.tog$geo_entity2,levels=c("Hawai'i Island","Maui Nui","O'ahu Island","Kaua'i Island"))


test<-ggplot(Scen2_rad.tog, aes(y=Rank_N, x=log(RelAbund),group=Island_Iter,colour=geo_entity2))+
  stat_ecdf(geom="line",alpha=0.1)+
  #geom_smooth(data=RAD_2, aes(y=RankN_pred,x=log(RelAbund), group=geo_entity2, color=geo_entity2),method="gam", formula=y~s(x),size=1,se=FALSE)+
  stat_ecdf(data=Scen2_rad.tog, aes(y=Rank_N,x=log(RelAbund), group=geo_entity2, color=geo_entity2),
            geom="line", linetype=1,size=1)+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  ylab("% Species")+xlab("% Abundance (log scale)")+
  #scale_color_npg()+
  guides(colour=guide_legend(title=""))+
  theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_text(colour="black",face="bold",size=6),
                   axis.text.x=element_text(colour=c("black"),face="bold",size=6),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   legend.text=element_text(colour="black",face="bold",size=6),
                   legend.title.align = 0,
                   legend.position=c("none"),
                   legend.title=element_text(colour="black",face="bold",size=6),
                   legend.key.size = unit(2.5, 'lines'),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())



png(filename="Figures/RADtrial_Fig3.png", 
    units="in", 
    width=4, 
    height=4, 
    pointsize=2, 
    res=500)
test
dev.off()



###

test<-ggplot(rad.tog2, aes(y=Rank_N, x=log(RelAbund),group=geo_entity2,colour=geo_entity2))+
  stat_ecdf(geom="line")+
  scale_colour_manual(values=c("#D7191C1A","#FDAE611A","#ABD9E91A","#2C7BB61A"))+
  
  geom_smooth(data=rad_MN, aes(y=pred,x=log(RelAbund)),size=1,se=FALSE,color="black")+
  
  
  ylab("% Species")+xlab("% Abundance (log scale)")+
  guides(colour=guide_legend(title=""))
  

ggplot(Scen2_radd, aes(y=Rank_N, x=log(RelAbund),group=geo_entity2,colour=geo_entity2))+
  stat_ecdf(geom="line")+
  scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  
  
ggplot(df, aes(height)) + stat_ecdf(geom = "step")+
  labs(title="Empirical Cumulative \n Density Function",
       y = "F(height)", x="Height in inch")+
  theme_classic()

