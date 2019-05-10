##########################
# Figures: SACs & RADs   #
# Scenarios 2 & 3 ########
# just natives    ########
##########################

require(ggplot2)
require(viridis)
load("Cleaned_Data/modelpredictions_SAC_RAD_Scen23_Native_10plots.RData")

##############
# Scenario 2 #
##############

########
# SACs #  
########

pred$group<-as.character(pred$group)
pred$group<-ifelse(pred$group=="Hawai'i Island","Hawai'i",pred$group)
pred$group<-ifelse(pred$group=="Kaua'i Island","Kaua'i",pred$group)
pred$group<-ifelse(pred$group=="O'ahu Island","O'ahu",pred$group)
pred$group<-as.factor(pred$group)

pred$group<-as.factor(pred$group)
pred$group<-factor(pred$group,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

p_scen2<-ggplot(pred, aes(x, predicted,group=group,colour=group,fill=group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,fill=group), colour="transparent", alpha = .3)+
  scale_y_continuous(breaks=c(0,5, 10, 15, 20,25,30))+
  scale_x_continuous(breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000))+
  scale_color_viridis_d(option="D")+
  scale_fill_viridis_d(option="D")+
  #scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  #scale_fill_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  coord_cartesian(ylim=c(0,34.5))+
  
  ylab("Species diversity")+xlab("Number of individuals")+
  guides(colour=guide_legend(title="Het+Age",title.position = "top"),fill="none")+
  
  theme_bw()+theme(legend.position="top", 
                   axis.title.x=element_text(colour="black",face="bold",size=7,family="sans"),
                   axis.title.y=element_text(colour="black",face="bold",size=7,family="sans"),
                   axis.text.x=element_text(colour="black",face="bold",size=5,family="sans"),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6,family="sans"),
                   legend.text=element_text(colour=c("black"),face="bold",size=4,family="sans"),
                   legend.title = element_text(colour=c("black"),face="bold",size=6,family="sans"),
                   legend.title.align = 0.5,
                   legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   legend.key.size = unit(1, 'lines'),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank())

########
# RADs #  
########

rad_pred2$group<-as.character(rad_pred2$group)
rad_pred2$group<-ifelse(rad_pred2$group=="Hawai'i Island","Hawai'i",rad_pred2$group)
rad_pred2$group<-ifelse(rad_pred2$group=="Kaua'i Island","Kaua'i",rad_pred2$group)
rad_pred2$group<-ifelse(rad_pred2$group=="O'ahu Island","O'ahu",rad_pred2$group)
rad_pred2$group<-as.factor(rad_pred2$group)

rad_pred2$group<-as.factor(rad_pred2$group)
rad_pred2$group<-factor(rad_pred2$group,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

rad_scen2<-ggplot(rad_pred2, aes(x, predicted,group=group,colour=group,fill=group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,fill=group), colour="transparent", alpha = .3)+
  # scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  # scale_fill_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_color_viridis_d(option="D")+
  scale_fill_viridis_d(option="D")+
  
   coord_cartesian(ylim=c(0,1))+
  ylab("% Species")+xlab("% Abundance (log scale)")+
  guides(colour=guide_legend(title=""),fill="none")+
  theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=7,family="sans"),
                   axis.title.y=element_text(colour="black",face="bold",size=7,family="sans"),
                   axis.text.x=element_text(colour="black",face="bold",size=6,family="sans"),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6,family="sans"),
                   legend.text=element_text(colour=c("black"),face="bold",size=4,family="sans"),
                   legend.title = element_text(colour=c("black"),face="bold",size=6,family="sans"),
                   legend.title.align = 0,
                   legend.position=c("none"),
                   legend.key.size = unit(1, 'lines'),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank())

##############
# Scenario 3 #
##############

########
# SACs #  
########

pred2$group<-as.character(pred2$group)
pred2$group<-ifelse(pred2$group=="Hawai'i Island","Hawai'i",pred2$group)
pred2$group<-ifelse(pred2$group=="Kaua'i Island","Kaua'i",pred2$group)
pred2$group<-ifelse(pred2$group=="O'ahu Island","O'ahu",pred2$group)
pred2$group<-as.factor(pred2$group)

pred2$group<-as.factor(pred2$group)
pred2$group<-factor(pred2$group,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

p_scen3<-ggplot(pred2, aes(x, predicted,group=group,colour=group,fill=group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,fill=group), colour="transparent", alpha = .3)+
  scale_y_continuous(breaks=c(0,5, 10, 15, 20,25,30))+
  scale_x_continuous(breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000))+
  #scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  #scale_fill_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_color_viridis_d(option="D")+
  scale_fill_viridis_d(option="D")+
  coord_cartesian(ylim=c(0,34.5))+
  
  ylab("Species diversity")+xlab("Number of individuals")+
  guides(colour=guide_legend(title="Age",title.position = "top"),fill="none")+
  
  theme_bw()+theme(legend.position="top", 
                   axis.title.x=element_text(colour="black",face="bold",size=7,family="sans"),
                   axis.title.y=element_text(colour="black",face="bold",size=7,family="sans"),
                   axis.text.x=element_text(colour="black",face="bold",size=5,family="sans"),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6,family="sans"),
                   legend.text=element_text(colour=c("black"),face="bold",size=4,family="sans"),
                   legend.title = element_text(colour=c("black"),face="bold",size=6,family="sans"),
                   legend.title.align = 0.5,
                   legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                   legend.key.size = unit(1, 'lines'),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank())

########
# RADs #  
########

rad_pred3$group<-as.character(rad_pred3$group)
rad_pred3$group<-ifelse(rad_pred3$group=="Hawai'i Island","Hawai'i",rad_pred3$group)
rad_pred3$group<-ifelse(rad_pred3$group=="Kaua'i Island","Kaua'i",rad_pred3$group)
rad_pred3$group<-ifelse(rad_pred3$group=="O'ahu Island","O'ahu",rad_pred3$group)
rad_pred3$group<-as.factor(rad_pred3$group)

rad_pred3$group<-as.factor(rad_pred3$group)
rad_pred3$group<-factor(rad_pred3$group,levels=c("Hawai'i","Maui Nui","O'ahu","Kaua'i"))

rad_scen3<-ggplot(rad_pred3, aes(x, predicted,group=group,colour=group,fill=group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,fill=group), colour="transparent", alpha = .3)+
  #scale_colour_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  #scale_fill_manual(values=c("#d7191c","#fdae61","#abd9e9","#2c7bb6"))+
  scale_color_viridis_d(option="D")+
  scale_fill_viridis_d(option="D")+
  coord_cartesian(ylim=c(0,1))+
  ylab("% Species")+xlab("% Abundance (log scale)")+
  #scale_color_npg()+
  guides(colour=guide_legend(title=""),fill="none")+
  theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=7,family="sans"),
                   axis.title.y=element_text(colour="black",face="bold",size=7,family="sans"),
                   axis.text.x=element_text(colour="black",face="bold",size=6,family="sans"),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6,family="sans"),
                   legend.text=element_text(colour=c("black"),face="bold",size=4,family="sans"),
                   legend.title = element_text(colour=c("black"),face="bold",size=6,family="sans"),
                   legend.title.align = 0,
                   legend.position=c("none"),
                   legend.key.size = unit(1, 'lines'),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank())

########################
# combine all ##########
########################

require(cowplot)

togg2<-plot_grid(p_scen2,p_scen3,rad_scen2,rad_scen3,
                 labels=c("a)","b)","c)","d)"),label_size = 7,
                 ncol=2)

ggsave(filename = file.path("Figures", "SACs_RADs_Natives_Scen2_Scen3_10plots_Fig3.png"), 
       width    = 11.4, 
       height   = 8.55, 
       units    = "cm", dpi=900)


togg2

dev.off()

# as pdf

ggsave(filename = file.path("Figures", "SACs_RADs_Natives_Scen2_Scen3_10plots_Fig3.pdf"), 
       width    = 11.4, 
       height   = 8.55, 
       units    = "cm", dpi=900)

togg2

dev.off()
