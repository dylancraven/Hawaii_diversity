#####################
# Island - SAC ######
#####################

require(dplyr)
require(reshape2)
require(vegan)
require(ggplot2)
require(ggsci)
require(iNEXT)
#require(mobr)

######################
# JUST NATIVE ########
# all trees> 2.54 ####
######################

######################
## data ##############
######################

datt<-read.csv("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Cleaned_Data/HawIslandsAbundance_2SizeClasses_100plus.csv",header=T)

datt<-filter(datt, Plot_Prop_Invaded<=0.75 & SizeClass=="all")  
datt$SizeClass<-droplevels(datt$SizeClass)

datt<-filter(datt, Native_Status_HawFlora_simple=="native") 

#####
#qc #
#####

length(unique(datt$PlotIDn)) # 420 plots
length(unique(datt$SPP_CODE3A)) #115 plots   
range(datt$Plot_Area) #  100.0037 1017.8760
quantile(datt$Plot_Area, probs=c(0.5)) # median = 1000
       
##########################
# prepare data for iNExt #
##########################
    
datt2<-summarize(group_by(datt, geo_entity2, SPP_CODE3A),Abundance=sum(Abundance_ha))

hcomm2<-dcast(datt2, SPP_CODE3A~geo_entity2, value.var="Abundance",sum)
rownames(hcomm2)<-hcomm2$SPP_CODE3A
hcomm2<-select(hcomm2,-SPP_CODE3A)


#####################
# SACs (Abundance)  #
#####################

h<-iNEXT(hcomm2,q=c(0),datatype="abundance")

curves<-do.call(rbind.data.frame,h$iNextEst)
curves$geo_entity2<-rownames(curves)
curves$geo_entity2 <- gsub("\\..*","",curves$geo_entity2)

write.table(curves,"/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Cleaned_Data/Scen1_Natives_curves_estimates.csv",sep=",",row.names=F)

# SACs (zoom) up to 10,000

jj<-iNEXT(hcomm2,q=c(0),size=c(1,100,1000, 2000,3000,4000,5000,6000,7000,8000,9000,10000),datatype="abundance")

curves3<-do.call(rbind.data.frame,jj$iNextEst)
curves3$geo_entity2<-rownames(curves3)
curves3$geo_entity2 <- gsub("\\..*","",curves3$geo_entity2)

write.table(curves3,"/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Cleaned_Data/Scen1_Natives_zoom_curves_estimates.csv",sep=",",row.names=F)

#################
# hill Numbers  #
#################

orders<-estimateD(hcomm2,datatype="abundance",base="coverage", level=0.98,conf=0.95 )
colnames(orders)[1]<-"geo_entity2"

write.table(orders,"/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Cleaned_Data/Scen1_Natives_HillNumbers.csv",sep=",",row.names=F)


#############
## RADs #####
#############

dattN<-dcast(datt2, geo_entity2~SPP_CODE3A,value.var="Abundance",sum)
rownames(dattN)<-dattN$geo_entity2
nn<-dim(dattN)[2]

datt3 <- decostand(dattN[,2:nn], method = "total")
datt3$geo_entity2<-rownames(datt3)
oo<-dim(datt3)[2]-1
datt3<-melt(datt3,id.vars="geo_entity2",measure.vars=1:oo,variable.name="SPP_CODE3A",value.name="RelAbund")

radHaw<-filter(datt3, geo_entity2=="Hawai'i Island")
radHaw<-arrange(radHaw,RelAbund)
radHaw<-filter(radHaw,RelAbund>0)
n<-dim(radHaw)[1]
radHaw$Rank<-seq(from=1,to=n,by=1)
radHaw$Rank_N<-radHaw$Rank/n

radKa<-filter(datt3, geo_entity2=="Kaua'i Island")
radKa<-arrange(radKa,RelAbund)
radKa<-filter(radKa,RelAbund>0)
o<-dim(radKa)[1]
radKa$Rank<-seq(from=1,to=o,by=1)
radKa$Rank_N<-radKa$Rank/o

radOah<-filter(datt3, geo_entity2=="O'ahu Island")
radOah<-arrange(radOah,RelAbund)
radOah<-filter(radOah,RelAbund>0)
p<-dim(radOah)[1]
radOah$Rank<-seq(from=1,to=p,by=1)
radOah$Rank_N<-radOah$Rank/p
radOah$geo_entity2<-"O'ahu Island"

radMN<-filter(datt3, geo_entity2=="Maui Nui")
radMN<-arrange(radMN,RelAbund)
radMN<-filter(radMN,RelAbund>0)
q<-dim(radMN)[1]
radMN$Rank<-seq(from=1,to=q,by=1)
radMN$Rank_N<-radMN$Rank/q

radd<-rbind.data.frame(radHaw,radKa,radMN,radOah)

radd$geo_entity2<-as.factor(radd$geo_entity2)
radd$geo_entity2<-factor(radd$geo_entity2,levels=c("Hawai'i Island","Maui Nui","O'ahu Island","Kaua'i Island"))

write.table(radd,"/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Cleaned_Data/Scen1_Natives_RAD.csv",sep=",",row.names=F)

