######################################################
# convert tree-level data to abundance data   ########
######################################################
# 1) calculate abundance data for two size classes  ##
# 2) add in SPP information                         ##
######################################################

len_un <- function (x){length(unique(x))}

replace_na_with_last<-function(x,a=!is.na(x)){
  x[which(a)[c(1,1:sum(a))][cumsum(a)+1]]
}

require(here)
require(dplyr)
require(tidyr)
require(reshape2)

########################
# load data ############
########################

spp<-read.delim("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Data/Hawaii_fullSPP_clean.csv",sep=",",header=T)
spp<-select(spp, SPP_CODE3A,Native_Status_HawFlora_simple)
spp<-unique(spp)

tog<-read.delim("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Data/HawaiiOnly_TreeData_122017.csv",sep=",",header=T)

togg<-select(tog, geo_entity_ref, geo_entity, Study, PlotIDn, Plot_Area,SPP_CODE3A,ID, DBH_cm,Abundance, Abundance_ha)

##############################
# Part I: Make size classes  #
##############################

## All individuals greater than 2.54

togg$SizeClass_All<-ifelse(togg$DBH_cm>=2.54,1,NA)

togg$SizeClass_All<-ifelse(togg$Study=="PACN" & is.na(togg$DBH_cm)==TRUE, 1,togg$SizeClass_All) # Add in saplings from PACN

## All individuals greater than 12.7  (FIA cut-off)

togg$SizeClass_Big<-ifelse(togg$DBH_cm>=12.7,1,NA)

### separate into size classes: all, big (>10)

all<-filter(togg, SizeClass_All==1)
all<-select(all,-SizeClass_All)
all$SizeClass<-"all"

#########
## qc ###
#########

min(all$DBH_cm,na.rm=T) # 2.54

max(all$DBH_cm,na.rm=T) # 250

######

all2<-summarize(group_by(all, geo_entity_ref,geo_entity,Study, PlotIDn,Plot_Area, SizeClass,SPP_CODE3A), 
                Abundance_ha=sum(Abundance_ha))

########

big<-filter(togg,SizeClass_Big==1)
big<-select(big,-SizeClass_Big)
big$SizeClass<-"big"

#########
## qc ###
#########

min(big$DBH_cm,na.rm=T) # 12.7

max(big$DBH_cm,na.rm=T) # 250


####

big2<-summarize(group_by(big, geo_entity_ref,geo_entity,Study, PlotIDn,Plot_Area, SizeClass,SPP_CODE3A), 
                Abundance_ha=sum(Abundance_ha))

togg2<-rbind.data.frame(all2, big2) # all size classes

####################################
# Part II: add in SPP information  #
####################################

togg3<-merge(togg2, spp, by.y="SPP_CODE3A")

togg33<-select(togg3, geo_entity_ref, geo_entity, Study, PlotIDn, Plot_Area,SPP_CODE3A, Native_Status_HawFlora_simple, SizeClass,Abundance_ha)

togg33<-filter(togg33, Study!="HIPPNET") # b/c plot is too big

togg33<-filter(togg33, Plot_Area>=100)

togg33$geo_entity2<-togg33$geo_entity
togg33$geo_entity2<-as.character(togg33$geo_entity2)
togg33$geo_entity2<-ifelse(togg33$geo_entity2=="Maui Island"|togg33$geo_entity2=="Lana'i Island"|togg33$geo_entity2=="Moloka'i Island","Maui Nui",togg33$geo_entity2)

togg33$geo_entity2<-ifelse(togg33$geo_entity2=="O'ahu Island (incl. Mokoli'i Islet)","O'ahu Island",togg33$geo_entity2)

togg33$Abundance_ha<-round(togg33$Abundance_ha)


########################
# Proportion Invaded ###
########################

tog_tog<- dcast(togg33, geo_entity+ PlotIDn + SizeClass~Native_Status_HawFlora_simple,value.var="Abundance_ha",sum)
tog_tog<-tog_tog %>%
  mutate(Tot_Abund= alien+native+uncertain) 


tog_tog<-tog_tog %>%
  mutate(PropInvaded= alien/Tot_Abund) 

tog_tog<-select(tog_tog, PlotIDn, SizeClass,PropInvaded)


togg34<-merge(togg33, tog_tog,by.y=c("PlotIDn", "SizeClass"))

########################
# add in climate data ##
########################

env<-read.csv("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Data/Hawaiian_EnvData_1km.csv",sep=",",header=T)
env<-select(env,-CellID)


togg35<-merge(togg34, env,by.y=c("PlotIDn"))

########################
# arrange ##############
########################

togg36<-select(togg35,geo_entity_ref, geo_entity, geo_entity2, Study, PlotIDn, Plot_Area, Lat_Dec, Long_Dec, Elev_m,MAT, MAP, PrecipSeasonality, TempSeasonality, PET,
                HFP, HII, Plot_Prop_Invaded=PropInvaded, SizeClass, SPP_CODE3A, Native_Status_HawFlora_simple, Abundance_ha)


write.table(togg36,"/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Cleaned_Data/HawIslandsAbundance_2SizeClasses_100plus.csv",sep=",",row.names=F)

