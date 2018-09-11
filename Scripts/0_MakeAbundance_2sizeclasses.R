######################################################
# convert tree-level data to abundance data   ########
######################################################
######################################################

len_un <- function (x){length(unique(x))}

replace_na_with_last<-function(x,a=!is.na(x)){
  x[which(a)[c(1,1:sum(a))][cumsum(a)+1]]
}

require(here)
require(dplyr)
require(tidyr)
require(stringr)
require(reshape2)

########################
# load data ############
########################

spp<-read.delim("Data/Hawaii_fullSPP_clean.csv",sep=",",header=T)
spp<-select(spp, Scientific_name=Accepted_name_species, SPP_CODE3A)
spp$Scientific_name<-str_replace_all(spp$Scientific_name, "spp1", "spp.")
spp$Scientific_name<-str_replace_all(spp$Scientific_name, "spp2", "spp.")
spp$Scientific_name<-str_replace_all(spp$Scientific_name, "spp3", "spp.")
spp$Scientific_name<-str_replace_all(spp$Scientific_name, "spp4", "spp.")
spp$Scientific_name<-str_replace_all(spp$Scientific_name, "spp5", "spp.")
spp$Scientific_name<-str_replace_all(spp$Scientific_name, "spp6", "spp.")

spp<-unique(spp)


tog<-read.delim("Data/OpenNahele_Tree_Data.csv",sep=",",header=T)

togg<-select(tog, Island, Study, PlotID, Plot_Area,Scientific_name, Native_Status, Tree_ID, DBH_cm,Abundance, Abundance_ha)

togg$geo_entity2<-togg$Island
togg$geo_entity2<-as.character(togg$geo_entity2)
togg$geo_entity2<-ifelse(togg$geo_entity2=="Maui Island"|togg$geo_entity2=="Lana'i Island"|togg$geo_entity2=="Moloka'i Island","Maui Nui",togg$geo_entity2)

togg$geo_entity2<-ifelse(togg$geo_entity2=="O'ahu Island (incl. Mokoli'i Islet)","O'ahu Island",togg$geo_entity2)

togg$Scientific_name<-as.character(togg$Scientific_name)

togg<-left_join(togg, spp, by="Scientific_name")

togg<-select(togg, PlotID, Study, Island, geo_entity2, Plot_Area, Tree_ID, Scientific_name, SPP_CODE3A, Native_Status, DBH_cm, Abundance,  Abundance_ha)

##############################
# Part I: Filter data        #
##############################

# Criteria: plot size > 100 m2 & eliminate HIPPNET (too big) 

togg1<- filter(togg, Plot_Area>=100)%>%
        filter(., Study!="HIPPNET")

#########
## qc ###
#########

min(togg1$DBH_cm,na.rm=T) # 5

max(togg1$DBH_cm,na.rm=T) # 214

#################
# trees > 5 cm  #
#################

all<-summarize(group_by(togg1, Study, Island, geo_entity2, PlotID,Plot_Area, Scientific_name,SPP_CODE3A, Native_Status), Abundance=sum(Abundance),
                Abundance_ha=sum(Abundance_ha))

all$Abundance_ha<-round(all$Abundance_ha)

all$SizeClass<-"5"

#################
# trees > 10 cm  #
#################

big<- filter(togg1, DBH_cm>=10 & is.na(DBH_cm)==FALSE )

bigg<-summarize(group_by(big, Study, Island, geo_entity2, PlotID,Plot_Area, Scientific_name,SPP_CODE3A, Native_Status), Abundance=sum(Abundance),
               Abundance_ha=sum(Abundance_ha))

bigg$Abundance_ha<-round(bigg$Abundance_ha)

bigg$SizeClass<-"10"


all_big<-rbind.data.frame(all, bigg) # both size classes together

#########################################
# Part II: Identify highly invaded plots  #
#########################################

tog_tog<- dcast(all_big, PlotID +SizeClass~Native_Status,value.var="Abundance_ha",sum)
tog_tog<-tog_tog %>%
  mutate(Tot_Abund= alien+native+uncertain) 

tog_tog<-tog_tog %>%
  mutate(PropInvaded= alien/Tot_Abund) 

tog_tog<-select(tog_tog, PlotID, SizeClass,PropInvaded)

all_bigg<-merge(all_big, tog_tog,by.y=c("PlotID", "SizeClass"))

########################
# add in climate data ##
########################

env<-read.csv("Data/Hawaiian_Env_Soil_1km.csv",sep=",",header=T)
colnames(env)[1]<-"PlotID"

all_bigg<-left_join(all_bigg, env,by.y=c("PlotID"))

########################
# arrange ##############
########################

all_bigg2<-select(all_bigg, PlotID, Study, Island, geo_entity2, Plot_Area, Lat_Dec, Long_Dec, Elev_m,MAT, MAP, PrecipSeasonality, TempSeasonality, PET,
                HFP, HII, Plot_Prop_Invaded=PropInvaded, SizeClass, Scientific_name, SPP_CODE3A, Native_Status, Abundance, Abundance_ha)


all_bigg2<-arrange(all_bigg2, PlotID, SizeClass)

write.table(all_bigg2,"/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Cleaned_Data/HawIslandsAbundance_2SizeClasses_100plus.csv",sep=",",row.names=F)

