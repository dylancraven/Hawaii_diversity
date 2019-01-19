##############################################
# Process Hawaiian flora (just woody plants) #
##############################################

require(dplyr)

###############################################
# Data  #######################################
# source: Wagner, W. L., Herbst, D. L., & Lorence, D. H. (2005). Flora of the Hawaiian Islands website. Retrieved 1 May 2017, from http://botany.si.edu/pacificislandbiodiversity/hawaiianflora/index.htm

haw_woody<-read.csv("Data/Hawaii_Woodiness_cleanMay2017.csv",header=TRUE)

haw_woody<-haw_woody%>%
           filter(., Location==1)
haw_woody$Island<-as.character(haw_woody$Island)
haw_woody$geo_entity2<-NA

haw_woody$geo_entity2<-ifelse(haw_woody$Island=="H","Hawai'i Island",NA)
haw_woody$geo_entity2<-ifelse(haw_woody$Island=="K","Kaua'i Island",haw_woody$geo_entity2)
haw_woody$geo_entity2<-ifelse(haw_woody$Island=="Ka","Maui Nui",haw_woody$geo_entity2)
haw_woody$geo_entity2<-ifelse(haw_woody$Island=="L","Maui Nui",haw_woody$geo_entity2)
haw_woody$geo_entity2<-ifelse(haw_woody$Island=="M","Maui Nui",haw_woody$geo_entity2)
haw_woody$geo_entity2<-ifelse(haw_woody$Island=="Mo","Maui Nui",haw_woody$geo_entity2)
haw_woody$geo_entity2<-ifelse(haw_woody$Island=="Ni","Nihau",haw_woody$geo_entity2)
haw_woody$geo_entity2<-ifelse(haw_woody$Island=="O","O'ahu Island",haw_woody$geo_entity2)
haw_woody$geo_entity2<-ifelse(is.na(haw_woody$geo_entity2)==TRUE, haw_woody$Island,haw_woody$geo_entity2)

haw_woody<-select(haw_woody, ID, Accepted_name_species, Native_status, Island, geo_entity2, P_A=Location )

##########################
# Single-island endemics #
##########################

sie<-summarise(group_by(haw_woody, Accepted_name_species), islands_present=sum(P_A))

sie$islands_present<-ifelse(sie$islands_present!=1, 0,sie$islands_present)

haw_woody2<-left_join(haw_woody, sie, by="Accepted_name_species")

#######################
# Species richness ####
#######################

isl_summ<-summarize(group_by(haw_woody2, geo_entity2,Native_status), SppN=length(unique(Accepted_name_species)))
isl_summ<-select(isl_summ, geo_entity2, species_group=Native_status, SppN)

isl_summ2<-summarize(group_by(haw_woody2, geo_entity2), SppN=length(unique(Accepted_name_species)))
isl_summ2$species_group<-"all"

###########################
### single-island endemic #
###########################

sie2<-filter(haw_woody2, Native_status=="native" & islands_present==1)

sie22<-summarize(group_by(sie2,geo_entity2,Native_status),SppN=length(unique(Accepted_name_species)))
sie22$species_group<-"nSIE"
sie22$Native_status<-NULL

###########
# combine #
###########

togg<-rbind.data.frame(isl_summ, isl_summ2, sie22)
togg$geo_entity2<-as.character(togg$geo_entity2)

isls<-c("Hawai'i Island","Maui Nui", "Kaua'i Island","O'ahu Island")

togg<-filter(togg, geo_entity2 %in% isls)

write.csv(togg, "Data/Hawaii_Div_Macro_SIE.csv",row.names=FALSE)
