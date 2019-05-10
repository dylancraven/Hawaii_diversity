########################
# Calculate Alpha div. #
# for all plots in data#
# set ##################
########################

require(dplyr)
require(reshape2)
#require(BBmisc)
require(mobr)

########

dat<-read.csv("Cleaned_Data/HawIslandsAbundance_2SizeClasses_100plus.csv",header=T)

datt<-filter(dat, SizeClass==5) %>%
  filter(., Native_Status=="alien" |Native_Status=="native")

#####
#qc #
#####

length(unique(datt$PlotID)) # 517 plots
length(unique(datt$SPP_CODE3A)) # 166 spp   
range(datt$Plot_Area) #  100.0037 1017.8760

summarise(group_by(datt, geo_entity2), PlotArea=mean(Plot_Area), 
          PlotN=length(unique(PlotID)))

####

h_comm3<-dcast(datt, PlotID~SPP_CODE3A, value.var="Abundance_ha",sum)
rownames(h_comm3)<-h_comm3$PlotID
h_comm3<-select(h_comm3,-PlotID)

# group information

groups<-unique(select(datt,PlotID, geo_entity2))
groups<-arrange(groups,PlotID)
rownames(groups)<-groups$PlotID
groupss<-as.character(groups$geo_entity2)

h_stats<-calc_biodiv(h_comm3, groups=groupss,effort=100,extrapolate=TRUE,
                     index=c("N","S","S_n","S_asymp","S_PIE"),return_NA=FALSE)

h_stats$PlotID<-groups$PlotID

h_plot_divv<-dcast(h_stats, PlotID~index, value.var="value", sum)
h_plot_divv$PlotID<-as.integer(h_plot_divv$PlotID)

h_map_data<-distinct(select(datt, geo_entity2, PlotID, Lat_Dec, Long_Dec, MAT, MAP, PET, 
                            AridInd,SubstrateAge_range, Plot_Prop_Invaded))

h_map_data<-left_join(h_plot_divv, h_map_data, by="PlotID",all.x=TRUE)

write.table(h_map_data,"Cleaned_Data/Hawaii_mapdata.csv",sep=",",row.names=F)
