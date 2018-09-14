########################
# Calculate Alpha div. #
# for all plots in data#
# set ##################
########################

require(dplyr)
require(reshape2)
require(BBmisc)
require(mobr)

########

datt<-read.csv("Cleaned_Data/HawIslandsAbundance_2SizeClasses_100plus.csv",header=T)


datt<-filter(datt, Plot_Prop_Invaded<=0.75 & SizeClass==5) %>%
  filter(., Native_Status=="native")

#####
#qc #
#####

length(unique(datt$PlotID)) # 429 plots
length(unique(datt$SPP_CODE3A)) #113 spp   
range(datt$Plot_Area) #  100.0037 1017.8760

####

h_comm3<-dcast(datt, PlotID~SPP_CODE3A, value.var="Abundance_ha",sum)
rownames(h_comm3)<-h_comm3$PlotID
h_comm3<-select(h_comm3,-PlotID)

# group information

h_attr<-unique(select(datt,PlotID, geo_entity2))

h_attr<-arrange(h_attr,PlotID)

h_attr<-data.frame(h_attr)
rownames(h_attr)<-h_attr$PlotID
h_attr<-select(h_attr,-PlotID)
colnames(h_attr)<-"group"
h_attr$group<-as.factor(h_attr$group)

# make mob structure
h_mob_in <- make_mob_in(h_comm3, h_attr)

h_stats <- get_mob_stats(h_mob_in, group_var = "group",nperm=10, n_min=100)

h_plot_div<-data.frame(h_stats$samples)
h_plot_div$PlotID<-rownames(h_plot_div)
h_plot_div<-select(h_plot_div, PlotID, S_asymp, S_rare100=S_rare.n...100)
h_plot_div$PlotID<-as.integer(h_plot_div$PlotID)

h_map_data<-distinct(select(datt, geo_entity2, PlotID, Lat_Dec, Long_Dec, Elev_m, MAT, MAP, PrecipSeasonality, TempSeasonality, PET, HII, Plot_Prop_Invaded))

h_map_data<-left_join(h_map_data, h_plot_div, by="PlotID",all.x=TRUE)

write.table(h_map_data,"Cleaned_Data/Hawaii_mapdata.csv",sep=",",row.names=F)
