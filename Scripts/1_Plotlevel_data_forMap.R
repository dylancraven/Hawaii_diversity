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

datt<-read.csv("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Cleaned_Data/HawIslandsAbundance_2SizeClasses_100plus.csv",header=T)

datt<-filter(datt, Plot_Prop_Invaded<=0.75 & SizeClass=="all")  
datt$SizeClass<-droplevels(datt$SizeClass)

datt<-filter(datt, Native_Status_HawFlora_simple=="native") 

#####
#qc #
#####

length(unique(datt$PlotIDn)) # 421 plots
length(unique(datt$SPP_CODE3A)) #113 spp   
range(datt$Plot_Area) #  100.0037 1017.8760

####

h_comm3<-dcast(datt, PlotIDn~SPP_CODE3A, value.var="Abundance_ha",sum)
rownames(h_comm3)<-h_comm3$PlotIDn
h_comm3<-select(h_comm3,-PlotIDn)

# group information

h_attr<-unique(select(datt,PlotIDn, geo_entity2))

h_attr<-arrange(h_attr,PlotIDn)

h_attr<-data.frame(h_attr)
rownames(h_attr)<-h_attr$PlotIDn
h_attr<-select(h_attr,-PlotIDn)
colnames(h_attr)<-"group"
h_attr$group<-as.factor(h_attr$group)

# make mob structure
h_mob_in <- make_mob_in(h_comm3, h_attr)

h_stats <- get_mob_stats(h_mob_in, group_var = "group",nperm=10)

h_plot_div<-data.frame(h_stats$samples)
h_plot_div$PlotIDn<-rownames(h_plot_div)
h_plot_div<-select(h_plot_div, PlotIDn, S_asymp, S_rare10=S_rare.n...10)
h_plot_div$PlotIDn<-as.integer(h_plot_div$PlotIDn)

h_map_data<-distinct(select(datt, geo_entity2, PlotIDn, Lat_Dec, Long_Dec, Elev_m, MAT, MAP, PrecipSeasonality, TempSeasonality, PET, HII, Plot_Prop_Invaded))

h_map_data<-left_join(h_map_data, h_plot_div, by="PlotIDn")

write.table(h_map_data,"/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Cleaned_Data/Hawaii_mapdata.csv",sep=",",row.names=F)
