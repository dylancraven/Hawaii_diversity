##########################
# Distance between plots # 
# Scenario: 'Het + Age'  #
# Species: Native        #
##########################

require(dplyr)
require(tidyr)
require(tibble)
require(ecodist)
require(vegan)
require(sp)

# load data

dat<-read.csv("Cleaned_Data/HawIslandsAbundance_2SizeClasses_100plus.csv",header=T)

datt<- select(dat,PlotID, geo_entity2, Lat_Dec, Long_Dec, Elev_m, MAT, MAP, AridInd, 
              SubstrateAge_range)%>% distinct(.)

beta_div<-read.csv("Cleaned_Data/Scen2_Natives_10plots_BetaPIE.csv",stringsAsFactors = FALSE)

# merge data

big_beta<-left_join(beta_div, datt, by.y=c("geo_entity2", "PlotID"))

# qc check
qcc<-big_beta%>% group_by(geo_entity2, Iteration)%>% summarize(plotn=length(unique(PlotID)))

qcc2<- qcc%>% group_by(geo_entity2)%>% summarize(iter=length(unique(Iteration)))

# calculate geographical distances between all sites

big_beta<- big_beta%>% unite("iter_island",c("Iteration","geo_entity2"),sep=":",remove=F)

out_sel<-list(); 

for(i in 1:400){
  #for(j in 1:100){
  
  bib<-filter(big_beta, iter_island == unique(iter_island)[i])
 
  geo_dist<-select(bib, PlotID, Lat_Dec, Long_Dec)%>%
    distinct(.)
  
  coords<-select(geo_dist, Long_Dec, Lat_Dec)
  data<-select(geo_dist, PlotID)
  crs<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  isl_dist <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string = crs)
  
  isl_dist<-spDists(isl_dist, longlat = TRUE, diagonal = FALSE)
  isl_dist<-data.frame(isl_dist)
  isl_dist$Plot<-rownames(isl_dist)
  isl_dist_l<-pivot_longer(isl_dist, cols=1:10, names_to="other",values_to = "Distance_km")
  isl_dist_l$other<-sub('.', '', isl_dist_l$other)
  isl_dist_l<-filter(isl_dist_l, Plot!=other) # get rid of same plot to same plot
  
  isl_dist_l<- isl_dist_l %>%
    unite('tog',c('Plot','other'),sep=":",remove=FALSE) # get rid of duplicate edges
  
  isl_dist_l$dupes<-duplicated(isl_dist_l$Distance_km)
  
  isl_dist_ll<-filter(isl_dist_l, dupes==FALSE)
  
  mean_dist<-mean(isl_dist_ll$Distance_km)
  min_dist<-min(isl_dist_ll$Distance_km)
  
  partz<-cbind.data.frame(iter_island=unique(bib$iter_island), mean_dist,min_dist)
  out_sel[[i]]<-rbind.data.frame( partz)
}

out_sell<-do.call(rbind.data.frame, out_sel)

out_sell<- out_sell%>%
           separate(iter_island,into=c("Iteration","geo_entity2"),sep=":",remove=TRUE)

out_sell$Scenario<-"Het+Age"

###########################
## Scenario 3 #############
###########################

beta_div2<-read.csv("Cleaned_Data/Scen3_Natives_10plots_BetaPIE.csv",stringsAsFactors = FALSE)

# merge data

big_beta_2<-left_join(beta_div2, datt, by.y=c("geo_entity2", "PlotID"))

# qc check
qcc<-big_beta_2%>% group_by(geo_entity2, Iteration)%>% summarize(plotn=length(unique(PlotID)))

qcc2<- qcc%>% group_by(geo_entity2)%>% summarize(iter=length(unique(Iteration)))

# calculate geographical distances between all sites

big_beta_2<- big_beta_2%>% unite("iter_island",c("Iteration","geo_entity2"),sep=":",remove=F)

out_sel_n<-list(); 

for(i in 1:400){
  #for(j in 1:100){
  
  bib<-filter(big_beta_2, iter_island == unique(iter_island)[i])
  
  geo_dist<-select(bib, PlotID, Lat_Dec, Long_Dec)%>%
    distinct(.)
  
  coords<-select(geo_dist, Long_Dec, Lat_Dec)
  data<-select(geo_dist, PlotID)
  crs<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  isl_dist <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string = crs)
  
  isl_dist<-spDists(isl_dist, longlat = TRUE, diagonal = FALSE)
  isl_dist<-data.frame(isl_dist)
  isl_dist$Plot<-rownames(isl_dist)
  isl_dist_l<-pivot_longer(isl_dist, cols=1:10, names_to="other",values_to = "Distance_km")
  isl_dist_l$other<-sub('.', '', isl_dist_l$other)
  isl_dist_l<-filter(isl_dist_l, Plot!=other) # get rid of same plot to same plot
  
  isl_dist_l<- isl_dist_l %>%
    unite('tog',c('Plot','other'),sep=":",remove=FALSE) # get rid of duplicate edges
  
  isl_dist_l$dupes<-duplicated(isl_dist_l$Distance_km)
  
  isl_dist_ll<-filter(isl_dist_l, dupes==FALSE)
  
  mean_dist<-mean(isl_dist_ll$Distance_km)
  min_dist<-min(isl_dist_ll$Distance_km)
  
  partz<-cbind.data.frame(iter_island=unique(bib$iter_island), mean_dist,min_dist)
  out_sel_n[[i]]<-rbind.data.frame( partz)
}

out_sell_n<-do.call(rbind.data.frame, out_sel_n)

out_sell_n<- out_sell_n%>%
  separate(iter_island,into=c("Iteration","geo_entity2"),sep=":",remove=TRUE)

out_sell_n$Scenario<-"Age"

#### join data frames

plotdist_native<-rbind.data.frame(out_sell, out_sell_n)

plotdist_native<-plotdist_native%>%
                 group_by(Scenario)%>%
                 summarize(mean_distt=mean(mean_dist), min_distt=mean(min_dist),
                          min_min=min(min_dist))

write.csv(plotdist_native,"Cleaned_Data/PlotDistance_Natives_Scen23.csv",row.names = FALSE)

## each island

plotdist_native_island<-plotdist_native%>%
  group_by(Scenario, geo_entity2)%>%
  summarize(mean_distt=mean(mean_dist), min_distt=mean(min_dist),
            min_min=min(min_dist))

write.csv(plotdist_native_island,"Cleaned_Data/PlotDistance_Islands_Natives_Scen23.csv",row.names = FALSE)
