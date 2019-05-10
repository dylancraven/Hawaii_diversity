#########################
# Island - SAC         ##
# Plot selection       ##
# Scenario: 'Age'      ##
#########################
# just natives (10 plots)
#########################

require(dplyr)
require(tidyr)
require(ecodist)
require(vegan)
require(sp)

sample_n_groups = function(tbl, size, replace = FALSE, weight = NULL) {
  # regroup when done
  grps = tbl %>% groups %>% lapply(as.character) %>% unlist
  # check length of groups non-zero
  keep = tbl %>% summarise() %>% ungroup() %>% sample_n(size, replace, weight)
  # keep only selected groups, regroup because joins change count.
  # regrouping may be unnecessary but joins do something funky to grouping variable
  tbl %>% right_join(keep, by=grps) %>% group_by(.dots = grps)
}

######################
## data ##############
######################

dat<-read.csv("Cleaned_Data/HawIslandsAbundance_2SizeClasses_100plus.csv",header=T)

length(unique(dat$PlotID)) # 517

datt<-filter(dat, SizeClass==5) %>%
  filter(., Native_Status=="native")

#####
#qc #
#####

length(unique(datt$PlotID)) # 473 plots
length(unique(datt$SPP_CODE3A)) # 104 spp   
range(datt$Plot_Area) #  100.0037 1017.8760
quantile(datt$Plot_Area, probs=c(0.5)) # median = 1000

datt%>%
  group_by(geo_entity2)%>%
  summarise(PlotArea=mean(Plot_Area), PlotArea_sd=sd(Plot_Area), 
            PlotN=length(unique(PlotID))) # min 19 plots (Kauai and Oahu)

##########################################################
# Aridity and elevation ranges (across all islands)      #
##########################################################

scen2_selplots<-read.csv("Cleaned_Data/Scen2_Natives_SelPlots.csv", header=TRUE)
scen2_selplotss<-select(scen2_selplots,  -Plot_Area)

datt_total<-left_join(scen2_selplotss,datt, by=c('geo_entity2','PlotID'))
datt_total<-select(datt_total, Iteration, geo_entity2, PlotID, Elev_m, AridInd, MAP)%>% 
  distinct(.)

clim_isl<- datt_total%>% group_by(geo_entity2, Iteration) %>%
  summarize(mean_MAP=mean(MAP), min_MAP=min(MAP),max_MAP=max(MAP),
            mean_Arid=mean(AridInd),min_Arid=min(AridInd),
            max_Arid=max(AridInd),min_Elev=min(Elev_m),max_Elev=max(Elev_m),
            PlotN=length(unique(PlotID)))

clim_isl$r_MAP<-clim_isl$max_MAP-clim_isl$min_MAP
clim_isl$r_Arid<-clim_isl$max_Arid-clim_isl$min_Arid
clim_isl$r_Elev<-clim_isl$max_Elev-clim_isl$min_Elev

clim_isll<- clim_isl %>% group_by(geo_entity2)%>%
  summarize(mean_MAP_r=mean(r_MAP),
            lowerq_MAP_r=quantile(r_MAP,probs=0.10),
            upperq_MAP_r=quantile(r_MAP,probs=0.90),  
            mean_Arid_r=mean(r_Arid), lowerq_Arid_r=quantile(mean_Arid,probs=0.10),
            upperq_Arid_r=quantile(r_Arid,probs=0.90),
            Prob75q_Arid_r=quantile(r_Arid,probs=0.75),
            Prob50q_Arid_r=quantile(r_Arid,probs=0.50),
            mean_Elev_r=mean(r_Elev),lowerq_Elev_r=quantile(r_Elev,probs=0.10),
            upperq_Elev_r=quantile(r_Elev,probs=0.90),
            Prob75q_Elev_r=quantile(r_Elev,probs=0.75),
            Prob50q_Elev_r=quantile(r_Elev,probs=0.50))

# get ranges in total plot area per iteration (based on Kauai)

ka_plotarea<-filter(scen2_selplots, geo_entity2=="Kaua'i Island")%>%
  group_by(Iteration)%>% summarise(totPlotArea=sum(Plot_Area))

ka_plotarea<-cbind.data.frame(Plot_Area=mean(ka_plotarea$totPlotArea), 
                              Area_lower10=quantile(ka_plotarea$totPlotArea,probs=0.10),
                              Area_upper10=quantile(ka_plotarea$totPlotArea,probs=0.90))

ka_plotarea_min<-ka_plotarea$Area_lower10
ka_plotarea_max<-ka_plotarea$Area_upper10

#######################################
# Select plots within Aridity range   #
#         & min. dist >0.1 km         #
#######################################

ka<-filter(datt, geo_entity2=="Kaua'i Island")
ka_range<-filter(clim_isll,geo_entity2=="Kaua'i Island")%>%
  select(., Prob50q_Arid_r, Prob75q_Arid_r, Prob50q_Elev_r,Prob75q_Elev_r)

haw<-filter(datt, geo_entity2=="Hawai'i Island")
haw_range<-filter(clim_isll,geo_entity2=="Hawai'i Island")%>%
  select(., Prob50q_Arid_r, Prob75q_Arid_r, Prob50q_Elev_r,Prob75q_Elev_r)

oah<-filter(datt, geo_entity2=="O'ahu Island")
oah_range<-filter(clim_isll,geo_entity2=="O'ahu Island")%>%
  select(., Prob50q_Arid_r, Prob75q_Arid_r, Prob50q_Elev_r,Prob75q_Elev_r)

maui<-filter(datt, geo_entity2=="Maui Nui")
maui_range<-filter(clim_isll,geo_entity2=="Maui Nui")%>%
  select(., Prob50q_Arid_r, Prob75q_Arid_r, Prob50q_Elev_r,Prob75q_Elev_r)

#######################
# 4 big nasty loops   #
#######################

set.seed(29)

# Kauai 

ka_dist_Scen3<-list(); ka_sel_Scen3<-list();

for(i in 1:7500){
  ka_sel<- ka %>% group_by(geo_entity2,PlotID) %>% sample_n_groups(10,replace=F)%>%
    ungroup()
  
  # range in Aridity & Elevation
  ka_rangez<-select(ka_sel, geo_entity2, PlotID, Lat_Dec, Long_Dec, AridInd, Elev_m,
                    Plot_Area) %>% distinct(.)
  
  ka_rangezz<-ka_rangez%>% summarize(min_Arid=min(AridInd),max_Arid=max(AridInd),
                                     min_Elev=min(Elev_m),max_Elev=max(Elev_m),
                                     totPlotArea=sum(Plot_Area))
  
  ka_rangezz$r_Arid<-ka_rangezz$max_Arid-ka_rangezz$min_Arid
  ka_rangezz$r_Elev<-ka_rangezz$max_Elev-ka_rangezz$min_Elev
  ka_rangezz<-select(ka_rangezz, r_Arid, r_Elev,totPlotArea)
  
  # min distance between Plots
  
  k_geo_dist<-select(ka_rangez, PlotID, Lat_Dec, Long_Dec)
  k_coords<-select(k_geo_dist, Long_Dec, Lat_Dec)
  k_data<-select(k_geo_dist, PlotID)
  crs<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  k_dist <- SpatialPointsDataFrame(coords = k_coords,
                                   data = k_data, 
                                   proj4string = crs)
  
  k_distt<-spDists(k_dist, longlat = TRUE, diagonal = FALSE)
  k_distt<-data.frame(k_distt)
  k_distt$Plots<-rownames(k_distt)
  
  # convert dist. matrix to data frame; remove repeats 
  out_k<-pivot_longer(k_distt, cols=c(1:10),names_to= "Plots_from",values_to = "Distance",
                      names_prefix="X")
  
  plotss_k<-cbind.data.frame(Plots=rownames(k_geo_dist), PlotID=k_geo_dist$PlotID) # 
  plotss_k$Plots<-as.character(plotss_k$Plots)
  
  out_kk<-left_join(out_k, plotss_k, by="Plots")
  out_kk<-select(out_kk, Plots_to=PlotID, Plots=Plots_from, Distance)
  out_kk<-left_join(out_kk, plotss_k, by="Plots")
  out_kk<-select(out_kk, Plots_to, Plots_from= PlotID, Distance)
  
  out_kk<-filter(out_kk, !Plots_to==Plots_from)
  out_kkk<- out_kk[!duplicated(t(apply(out_kk, 1, sort))),] # only unique 
  
  min_dist_k<-min(out_kkk$Distance)
  
  cat("progress", i, sep=' ','\n')
  
  if(min_dist_k<=0.1) next 
  if(ka_rangezz$r_Arid>ka_range$Prob50q_Elev_r) next
  if(ka_rangezz$totPlotArea<ka_plotarea_min | ka_rangezz$totPlotArea>ka_plotarea_max) next
  
  ka_out<-select(ka_rangez, geo_entity2, PlotID,Plot_Area )
  ka_out$Iteration<-i
  
  ka_dist_Scen3[[i]]<-cbind.data.frame(Iteration=i,geo_entity2=unique(ka_sel$geo_entity2),
                                       min_dist_km=min_dist_k,totPlotArea=ka_rangezz$totPlotArea,
                                       r_Arid=ka_rangezz$r_Arid,r_Elev=ka_rangezz$r_Elev)
  
  ka_sel_Scen3[[i]]<-rbind.data.frame(ka_out)
}

ka_dist_Scen3<-do.call(rbind.data.frame,ka_dist_Scen3)

ka_sel_Scen3<-do.call(rbind.data.frame,ka_sel_Scen3)

# QC: number of iterations per island, Aridity range & plot area

qc<- ka_sel_Scen3 %>% group_by(geo_entity2) %>% summarize(iters=length(unique(Iteration)),
                                                          PlotN=length(unique(PlotID)))
qc2<- ka_dist_Scen3 %>% group_by(geo_entity2) %>% 
  summarize(iters=length(unique(Iteration)), min_dist=min(min_dist_km),
            min_Arid=min(r_Arid), max_Arid=max(r_Arid), mean_Aridr=mean(r_Arid),
            min_Elev=min(r_Elev),max_Elev=max(r_Elev), mean_Elevr=mean(r_Elev),
            maxPlotArea=max(totPlotArea), minPlotArea=min(totPlotArea))
# Maui Nui

ma_dist_Scen3<-list(); ma_sel_Scen3<-list();

for(i in 1:7500){
  ma_sel<- maui %>% group_by(geo_entity2,PlotID) %>% sample_n_groups(10,replace=F)%>%
    ungroup()
  
  # range in Aridity & Elevation
  ma_rangez<-select(ma_sel, geo_entity2, PlotID, Lat_Dec, Long_Dec, AridInd, Elev_m,
                    Plot_Area) %>% distinct(.)
  
  ma_rangezz<-ma_rangez%>% summarize(min_Arid=min(AridInd),max_Arid=max(AridInd),
                                     min_Elev=min(Elev_m),max_Elev=max(Elev_m),
                                     totPlotArea=sum(Plot_Area))
  
  ma_rangezz$r_Arid<-ma_rangezz$max_Arid-ma_rangezz$min_Arid
  ma_rangezz$r_Elev<-ma_rangezz$max_Elev-ma_rangezz$min_Elev
  ma_rangezz<-select(ma_rangezz, r_Arid, r_Elev,totPlotArea)
  
  # min distance between Plots
  
  m_geo_dist<-select(ma_rangez, PlotID, Lat_Dec, Long_Dec)
  m_coords<-select(m_geo_dist, Long_Dec, Lat_Dec)
  m_data<-select(m_geo_dist, PlotID)
  crs<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  m_dist <- SpatialPointsDataFrame(coords = m_coords,
                                   data = m_data, 
                                   proj4string = crs)
  
  m_distt<-spDists(m_dist, longlat = TRUE, diagonal = FALSE)
  m_distt<-data.frame(m_distt)
  m_distt$Plots<-rownames(m_distt)
  
  # convert dist. matrix to data frame; remove repeats 
  out_m<-pivot_longer(m_distt, cols=c(1:10),names_to= "Plots_from",values_to = "Distance",
                      names_prefix="X")
  
  plotss_m<-cbind.data.frame(Plots=rownames(m_geo_dist), PlotID=m_geo_dist$PlotID) # 
  plotss_m$Plots<-as.character(plotss_m$Plots)
  
  out_mm<-left_join(out_m, plotss_m, by="Plots")
  out_mm<-select(out_mm, Plots_to=PlotID, Plots=Plots_from, Distance)
  out_mm<-left_join(out_mm, plotss_m, by="Plots")
  out_mm<-select(out_mm, Plots_to, Plots_from= PlotID, Distance)
  
  out_mm<-filter(out_mm, !Plots_to==Plots_from)
  out_mmm<- out_mm[!duplicated(t(apply(out_mm, 1, sort))),] # only unique 
  
  min_dist_m<-min(out_mmm$Distance)
  
  cat("progress", i, sep=' ','\n')
  
  if(min_dist_m<=0.1) next 
  if(ma_rangezz$r_Arid>maui_range$Prob50q_Elev_r) next
  if(ma_rangezz$totPlotArea<ka_plotarea_min | ma_rangezz$totPlotArea>ka_plotarea_max) next
  
  ma_out<-select(ma_rangez, geo_entity2, PlotID,Plot_Area )
  ma_out$Iteration<-i
  
  ma_dist_Scen3[[i]]<-cbind.data.frame(Iteration=i,geo_entity2=unique(ma_sel$geo_entity2),
                                       min_dist_km=min_dist_m,totPlotArea=ma_rangezz$totPlotArea,
                                       r_Arid=ma_rangezz$r_Arid,r_Elev=ma_rangezz$r_Elev)
  
  ma_sel_Scen3[[i]]<-rbind.data.frame(ma_out)
}

ma_dist_Scen3<-do.call(rbind.data.frame,ma_dist_Scen3)

ma_sel_Scen3<-do.call(rbind.data.frame,ma_sel_Scen3)

# QC: number of iterations per island, Aridity range & plot area

qc<- ma_sel_Scen3 %>% group_by(geo_entity2) %>% summarize(iters=length(unique(Iteration)),
                                                          PlotN=length(unique(PlotID)))

qc2<- ma_dist_Scen3 %>% group_by(geo_entity2) %>% 
  summarize(iters=length(unique(Iteration)), min_dist=min(min_dist_km),
            min_Arid=min(r_Arid), max_Arid=max(r_Arid), mean_Aridr=mean(r_Arid),
            min_Elev=min(r_Elev),max_Elev=max(r_Elev), mean_Elevr=mean(r_Elev),
            maxPlotArea=max(totPlotArea), minPlotArea=min(totPlotArea))

# Oahu

oa_dist_Scen3<-list(); oa_sel_Scen3<-list();

for(i in 1:7500){
  oa_sel<- oah %>% group_by(geo_entity2,PlotID) %>% sample_n_groups(10,replace=F)%>%
    ungroup()
  
  # range in Aridity & Elevation
  oa_rangez<-select(oa_sel, geo_entity2, PlotID, Lat_Dec, Long_Dec, AridInd, Elev_m,
                    Plot_Area) %>% distinct(.)
  
  oa_rangezz<-oa_rangez%>% summarize(min_Arid=min(AridInd),max_Arid=max(AridInd),
                                     min_Elev=min(Elev_m),max_Elev=max(Elev_m),
                                     totPlotArea=sum(Plot_Area))
  
  oa_rangezz$r_Arid<-oa_rangezz$max_Arid-oa_rangezz$min_Arid
  oa_rangezz$r_Elev<-oa_rangezz$max_Elev-oa_rangezz$min_Elev
  oa_rangezz<-select(oa_rangezz, r_Arid, r_Elev,totPlotArea)
  
  # min distance between Plots
  
  o_geo_dist<-select(oa_rangez, PlotID, Lat_Dec, Long_Dec)
  o_coords<-select(o_geo_dist, Long_Dec, Lat_Dec)
  o_data<-select(o_geo_dist, PlotID)
  crs<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  o_dist <- SpatialPointsDataFrame(coords = o_coords,
                                   data = o_data, 
                                   proj4string = crs)
  
  o_distt<-spDists(o_dist, longlat = TRUE, diagonal = FALSE)
  o_distt<-data.frame(o_distt)
  o_distt$Plots<-rownames(o_distt)
  
  # convert dist. matrix to data frame; remove repeats 
  out_o<-pivot_longer(o_distt, cols=c(1:10),names_to= "Plots_from",values_to = "Distance",
                      names_prefix="X")
  
  plotss_o<-cbind.data.frame(Plots=rownames(o_geo_dist), PlotID=o_geo_dist$PlotID) # 
  plotss_o$Plots<-as.character(plotss_o$Plots)
  
  out_oo<-left_join(out_o, plotss_o, by="Plots")
  out_oo<-select(out_oo, Plots_to=PlotID, Plots=Plots_from, Distance)
  out_oo<-left_join(out_oo, plotss_o, by="Plots")
  out_oo<-select(out_oo, Plots_to, Plots_from= PlotID, Distance)
  
  out_oo<-filter(out_oo, !Plots_to==Plots_from)
  out_ooo<- out_oo[!duplicated(t(apply(out_oo, 1, sort))),] # only unique 
  
  min_dist_o<-min(out_ooo$Distance)
  
  cat("progress", i, sep=' ','\n')
  
  if(min_dist_o<=0.1) next 
  if(oa_rangezz$r_Arid>oah_range$Prob50q_Elev_r) next
  if(oa_rangezz$totPlotArea<ka_plotarea_min | oa_rangezz$totPlotArea>ka_plotarea_max) next
  
  oa_out<-select(oa_rangez, geo_entity2, PlotID,Plot_Area )
  oa_out$Iteration<-i
  
  oa_dist_Scen3[[i]]<-cbind.data.frame(Iteration=i,geo_entity2=unique(oa_sel$geo_entity2),
                                       min_dist_km=min_dist_o,totPlotArea=oa_rangezz$totPlotArea,
                                       r_Arid=oa_rangezz$r_Arid,r_Elev=oa_rangezz$r_Elev)
  
  oa_sel_Scen3[[i]]<-rbind.data.frame(oa_out)
}

oa_dist_Scen3<-do.call(rbind.data.frame,oa_dist_Scen3)

oa_sel_Scen3<-do.call(rbind.data.frame,oa_sel_Scen3)

# QC: number of iterations per island, Aridity range & plot area

qc<- oa_sel_Scen3 %>% group_by(geo_entity2) %>% summarize(iters=length(unique(Iteration)),
                                                          plotn=length(unique(PlotID)))
qc2<- oa_dist_Scen3 %>% group_by(geo_entity2) %>% 
  summarize(iters=length(unique(Iteration)), min_dist=min(min_dist_km),
            min_Arid=min(r_Arid), max_Arid=max(r_Arid), mean_Aridr=mean(r_Arid),
            min_Elev=min(r_Elev),max_Elev=max(r_Elev), mean_Elevr=mean(r_Elev),
            maxPlotArea=max(totPlotArea), minPlotArea=min(totPlotArea))
# Hawaii

ha_dist_Scen3<-list(); ha_sel_Scen3<-list();

for(i in 1:7500){
  ha_sel<- haw %>% group_by(geo_entity2,PlotID) %>% sample_n_groups(10,replace=F)%>%
    ungroup()
  
  # range in Aridity & Elevation
  ha_rangez<-select(ha_sel, geo_entity2, PlotID, Lat_Dec, Long_Dec, AridInd, Elev_m,
                    Plot_Area) %>% distinct(.)
  
  ha_rangezz<-ha_rangez%>% summarize(min_Arid=min(AridInd),max_Arid=max(AridInd),
                                     min_Elev=min(Elev_m),max_Elev=max(Elev_m),
                                     totPlotArea=sum(Plot_Area))
  
  ha_rangezz$r_Arid<-ha_rangezz$max_Arid-ha_rangezz$min_Arid
  ha_rangezz$r_Elev<-ha_rangezz$max_Elev-ha_rangezz$min_Elev
  ha_rangezz<-select(ha_rangezz, r_Arid, r_Elev,totPlotArea)
  
  # min distance between Plots
  
  h_geo_dist<-select(ha_rangez, PlotID, Lat_Dec, Long_Dec)
  h_coords<-select(h_geo_dist, Long_Dec, Lat_Dec)
  h_data<-select(h_geo_dist, PlotID)
  crs<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  h_dist <- SpatialPointsDataFrame(coords = h_coords,
                                   data = h_data, 
                                   proj4string = crs)
  
  h_distt<-spDists(h_dist, longlat = TRUE, diagonal = FALSE)
  h_distt<-data.frame(h_distt)
  h_distt$Plots<-rownames(h_distt)
  
  # convert dist. matrix to data frame; remove repeats 
  out_h<-pivot_longer(h_distt, cols=c(1:10),names_to= "Plots_from",values_to = "Distance",
                      names_prefix="X")
  
  plotss_h<-cbind.data.frame(Plots=rownames(h_geo_dist), PlotID=h_geo_dist$PlotID) # 
  plotss_h$Plots<-as.character(plotss_h$Plots)
  
  out_hh<-left_join(out_h, plotss_h, by="Plots")
  out_hh<-select(out_hh, Plots_to=PlotID, Plots=Plots_from, Distance)
  out_hh<-left_join(out_hh, plotss_h, by="Plots")
  out_hh<-select(out_hh, Plots_to, Plots_from= PlotID, Distance)
  
  out_hh<-filter(out_hh, !Plots_to==Plots_from)
  out_hhh<- out_hh[!duplicated(t(apply(out_hh, 1, sort))),] # only unique 
  
  min_dist_h<-min(out_hhh$Distance)
  
  cat("progress", i, sep=' ','\n')
  
  if(min_dist_h<=0.1) next 
  if(ha_rangezz$r_Arid>haw_range$Prob50q_Elev_r) next
  if(ha_rangezz$totPlotArea<ka_plotarea_min | ha_rangezz$totPlotArea>ka_plotarea_max) next
  
  ha_out<-select(ha_rangez, geo_entity2, PlotID,Plot_Area )
  ha_out$Iteration<-i
  
  ha_dist_Scen3[[i]]<-cbind.data.frame(Iteration=i,geo_entity2=unique(ha_sel$geo_entity2),
                                       min_dist_km=min_dist_h,totPlotArea=ha_rangezz$totPlotArea,
                                       r_Arid=ha_rangezz$r_Arid,r_Elev=ha_rangezz$r_Elev)
  
  ha_sel_Scen3[[i]]<-rbind.data.frame(ha_out)
}

ha_dist_Scen3<-do.call(rbind.data.frame,ha_dist_Scen3)

ha_sel_Scen3<-do.call(rbind.data.frame,ha_sel_Scen3)

# QC: number of iterations per island, Aridity range & plot area

qc<- ha_sel_Scen3 %>% group_by(geo_entity2) %>% summarize(iters=length(unique(Iteration)),
                                                          plotn=length(unique(PlotID)))

qc2<- ha_dist_Scen3 %>% group_by(geo_entity2) %>%  
  summarize(iters=length(unique(Iteration)), min_dist=min(min_dist_km),
            min_Arid=min(r_Arid), max_Arid=max(r_Arid), mean_Aridr=mean(r_Arid),
            min_Elev=min(r_Elev),max_Elev=max(r_Elev), mean_Elevr=mean(r_Elev),
            maxPlotArea=max(totPlotArea), minPlotArea=min(totPlotArea))

# join all island samples together

ka_togg<-left_join(ka_sel_Scen3, ka_dist_Scen3, by=c("geo_entity2", "Iteration"))
ma_togg<-left_join(ma_sel_Scen3, ma_dist_Scen3, by=c("geo_entity2", "Iteration"))
oa_togg<-left_join(oa_sel_Scen3, oa_dist_Scen3, by=c("geo_entity2", "Iteration"))
ha_togg<-left_join(ha_sel_Scen3, ha_dist_Scen3, by=c("geo_entity2", "Iteration"))

islands_togg<-rbind.data.frame(ka_togg,ma_togg)
islands_togg<-rbind.data.frame(islands_togg, oa_togg)
islands_togg<-rbind.data.frame(islands_togg, ha_togg)

#
qc<- islands_togg %>% group_by(geo_entity2) %>% summarize(iters=length(unique(Iteration)),
                                                          plotn=length(unique(PlotID)))

# randomly select 100 iterations from each island

outt2<-list();

for(i in 1:4){
  
  test<-filter(islands_togg, geo_entity2 == unique(geo_entity2)[i])
  dd<- test %>% group_by(geo_entity2,Iteration) %>% sample_n_groups(100,replace=F)
  dd<-ungroup(dd)
  
  outt2[[i]]<-rbind.data.frame(dd)
}

outt2<-do.call(rbind.data.frame, outt2)

iters<-distinct(outt2, geo_entity2, Iteration)

iterss<-iters %>% group_by(geo_entity2) %>% mutate(Iteration2=row_number())

######################
# Step 3: Join data  #
######################

outt3<-left_join(outt2, iterss, by=c('geo_entity2','Iteration'))
outt3<-select(outt3, Iteration=Iteration2, geo_entity2, PlotID, Plot_Area,
              min_dist_km, r_Arid, r_Elev,-Iteration)

# quick qc

qcc<-outt3%>% group_by(geo_entity2, Iteration)%>% summarize(plotn=length(unique(PlotID)),
                                                            PlotArea=sum(Plot_Area),
                                                            Arid_r=mean(r_Arid),
                                                            Elev_r=mean(r_Elev))

qcc2<-qcc%>% group_by(geo_entity2)%>% summarize(iter=length(unique(Iteration)), 
                                                PlotArea2=mean(PlotArea),
                                                m_Arid_r=mean(Arid_r), m_Elev_r=mean(Elev_r),
                                                minPlotN=min(plotn),maxPlotN=max(plotn))

##########################

scen3_selplots<-rbind.data.frame(outt3)

write.csv(scen3_selplots, "Cleaned_Data/Scen3_Natives_SelPlots_ElevRange.csv",row.names=F)
