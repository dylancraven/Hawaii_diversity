#############################
# Island - SAC             ##
# Plot selection           ##
# Scenario: 'Het + Age'    ##
#############################
# natives & alien (10 plots)#
#############################

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

datt<-filter(dat, SizeClass==5) %>%
  filter(., Native_Status=="alien" |Native_Status=="native")

#####
#qc #
#####

length(unique(datt$PlotID)) # 517 plots
length(unique(datt$SPP_CODE3A)) # 166 spp   
range(datt$Plot_Area) #  100.0037 1017.88
quantile(datt$Plot_Area, probs=c(0.5)) # median = 1000

datt%>%
  group_by(geo_entity2)%>%
  summarise(PlotArea=mean(Plot_Area), PlotArea_sd=sd(Plot_Area), 
            PlotN=length(unique(PlotID)))

################################################
# Step 1.: calculate area ranges (max. & min)  #
#         & min. distance among plots > 1 km   #
################################################

# baseline is Kauai b/c it has the fewest plots

kauai<-filter(datt,geo_entity2=="Kaua'i Island")

set.seed(27)

ka_sel_plots<-list(); ka_distt<-list();

for(i in 1:100){
  
  d<- kauai %>%
    group_by(geo_entity2,PlotID) %>% sample_n_groups(10,replace=F) %>% ungroup()
  
  togg2<-select(d, PlotID, Plot_Area)%>%
    distinct(.) %>% ungroup()
  
  plots<-togg2$PlotID
  
  # calculate distance between plots
  dd<-filter(d, PlotID %in% plots)
  
  geo_dist<-select(dd, PlotID, Lat_Dec, Long_Dec)%>% distinct(.)
  
  coords<-select(geo_dist, Long_Dec, Lat_Dec)
  data<-select(geo_dist, PlotID)
  crs<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  ka_dist <- SpatialPointsDataFrame(coords = coords,
                                    data = data, 
                                    proj4string = crs)
  
  outt<-spDists(ka_dist, longlat = TRUE, diagonal = FALSE)
  outt<-data.frame(outt)
  outt$Plots<-rownames(outt)
  
  # convert dist. matrix to data frame; remove repeats 
  out_l<-pivot_longer(outt, cols=c(1:10),names_to= "Plots_from",values_to = "Distance",
                      names_prefix="X")
  
  plotss<-cbind.data.frame(Plots=rownames(geo_dist), PlotID=geo_dist$PlotID) # 
  plotss$Plots<-as.character(plotss$Plots)
  
  out_ll<-left_join(out_l, plotss, by="Plots")
  out_ll<-select(out_ll, Plots_to=PlotID, Plots=Plots_from, Distance)
  out_ll<-left_join(out_ll, plotss, by="Plots")
  out_ll<-select(out_ll, Plots_to, Plots_from= PlotID, Distance)
  
  out_lll<-filter(out_ll, !Plots_to==Plots_from)
  
  out_put<- out_lll[!duplicated(t(apply(out_lll, 1, sort))),] # only unique 
  
  min_dist<-min(out_put$Distance)
  cat("Iteration",i, sep=' ','\n')
  if(min_dist<=0.1) next
  
  togg2$Iteration<-i
  
  ka_distt[[i]]<-cbind.data.frame(min_dist_km=min_dist, Iteration=i)
  ka_sel_plots[[i]]<-rbind.data.frame(togg2)
}

ka_sel_plots<-do.call(rbind.data.frame,ka_sel_plots)
ka_sel_plots$geo_entity2<-"Kaua'i Island"

ka_distt<-do.call(rbind.data.frame, ka_distt)

ka_plotarea<-ka_sel_plots %>% group_by(Iteration) %>% summarise(Plot_Area=sum(Plot_Area))

ka_plotarea<- left_join(ka_plotarea, ka_distt, by="Iteration")

ka_plotarea<-cbind.data.frame(Plot_Area=mean(ka_plotarea$Plot_Area), 
                              Area_lower10=quantile(ka_plotarea$Plot_Area,probs=0.10),
                              Area_upper10=quantile(ka_plotarea$Plot_Area,probs=0.90),
                              Min_dist=mean(ka_plotarea$min_dist_km))

ka_plotarea$geo_entity2<-"Kaua'i Island"

######################################################################
# Step 2: select plots from other islands within area ranges of Kauai#
######################################################################

datt2<-filter(datt, geo_entity2!="Kaua'i Island")
datt2$geo_entity2<-droplevels(datt2$geo_entity2)

out_sel<-list(); isl_distt<-list();

for(i in 1:3)
{
  for(j in 1:5000)
  {
    
    datt3<-filter(datt2, geo_entity2 == unique(geo_entity2)[i])
    
    d<- datt3 %>% group_by(geo_entity2,PlotID) %>% sample_n_groups(10,replace=F) %>%
      ungroup()
    
    # total plot area per island, per iteration
    togg<-select(d, geo_entity2, PlotID, Plot_Area)%>%
      distinct(.) %>% ungroup()
    
    plotarea<-sum(togg$Plot_Area)    
    
    cat("island", as.character(unique(togg$geo_entity2)), sep=' ','\n')
    cat("iteration", j, sep=' ','\n')
    
    # min. distance between plots per island, per iteration
    
    geo_dist<-select(d, PlotID, Lat_Dec, Long_Dec)%>% distinct(.)
    coords<-select(geo_dist, Long_Dec, Lat_Dec)
    data<-select(geo_dist, PlotID)
    crs<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    
    isl_dist <- SpatialPointsDataFrame(coords = coords,
                                       data = data, 
                                       proj4string = crs)
    
    outt<-spDists(isl_dist, longlat = TRUE, diagonal = FALSE)
    outt<-data.frame(outt)
    outt$Plots<-rownames(outt)
    
    # convert dist. matrix to data frame; remove repeats 
    out_l<-pivot_longer(outt, cols=c(1:10),names_to= "Plots_from",values_to = "Distance",
                        names_prefix="X")
    
    plotss<-cbind.data.frame(Plots=rownames(geo_dist), PlotID=geo_dist$PlotID) # 
    plotss$Plots<-as.character(plotss$Plots)
    
    out_ll<-left_join(out_l, plotss, by="Plots")
    out_ll<-select(out_ll, Plots_to=PlotID, Plots=Plots_from, Distance)
    out_ll<-left_join(out_ll, plotss, by="Plots")
    out_ll<-select(out_ll, Plots_to, Plots_from= PlotID, Distance)
    
    out_lll<-filter(out_ll, !Plots_to==Plots_from)
    
    out_put<- out_lll[!duplicated(t(apply(out_lll, 1, sort))),] # only unique 
    
    min_dist<-min(out_put$Distance)
    
    #
    if(plotarea<ka_plotarea$Area_lower10|plotarea>ka_plotarea$Area_upper10) next
    if(min_dist<=0.1) next
    
    togg$Iteration<-j
    
    isl_distt[[j]]<-cbind.data.frame(min_dist_km=min_dist, geo_entity2=unique(d$geo_entity2),Iteration=j)
    out_sel[[j]]<-rbind.data.frame(togg)
  }
}

out_sel<-do.call(rbind.data.frame, out_sel)
isl_distt<-do.call(rbind.data.frame, isl_distt)

sel_tog<-left_join(out_sel, isl_distt, by=c("geo_entity2","Iteration"))

# quick qc

qc<-sel_tog%>% group_by(geo_entity2 , Iteration) %>% 
  summarize(plotn=length(unique(PlotID)),PlotArea=sum(Plot_Area), 
            MinDist=unique(min_dist_km))

qc2<-qc %>% group_by(geo_entity2)%>% summarize(iter=length(unique(Iteration)), 
                                               PlotAreaa=mean(PlotArea), 
                                               minPlotN=min(plotn),maxPlotN=max(plotn),
                                               MinDist=mean(MinDist))

# randomly select 100 iterations from each island

outt2<-list();

for(i in 1:3){
  
  test<-filter(sel_tog, geo_entity2 == unique(geo_entity2)[i])
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
outt3<-select(outt3, Iteration=Iteration2, geo_entity2, PlotID, Plot_Area,-Iteration)

scen2_selplots<-rbind.data.frame(outt3,ka_sel_plots)

# quick qc

qcc<-scen2_selplots%>% group_by(geo_entity2, Iteration)%>% summarize(plotn=length(unique(PlotID)),
                                                                     PlotArea=sum(Plot_Area))

qcc2<-qcc%>% group_by(geo_entity2)%>% summarize(iter=length(unique(Iteration)),
                                                PlotArea2=mean(PlotArea))

#

write.csv(scen2_selplots, "Cleaned_Data/Scen2_Total_SelPlots.csv",row.names=F)

