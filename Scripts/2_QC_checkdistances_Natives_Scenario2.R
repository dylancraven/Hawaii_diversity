###########################
# distances between plots #
###########################

require(dplyr)
require(tidyr)
require(tibble)
require(ecodist)
require(vegan)
require(sp)
#require(FactoMineR)

sample_n_groups = function(tbl, size, replace = FALSE, weight = NULL) {
  # regroup when done
  grps = tbl %>% groups %>% lapply(as.character) %>% unlist
  # check length of groups non-zero
  keep = tbl %>% summarise() %>% ungroup() %>% sample_n(size, replace, weight)
  # keep only selected groups, regroup because joins change count.
  # regrouping may be unnecessary but joins do something funky to grouping variable
  tbl %>% right_join(keep, by=grps) %>% group_by(.dots = grps)
}

########
# data #
########

sel_p<-read.csv("Cleaned_Data/Scen2_Natives_SelPlots.csv",header = T, 
              stringsAsFactors = FALSE) 

sel_p<-select(sel_p, -Plot_Area)

plot_i<-read.csv("Cleaned_Data/HawIslandsAbundance_2SizeClasses_100plus.csv",header=T,
                 stringsAsFactors = FALSE)

plot_i<-select(plot_i,geo_entity2,PlotID, Lat_Dec, Long_Dec,MAT, MAP, AridInd, SubstrateAge_range)%>%
               distinct(.) %>% ungroup()

sel_pp<-left_join(sel_p, plot_i, by=c("geo_entity2","PlotID"))

# calculate geographical distances between all sites (each iteration)

dist_out<-list(); 

for(i in 1:100){
  
  test<-filter(sel_pp, Iteration == unique(Iteration)[i])

  geo_dist<-select(test, PlotID, Lat_Dec, Long_Dec)%>% distinct(.)

  coords<-select(geo_dist, Long_Dec, Lat_Dec)
  data<-select(geo_dist, PlotID)
  crs<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

  haw_dist <- SpatialPointsDataFrame(coords = coords,
                                   data = data, 
                                   proj4string = crs)

  outt<-spDists(haw_dist, longlat = TRUE, diagonal = FALSE)
  outt<-data.frame(outt)
  outt$Plots<-rownames(outt)
  
  # convert dist. matrix to data frame; remove repeats 
  out_l<-pivot_longer(outt, cols=c(1:40),names_to= "Plots_from",values_to = "Distance",
                      names_prefix="X")
  
  plotss<-cbind.data.frame(Plots=rownames(test), PlotID=test$PlotID) # 
  plotss$Plots<-as.character(plotss$Plots)
  
  out_ll<-left_join(out_l, plotss, by="Plots")
  out_ll<-select(out_ll, Plots_to=PlotID, Plots=Plots_from, Distance)
  out_ll<-left_join(out_ll, plotss, by="Plots")
  out_ll<-select(out_ll, Plots_to, Plots_from= PlotID, Distance)
  
  out_lll<-filter(out_ll, !Plots_to==Plots_from)
  
  out_put<- out_lll[!duplicated(t(apply(out_lll, 1, sort))),] # only unique 
  
  isls<-select(test, Plots_to=PlotID, geo_entity2) %>% distinct(.)
  out_putt<-left_join(out_put, isls, by="Plots_to")
  out_putt<-select(out_putt, Plots_to, Islands_to=geo_entity2, Plots_from, Distance)
  
  isls<-select(test, Plots_from=PlotID, geo_entity2) %>% distinct(.)
  out_putt<-left_join(out_putt, isls, by="Plots_from")
  out_putt<-select(out_putt, Plots_to, Islands_to,Plots_from, Islands_from =geo_entity2, 
                   Distance)

  out_puttt<-filter(out_putt, Islands_to==Islands_from)  
  out_puttt$Iteration<-unique(test$Iteration)
  out_puttt<-ungroup(out_puttt)
  
  cat("Iteration",i, sep=' ','\n')
  dist_out[[i]]<-rbind.data.frame(out_puttt)
  }
  
dist_out<-do.call(rbind.data.frame, dist_out)
dist_out$geo_entity2<-dist_out$Islands_to
dist_out<-select(dist_out, Iteration, geo_entity2, Plots_to, Plots_from, Distance)

# summarize 
summ<-dist_out %>% group_by(Iteration, geo_entity2)%>% summarize(meanDist=mean(Distance),
                                                sdDist=sd(Distance),maxDist=max(Distance),
                                                              minDist=min(Distance))

summ2<-summ %>% group_by(geo_entity2)%>% summarize(meanDist=mean(meanDist),
                                                   sdDist=mean(sdDist),
                                                maxDist=max(maxDist),minDist=min(minDist))

# select iterations with min and max distances

iteration_sel<-filter(summ, minDist>1 )

summ2<-iteration_sel %>% group_by(geo_entity2)%>% summarize(meanDist=mean(meanDist),
                                                   sdDist=mean(sdDist),
                                                   maxDist=mean(maxDist),minDist=mean(minDist),
                                                   iters=length(unique(Iteration)))
