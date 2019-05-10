###########################
# Variance partitioning   #
# of beta diversity       #
# Scenario: 'Het + Age'   #
# Species: Native + Aliens#
###########################

require(dplyr)
require(tidyr)
require(tibble)
require(ecodist)
require(vegan)
require(sp)
require(FactoMineR)

# load data

dat<-read.csv("Cleaned_Data/HawIslandsAbundance_2SizeClasses_100plus.csv",header=T)

datt<- select(dat,PlotID, geo_entity2, Lat_Dec, Long_Dec, Elev_m, MAT, MAP, AridInd, 
              SubstrateAge_range)%>% distinct(.)

beta_div<-read.csv("Cleaned_Data/Scen2_Total_10plots_BetaPIE.csv",stringsAsFactors = FALSE)

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
  #bibb<-filter(bib, Iteration == unique(Iteration)[j])
  bibb<-filter(bib, index=="beta_S")
  bib_y<-data.frame(select(bibb, value))
  
  geo_dist<-select(bibb, PlotID, Lat_Dec, Long_Dec)%>%
    distinct(.)
  
  coords<-select(geo_dist, Long_Dec, Lat_Dec)
  data<-select(geo_dist, PlotID)
  crs<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  isl_dist <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string = crs)
  
  isl_dist<-spDists(isl_dist, longlat = TRUE, diagonal = FALSE)
  
  isl_pcnm <- pcnm(dist(isl_dist))
  isl_pcnm<-isl_pcnm$vectors
  isl_pcnm<-isl_pcnm[,1:2]
  # environmental distance (as PCA)
  
  env_dat<-select(bibb, PlotID, Elev_m,MAT, MAP, AridInd,SubstrateAge_range)%>%
    distinct(.)
  env_dat<-column_to_rownames(env_dat, var = "PlotID")
  
  env_pca <- PCA(env_dat,scale.unit=TRUE, ncp=5, graph = FALSE)
  env_pcaa<-data.frame(env_pca$ind$coord)
  env_pcs<-select(env_pcaa, PC_1=Dim.1,PC_2=Dim.2)
  
  # variance partitioning
  
  rda_mod<-varpart(bib_y, ~PC_1+PC_2 ,isl_pcnm, data=env_pcs,permutations = 1000) 
  
  # extract adj. r2 values
  
  adjr2_env<-rda_mod$part$fract[1,3] # a+b
  adjr2_space<-rda_mod$part$fract[2,3]#
  adjr2_env_space<-rda_mod$part$fract[3,3] #
  #resid<-1-rda_mod$part$fract[3,2]
  
  iterr<-unique(bibb)
  partz<-cbind.data.frame(geo_entity2=unique(bibb$geo_entity2), 
                          Iteration=unique(bibb$Iteration), 
                          adjr2_env=adjr2_env,adjr2_space,adjr2_env_space)
  
  
  cat("island", unique(bibb$geo_entity2), sep=' ','\n')
  cat("iteration", unique(bibb$Iteration), sep=' ','\n')
  
  out_sel[[i]]<-rbind.data.frame(partz)
}

out_sell<-do.call(rbind.data.frame, out_sel)
# out_sell$adjr2_env<-ifelse(out_sell$adjr2_env<0,0,out_sell$adjr2_env)
# out_sell$adjr2_space<-ifelse(out_sell$adjr2_space<0,0,out_sell$adjr2_space)
# out_sell$adjr2_env_space<-ifelse(out_sell$adjr2_env_space<0,0,out_sell$adjr2_env_space)

out_sell<-pivot_longer(out_sell, cols=c("adjr2_env","adjr2_space","adjr2_env_space"),
                        names_to="Partition", values_to = "AdjR2")

summ<-out_sell %>%
  group_by(geo_entity2, Partition) %>%
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$AdjR2))))

write.csv(summ, "Cleaned_Data/Scen2_total_VarPart_summ.csv",row.names=FALSE)
