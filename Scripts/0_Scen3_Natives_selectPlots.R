#####################
# Scenario 3 : ######
# 1. just natives ###
# 2. 7 plots per ####
#  island ###########
#####################

require(dplyr)
require(reshape2)


sample_n_groups = function(tbl, size, replace = FALSE, weight = NULL) {
  # regroup when done
  grps = tbl %>% groups %>% lapply(as.character) %>% unlist
  # check length of groups non-zero
  keep = tbl %>% summarise() %>% ungroup() %>% sample_n(size, replace, weight)
  # keep only selected groups, regroup because joins change count.
  # regrouping may be unnecessary but joins do something funky to grouping variable
  tbl %>% right_join(keep, by=grps) %>% group_by_(.dots = grps)
}

######################
# JUST NATIVE ########
# all trees> 2.54 ####
######################

######################
## data ##############
######################


datt<-read.csv("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Cleaned_Data/HawIslandsAbundance_2SizeClasses_100plus.csv",header=T)

datt<-filter(datt, Plot_Prop_Invaded<=0.75 & SizeClass=="all")  
datt$SizeClass<-droplevels(datt$SizeClass)

datt<-filter(datt, Native_Status_HawFlora_simple=="native") 

#####
#qc #
#####

length(unique(datt$PlotIDn)) # 420 plots
length(unique(datt$SPP_CODE3A)) #115 plots   
range(datt$Plot_Area) #  100.0037 1017.8760
quantile(datt$Plot_Area, probs=c(0.5)) # median = 1000

#########################
# quick data summary    #
#########################

summ<-unique(select(datt, geo_entity2, PlotIDn,Plot_Area))

summ2<-summarize(group_by(summ, geo_entity2), Plots=length(unique(PlotIDn)), PlotArea=sum(Plot_Area))

summ2

# 7 randomly drawn plots per island

kauai<-filter(datt,geo_entity2=="Kaua'i Island")
mn<-filter(datt,geo_entity2=="Maui Nui")
oh<-filter(datt,geo_entity2=="O'ahu Island")
big<-filter(datt,geo_entity2=="Hawai'i Island")

## Kaui ranges

k_rangez<-summarize(group_by(kauai,geo_entity2),min_MAT=min(MAT),max_MAT=max(MAT),min_MAP=min(MAP),max_MAP=max(MAP),min_PET=min(PET),max_PET=max(PET),
                    min_Elev=min(Elev_m),max_Elev=max(Elev_m), totPlotArea=sum(Plot_Area))

k_rangez$r_MAP<-k_rangez$max_MAP-k_rangez$min_MAP
k_rangez$r_MAT<-k_rangez$max_MAT-k_rangez$min_MAT
k_rangez$r_PET<-k_rangez$max_PET-k_rangez$min_PET
k_rangez$r_Elev<-k_rangez$max_Elev-k_rangez$min_Elev
k_rangez<-select(k_rangez,geo_entity2, r_MAP, r_MAT, r_PET, r_Elev,totPlotArea)

k_PET<-round(k_rangez$r_PET,digits=0)

###########

set.seed(27)

togg_out<-list();
for(i in 1:100000){
  
  a<- mn %>% group_by(geo_entity2,PlotIDn) %>% sample_n_groups(7,replace=F)
  b<- oh %>% group_by(geo_entity2,PlotIDn) %>% sample_n_groups(7,replace=F)
  c<- big %>% group_by(geo_entity2,PlotIDn) %>% sample_n_groups(7,replace=F)
  d<- kauai %>% group_by(geo_entity2,PlotIDn) %>% sample_n_groups(7,replace=F)
  
  togg<-rbind.data.frame(a,b,c,d)
  
  togg2<-ungroup(togg)
  
  rangez<-unique(select(togg2, geo_entity2, PlotIDn, PET, Plot_Area))
  rangezz<-summarize(group_by(rangez,geo_entity2),min_PET=min(PET),max_PET=max(PET), totPlotArea=sum(Plot_Area))
  
  rangezz$r_PET<-rangezz$max_PET-rangezz$min_PET
  rangezz<-select(rangezz,geo_entity2, r_PET, totPlotArea)
  
  rangezz$Keep<-ifelse(rangezz$r_PET<=200,1,0)
  
  Keep<-sum(rangezz$Keep)
  cat("progress", i, sep=' ','\n')
  if(Keep<4) next
  
  togg<-ungroup(togg)
  togg1<-select(togg, geo_entity2,PlotIDn, SPP_CODE3A,Abundance_ha)
  togg1<-merge(togg1, rangezz,by.y="geo_entity2")
  togg1$iteration<-i
  togg_out[[i]]<-rbind.data.frame(togg1)  
}  

tog_het<-do.call(rbind.data.frame,togg_out)



write.table(tog_het,"/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Cleaned_Data/Scen3_Natives_7plots_SimComms.csv",sep=",",row.names=F)


