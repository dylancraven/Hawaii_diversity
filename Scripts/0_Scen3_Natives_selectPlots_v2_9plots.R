#####################
# Scenario 3 : ######
# 1. just natives ###
# 2. 9 plots per ####
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

ka_plotarea<-read.csv("Cleaned_Data/Scen2_Natives_Kauai_PlotArea_9plots.csv",header=T)

datt<-read.csv("Cleaned_Data/HawIslandsAbundance_2SizeClasses_100plus.csv",header=T)

datt<-filter(datt, Plot_Prop_Invaded<=0.75 & SizeClass==5) %>%
  filter(., Native_Status=="native")

#####
#qc #
#####

length(unique(datt$PlotID)) # 429 plots
length(unique(datt$SPP_CODE3A)) #104 spp   
range(datt$Plot_Area) #  100.0037 1017.8760
quantile(datt$Plot_Area, probs=c(0.5)) # median = 1000

# Kaui PET ranges

kauai<-filter(datt,geo_entity2=="Kaua'i Island")

k_rangez<-summarize(group_by(kauai,geo_entity2),min_MAT=min(MAT),max_MAT=max(MAT),min_MAP=min(MAP),max_MAP=max(MAP),min_PET=min(PET),max_PET=max(PET),
                    meanPET=mean(PET),meanMAP=mean(MAP),
                    min_Elev=min(Elev_m),max_Elev=max(Elev_m), totPlotArea=sum(Plot_Area))

k_rangez$r_MAP<-k_rangez$max_MAP-k_rangez$min_MAP
k_rangez$r_MAT<-k_rangez$max_MAT-k_rangez$min_MAT
k_rangez$r_PET<-k_rangez$max_PET-k_rangez$min_PET
k_rangez$r_Elev<-k_rangez$max_Elev-k_rangez$min_Elev
k_rangez<-select(k_rangez,geo_entity2, meanPET, meanMAP, r_MAP, r_MAT, r_PET, r_Elev,totPlotArea)

k_PET<-round(k_rangez$r_PET,digits=0)

# kauia plot area ranges

ka_plotarea_min<-ka_plotarea$lower10
ka_plotarea_max<-ka_plotarea$upper10

############################################
# Step 1: select plots within PET range    #
############################################

set.seed(27)

togg_out<-list();
for(i in 1:4){
  for(j in 1:20000){
    
    datt3<-subset(datt, datt$geo_entity2==(unique(datt$geo_entity2))[i])  
    
    togg<- datt3 %>% group_by(geo_entity2,PlotID) %>% sample_n_groups(9,replace=F)
    togg<-ungroup(togg)
    
    rangez<-unique(select(togg, geo_entity2, PlotID, PET, MAP,Plot_Area))
    rangezz<-summarize(group_by(rangez,geo_entity2),min_PET=min(PET),max_PET=max(PET),meanPET=mean(PET),
                       meanMAP=mean(MAP),totPlotArea=sum(Plot_Area))
    
    rangezz$r_PET<-rangezz$max_PET-rangezz$min_PET
    rangezz<-select(rangezz,geo_entity2, r_PET, meanPET,meanMAP,totPlotArea)
    
    cat("island", as.character(unique(rangezz$geo_entity2)), sep=' ','\n')
    
    cat("progress", j, sep=' ','\n')
    
    if(rangezz$r_PET>=200) next 
    if(rangezz$totPlotArea<ka_plotarea_min | rangezz$totPlotArea>ka_plotarea_max) next
    
    togg<-ungroup(togg)
    togg1<-select(togg, geo_entity2,PlotID, SPP_CODE3A,Abundance_ha)
    togg1<-merge(togg1, rangezz,by.y="geo_entity2")
    togg1$iteration<-j
    togg_out[[j]]<-rbind.data.frame(togg1)}
}

tog_het<-do.call(rbind.data.frame,togg_out)

# QC: number of iterations per island, PET range & plot area

qc<-dplyr::summarize(group_by(tog_het, geo_entity2), iterations=length(unique(iteration)),
                     maxPET=max(r_PET), minPlotArea=min(totPlotArea),maxPlotArea=max(totPlotArea))

###########################################################
# Step 2: randomly select 100 iterations from each island #
###########################################################

tog_het2<-list();

for(i in 1:4){
  
  test<-subset(tog_het, tog_het$geo_entity2==(unique(tog_het$geo_entity2))[i])  
  
  d<- test %>% group_by(geo_entity2,iteration) %>% sample_n_groups(100,replace=F)
  d<-ungroup(d)
  
  tog_het2[[i]]<-rbind.data.frame(d)
}

tog_het2<-do.call(rbind.data.frame, tog_het2)

iters<-distinct(tog_het2, geo_entity2, iteration)

iterss<-iters %>% 
  group_by(geo_entity2) %>% mutate(Iteration2=row_number())

######################
# Step 3: Join data  #
######################

scen3_selplots<-left_join(tog_het2, iterss, by=c('geo_entity2','iteration'))
scen3_selplots$iteration<-NULL
colnames(scen3_selplots)[9]<-"Iteration"

write.table(scen3_selplots,"Cleaned_Data/Scen3_Natives_9plots_SimComms.csv",sep=",",row.names=F)