#########################
# Island - SAC         ##
# Scenario: 'Age'      ##
#########################
# 5 plots               #
#########################

require(dplyr)
require(reshape2)
require(iNEXT)
require(vegan)
require(BBmisc)
require(ggplot2)
require(mobr)

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

ka_plotarea<-read.csv("Cleaned_Data/Scen2_Natives_Kauai_PlotArea_5plots.csv",header=T)

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
  for(j in 1:10000){
    
    datt3<-subset(datt, datt$geo_entity2==(unique(datt$geo_entity2))[i])  
    
    togg<- datt3 %>% group_by(geo_entity2,PlotID) %>% sample_n_groups(5,replace=F)
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

# QC: number of iterations per island, PET range & plot area

qcc<-dplyr::summarize(group_by(scen3_selplots, geo_entity2), iterations=length(unique(Iteration)),
                      maxPET=max(r_PET), minPlotArea=min(totPlotArea),maxPlotArea=max(totPlotArea))

write.table(scen3_selplots,"Cleaned_Data/Scen3_Natives_5plots_SimComms.csv",sep=",",row.names=F)

# scen3_selplots<-read.csv("Cleaned_Data/Scen3_Natives_5plots_SimComms.csv",header=T)

#####################################
# Step 4: estimate diversity stuff  #
#####################################

scen3_selplots$geo_entity2<-factor(scen3_selplots$geo_entity2, levels=c("Hawai'i Island",
                                                                        "Kaua'i Island","Maui Nui","O'ahu Island"))

scen3_selplotss<-select(scen3_selplots, -r_PET, -meanPET, -meanMAP, -totPlotArea)

datt2<-distinct(select(datt,geo_entity2, PlotID, MAT, MAP, PET, Elev_m,Plot_Area))

scen3_dat<-left_join(scen3_selplotss, datt2, by=c("geo_entity2","PlotID"))

#####################################

curveZ<-list(); orderZ<-list();rangeZ<-list();alphaZ<-list();betaZ<-list(); radZ<-list();

for(i in 1:100){
  
  togg<-filter(scen3_dat, Iteration==i)
  
  #####################
  # prepare for iNEXT #
  #####################
  
  togg2<-dplyr::summarize(group_by(togg, geo_entity2, SPP_CODE3A),Abundance=sum(Abundance_ha))
  
  hcomm2<-dcast(togg2, SPP_CODE3A~geo_entity2, value.var="Abundance",sum)
  rownames(hcomm2)<-hcomm2$SPP_CODE3A
  hcomm2<-select(hcomm2,-SPP_CODE3A)
  
  togg<-ungroup(togg)
  
  ##########
  # SACs   #
  ##########
  
  h<-iNEXT(hcomm2,q=c(0),size=c(1,100,200,300,400,500,600,700,800,900,1000),datatype="abundance")
  curves<-do.call(rbind.data.frame,h$iNextEst)
  curves$geo_entity2<-rownames(curves)
  curves$geo_entity2 <- gsub("\\..*","",curves$geo_entity2)
  curves$iteration<-i   
  
  ##########
  # Hill N##
  ##########
  
  orders<-estimateD(hcomm2,datatype="abundance",base="size", level=1000,conf=0.95 )
  colnames(orders)[1]<-"geo_entity2"
  
  orders$iteration<-i
  
  ########
  # RADs #
  ########
  
  datt2<-dcast(togg, geo_entity2~SPP_CODE3A,value.var="Abundance_ha",sum)
  rownames(datt2)<-datt2$geo_entity2
  nn<-dim(datt2)[2]
  datt3 <- decostand(datt2[,2:nn], method = "total")
  datt3$geo_entity2<-rownames(datt3)
  oo<-dim(datt3)[2]-1
  datt3<-melt(datt3,id.vars="geo_entity2",measure.vars=1:oo,variable.name="SPP_CODE3A",value.name="RelAbund")
  
  radHaw<-filter(datt3, geo_entity2=="Hawai'i Island")
  radHaw<-arrange(radHaw,RelAbund)
  radHaw<-filter(radHaw,RelAbund>0)
  n<-dim(radHaw)[1]
  radHaw$Rank<-seq(from=1,to=n,by=1)
  radHaw$Rank_N<-radHaw$Rank/n
  
  radKa<-filter(datt3, geo_entity2=="Kaua'i Island")
  radKa<-arrange(radKa,RelAbund)
  radKa<-filter(radKa,RelAbund>0)
  o<-dim(radKa)[1]
  radKa$Rank<-seq(from=1,to=o,by=1)
  radKa$Rank_N<-radKa$Rank/o
  
  radOah<-filter(datt3, geo_entity2=="O'ahu Island")
  radOah<-arrange(radOah,RelAbund)
  radOah<-filter(radOah,RelAbund>0)
  p<-dim(radOah)[1]
  radOah$Rank<-seq(from=1,to=p,by=1)
  radOah$Rank_N<-radOah$Rank/p
  radOah$geo_entity2<-"O'ahu Island"
  
  radMN<-filter(datt3, geo_entity2=="Maui Nui")
  radMN<-arrange(radMN,RelAbund)
  radMN<-filter(radMN,RelAbund>0)
  q<-dim(radMN)[1]
  radMN$Rank<-seq(from=1,to=q,by=1)
  radMN$Rank_N<-radMN$Rank/q
  
  radd<-rbind.data.frame(radHaw,radKa,radMN,radOah)
  
  radd$geo_entity2<-as.factor(radd$geo_entity2)
  radd$geo_entity2<-factor(radd$geo_entity2,levels=c("Hawai'i Island","Maui Nui","O'ahu Island","Kaua'i Island"))
  
  radd$iteration<-i
  
  ############
  # BetaPIE  #
  ############
  
  # community matrix
  
  h_comm3<-dcast(togg, PlotID~SPP_CODE3A, value.var="Abundance_ha",sum)
  rownames(h_comm3)<-h_comm3$PlotID
  h_comm3<-select(h_comm3,-PlotID)
  
  # group information
  
  h_attr<-unique(select(togg,PlotID, geo_entity2))
  
  h_attr<-arrange(h_attr,PlotID)
  
  h_attr<-data.frame(h_attr)
  rownames(h_attr)<-h_attr$PlotID
  h_attr<-select(h_attr,-PlotID)
  colnames(h_attr)<-"group"
  h_attr$group<-as.factor(h_attr$group)
  
  h_attr$x<-NA  
  h_attr$x<-as.numeric(h_attr$x)
  
  h_attr$y<-NA  
  h_attr$y<-as.numeric(h_attr$y)
  
  # make mob structure
  h_mob_in <- make_mob_in(h_comm3, h_attr)
  
  h_stats <- get_mob_stats(h_mob_in, group_var = "group", index = c("N", "S", "S_n", "S_PIE"),nperm=2)
  
  h_betapie<-filter(h_stats$samples_stats, index=="beta_S"|index=="beta_S_PIE") %>%
    select(., geo_entity2=group, index, value) 
  
  
  h_betapie$Iteration<-i
  
  ##############################
  # Environmental variables  ###
  ##############################
  
  rangez<-unique(select(togg, geo_entity2, PlotID, MAT, MAP, PET, Elev_m,Plot_Area))
  rangezz<-dplyr::summarize(group_by(rangez,geo_entity2),mean_MAT=mean(MAT),min_MAT=min(MAT),max_MAT=max(MAT),
                            mean_MAP=mean(MAP),min_MAP=min(MAP),max_MAP=max(MAP),
                            mean_PET=mean(PET),min_PET=min(PET),max_PET=max(PET),
                            min_Elev=min(Elev_m),max_Elev=max(Elev_m), totPlotArea=sum(Plot_Area))
  
  rangezz$r_MAP<-rangezz$max_MAP-rangezz$min_MAP
  rangezz$r_MAT<-rangezz$max_MAT-rangezz$min_MAT
  rangezz$r_PET<-rangezz$max_PET-rangezz$min_PET
  rangezz$r_Elev<-rangezz$max_Elev-rangezz$min_Elev
  rangezz<-select(rangezz,geo_entity2, mean_MAP,r_MAP, mean_MAT, r_MAT, mean_PET, r_PET, r_Elev,totPlotArea)
  
  rangezz$Iteration<-i
  rangezz<-ungroup(rangezz)
  
  
  # output
  cat("progress", i, sep=' ','\n')
  
  curveZ[[i]]<-rbind.data.frame(curves)
  
  orderZ[[i]]<-rbind.data.frame(orders)
  
  rangeZ[[i]]<-rbind.data.frame(rangezz)
  
  betaZ[[i]]<-rbind.data.frame(h_betapie)
  
  radZ[[i]]<-rbind.data.frame(radd)
}

curves.tog<-do.call(rbind.data.frame,curveZ)

orders.tog<-do.call(rbind.data.frame,orderZ)

rangez.tog<-do.call(rbind.data.frame,rangeZ)

rad.tog<-do.call(rbind.data.frame,radZ)

beta.tog<-do.call(rbind.data.frame,betaZ)

##############
# write out  #
##############

write.table(orders.tog,"Cleaned_Data/Scen3_Natives_5plots_HillN.csv",sep=",",row.names=F)

write.table(curves.tog,"Cleaned_Data/Scen3_Natives_5plots_curves_estimates.csv",sep=",",row.names=F)

envs<-dplyr::summarize(group_by(rangez.tog, geo_entity2), mean_MAP=mean(mean_MAP),
                       mean_PET=mean(mean_PET),r_PET=mean(r_PET),
                       m_PlotArea=mean(totPlotArea),
                       sd_PlotArea=sd(totPlotArea))

write.table(envs,"Cleaned_Data/Scen3_Native_5plots_EnvConditions_summarized.csv",sep=",",row.names=F)

write.table(beta.tog,"Cleaned_Data/Scen3_Natives_5plots_BetaPIE.csv",sep=",",row.names=F)

write.table(rad.tog,"Cleaned_Data//Scen3_Natives_5plots_RAD.csv",sep=",",row.names=F)