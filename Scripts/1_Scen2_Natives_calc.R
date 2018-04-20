#####################
# Island - SAC ######
#####################
# Scenario 2 ########
#####################

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
## data ##############
######################

datt<-read.csv("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Cleaned_Data/HawIslandsAbundance_2SizeClasses_100plus.csv",header=T)

datt<-filter(datt, Plot_Prop_Invaded<=0.75 & SizeClass==5) %>%
  filter(., Native_Status=="native")

#####
#qc #
#####

length(unique(datt$PlotID)) # 429 plots
length(unique(datt$SPP_CODE3A)) # 104 spp   
range(datt$Plot_Area) #  100.0037 1017.8760
quantile(datt$Plot_Area, probs=c(0.5)) # median = 1000

##########################
# prepare data for iNExt #
##########################

datt2<-summarize(group_by(datt, geo_entity2, SPP_CODE3A),Abundance=sum(Abundance_ha))

#########################
# quick data summary    #
#########################

summ<-unique(select(datt, geo_entity2, PlotID,Plot_Area))

summ2<-summarize(group_by(summ, geo_entity2), Plots=length(unique(PlotID)), PlotArea=sum(Plot_Area))

summ2

####################
# separate islands #
####################

kauai<-filter(datt,geo_entity2=="Kaua'i Island")
mn<-filter(datt,geo_entity2=="Maui Nui")
oh<-filter(datt,geo_entity2=="O'ahu Island")
big<-filter(datt,geo_entity2=="Hawai'i Island")

###########

set.seed(27)
curveZ<-list(); orderZ<-list();rangeZ<-list();alphaZ<-list();betaZ<-list(); radZ<-list();

for(i in 1:100){
  
  a<- mn %>% group_by(geo_entity2,PlotID) %>% sample_n_groups(7,replace=F)
  b<- oh %>% group_by(geo_entity2,PlotID) %>% sample_n_groups(7,replace=F)
  c<- big %>% group_by(geo_entity2,PlotID) %>% sample_n_groups(7,replace=F)
  d<- kauai %>% group_by(geo_entity2,PlotID) %>% sample_n_groups(7,replace=F)
  
  togg<-rbind.data.frame(a,b,c,d)
  
  #####################
  # prepare for iNEXT #
  #####################
  
  togg2<-summarize(group_by(togg, geo_entity2, SPP_CODE3A),Abundance=sum(Abundance_ha))
  
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
  
  # make mob structure
  h_mob_in <- make_mob_in(h_comm3, h_attr)
  
  h_stats <- get_mob_stats(h_mob_in, group_var = "group",nperm=10)
  
  h_betapie<-data.frame(h_stats$samples$beta_ENS_PIE, h_stats$samples$beta_S)
  h_betapie$PlotID<-rownames(h_betapie)
  colnames(h_betapie)[1]<-"beta_ENS_PIE"
  colnames(h_betapie)[2]<-"beta_S"
  
  h_help<-unique(select(togg, geo_entity2, PlotID))
  
  h_betapie<-merge(h_help,h_betapie,by.y="PlotID")
  
  h_betapie<-select(h_betapie, geo_entity2, PlotID, beta_S, beta_ENS_PIE)
  
  h_betapie$Iteration<-i
  
  ##############################
  # Environmental variables  ###
  ##############################
  
  rangez<-unique(select(togg, geo_entity2, PlotID, MAT, MAP, PET, Elev_m,Plot_Area))
  rangezz<-summarize(group_by(rangez,geo_entity2),mean_MAT=mean(MAT),min_MAT=min(MAT),max_MAT=max(MAT),
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

##################
# aggregate ######
# across samples #
##################

# All

write.table(orders.tog,"Cleaned_Data/Scen2_Natives_7plots_HillN.csv",sep=",",row.names=F)

write.table(curves.tog,"Cleaned_Data/Scen2_Natives_7plots_curves_estimates.csv",sep=",",row.names=F)

envs<-summarize(group_by(rangez.tog, geo_entity2), mean_MAP=mean(mean_MAP),r_MAP=mean(r_MAP),
                mean_MAT=mean(mean_MAT),r_MAT=mean(r_MAT),
                mean_PET=mean(mean_PET),r_PET=mean(r_PET),r_Elev=mean(r_Elev),  m_PlotArea=mean(totPlotArea))


write.table(envs,"Cleaned_Data/Scen2_Natives_7plots_Envconditions_summarized.csv",sep=",",row.names=F)

write.table(beta.tog,"Cleaned_Data/Scen2_Natives_7plots_BetaPIE.csv",sep=",",row.names=F)

write.table(rad.tog,"Cleaned_Data/Scen2_Natives_7plots_RADs.csv",sep=",",row.names=F)

