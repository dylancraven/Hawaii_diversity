#########################
# Island - SAC         ##
# Scenario: 'Het + Age'##
#########################
# just natives (10 plots)
#########################

require(dplyr)
require(tidyr)
require(tibble)
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
            PlotN=length(unique(PlotID)))

# min 19 plots (Kauai and Oahu)

# selected plots ( 10 plots per island, min dist. >0.1 km)

scen3_selplots<-read.csv("Cleaned_Data/Scen3_Natives_SelPlots.csv", header=TRUE)
scen3_selplots<-select(scen3_selplots,  -Plot_Area, -min_dist_km, -r_Arid, -r_Elev)

######################
# Step 3: Join data  #
######################

datt_big<-left_join(scen3_selplots,datt, by=c('geo_entity2','PlotID'))
datt_big$Native_Status<-droplevels(datt_big$Native_Status)
# quick qc

qcc<-datt_big%>% group_by(geo_entity2, Iteration)%>% summarize(plotn=length(unique(PlotID)))

qcc2<-qcc%>% group_by(geo_entity2)%>% summarize(iter=length(unique(Iteration)),
                                                meanPlotN=mean(plotn))

unique(datt_big$Native_Status)

#####################################
# Step 4: estimate diversity stuff  #
#####################################

datt_big$geo_entity2<-factor(datt_big$geo_entity2, levels=c("Hawai'i Island",
                                                            "Kaua'i Island","Maui Nui","O'ahu Island"))

curveZ<-list(); orderZ<-list();rangeZ<-list();alphaZ<-list();betaZ<-list(); radZ<-list();

for(i in 1:100){
  togg<-filter(datt_big, Iteration==i)
  togg$geo_entity2<-as.character(togg$geo_entity2)
  togg<-arrange(togg, geo_entity2, PlotID)
  
  #####################
  # prepare for iNEXT #
  #####################
  
  togg2<-dplyr::summarize(group_by(togg, geo_entity2, SPP_CODE3A),
                          Abundance=sum(Abundance_ha))
  
  hcomm2<-pivot_wider(togg2, id_cols=SPP_CODE3A, names_from=geo_entity2, 
                      values_from = Abundance, values_fill=list(Abundance=0))
  
  hcomm2<-column_to_rownames(hcomm2, var = "SPP_CODE3A")
  
  togg<-ungroup(togg)
  
  ##########
  # SACs   #
  ##########
  
  h<-iNEXT(hcomm2,q=c(0),size=seq(from=1, to=10000, by=10),datatype="abundance")
  
  curves<-do.call(rbind.data.frame,h$iNextEst)
  curves$geo_entity2<-rownames(curves)
  curves$geo_entity2 <- gsub("\\..*","",curves$geo_entity2)
  curves$iteration<-i   
  
  ##########
  # Hill N##
  ##########
  
  y<-iNEXT(hcomm2,q=c(0,1,2),size=c(1000,10000),datatype="abundance")
  
  orders<-do.call(rbind.data.frame, y$iNextEst)
  orders$geo_entity2<-rownames(orders)
  orders$geo_entity2 <- gsub("\\..*","",orders$geo_entity2)
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
  
  toggX<-togg%>% unite("Ent_Plot", c("geo_entity2","PlotID"), sep="_", remove=FALSE)
  
  h_comm3<-pivot_wider(toggX, id_cols=Ent_Plot, names_from=SPP_CODE3A, 
                       values_from = Abundance_ha, values_fill=list(Abundance_ha=0))
  
  h_comm3<-column_to_rownames(h_comm3, var = "Ent_Plot")
  
  # group information
  rows<-data.frame(Ent_Plot=rownames(h_comm3))
  
  h_attr<-rows %>% separate("Ent_Plot",c("geo_entity2","PlotID"),sep="_",remove=TRUE)%>%
    select(., PlotID, geo_entity2)
  
  rownames(h_attr)<-h_attr$PlotID
  h_attr<-select(h_attr,-PlotID)
  colnames(h_attr)<-"group"
  h_attr$group<-as.factor(h_attr$group)
  
  h_attr$x<-NA  
  h_attr$x<-as.numeric(h_attr$x)
  
  h_attr$y<-NA  
  h_attr$y<-as.numeric(h_attr$y)
  
  rownames(h_comm3)<-rownames(h_attr)
  
  # make mob structure
  h_mob_in <- make_mob_in(h_comm3, h_attr)
  
  h_stats <- get_mob_stats(h_mob_in, group_var = "group", index = c("N", "S", "S_n", "S_PIE"),nperm=2)
  
  h_betapie<-filter(h_stats$samples_stats, index=="beta_S"|index=="beta_S_PIE") %>%
    select(., geo_entity2=group, index, value) 
  
  h_betapie$PlotID<-rep(rownames(h_attr),times=2)
  
  h_betapie$Iteration<-i
  
  ##############################
  # Environmental variables  ###
  ##############################
  
  rangez<-unique(select(togg, geo_entity2, PlotID, MAT, MAP, AridInd, PET, Elev_m,SubstrateAge_range, Plot_Area))
  rangezz<-dplyr::summarize(group_by(rangez,geo_entity2),mean_MAT=mean(MAT),min_MAT=min(MAT),max_MAT=max(MAT),
                            mean_MAP=mean(MAP),min_MAP=min(MAP),max_MAP=max(MAP),
                            mean_PET=mean(PET),min_PET=min(PET),max_PET=max(PET),
                            mean_Arid=mean(AridInd),min_Arid=min(AridInd),max_Arid=max(AridInd),
                            min_Elev=min(Elev_m),max_Elev=max(Elev_m),
                            mean_SubstrateAge=mean(SubstrateAge_range, na.rm=TRUE), min_SubstrateAge=min(SubstrateAge_range, na.rm=TRUE),
                            max_SubstrateAge=max(SubstrateAge_range, na.rm=TRUE),totPlotArea=sum(Plot_Area))
  rangezz$r_MAP<-rangezz$max_MAP-rangezz$min_MAP
  rangezz$r_MAT<-rangezz$max_MAT-rangezz$min_MAT
  rangezz$r_PET<-rangezz$max_PET-rangezz$min_PET
  rangezz$r_Arid<-rangezz$max_Arid-rangezz$min_Arid
  rangezz$r_Elev<-rangezz$max_Elev-rangezz$min_Elev
  rangezz$r_SubstrateAge<-rangezz$max_SubstrateAge-rangezz$min_SubstrateAge
  
  rangezz<-select(rangezz,geo_entity2, mean_MAP,r_MAP, mean_MAT, r_MAT, mean_PET, r_PET,mean_Arid, r_Arid, 
                  min_Elev, max_Elev, r_Elev, min_SubstrateAge, max_SubstrateAge, r_SubstrateAge, mean_SubstrateAge, totPlotArea)
  
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

write.table(orders.tog,"Cleaned_Data/Scen3_Natives_10plots_HillN.csv",sep=",",row.names=F)

write.table(curves.tog,"Cleaned_Data/Scen3_Natives_10plots_curves_estimates.csv",sep=",",
            row.names=F)

envs<-dplyr::summarize(group_by(rangez.tog, geo_entity2), mean_MAP=mean(mean_MAP),
                       r_MAP=mean(r_MAP),mean_MAT=mean(mean_MAT),r_MAT=mean(r_MAT),
                       mean_PET=mean(mean_PET),r_PET=mean(r_PET),
                       mean_Arid=mean(mean_Arid),r_Arid=mean(r_Arid),
                       mean_minElev=mean(min_Elev),mean_maxElev=mean(max_Elev),
                       r_Elev=mean(r_Elev),mean_SubstrateAge=mean(mean_SubstrateAge),
                       mean_minSubstrateAge=mean(min_SubstrateAge),
                       mean_maxSubstrateAge=mean(max_SubstrateAge),
                       r_SubstrateAge=mean(r_SubstrateAge),
                       m_PlotArea=mean(totPlotArea),sd_PlotArea=sd(totPlotArea))

write.table(envs,"Cleaned_Data/Scen3_Natives_10plots_Envconditions_summarized.csv",
            sep=",",row.names=F)

write.table(beta.tog,"Cleaned_Data/Scen3_Natives_10plots_BetaPIE.csv",sep=",",
            row.names=F)

write.table(rad.tog,"Cleaned_Data/Scen3_Natives_10plots_RADs.csv",sep=",",row.names=F)