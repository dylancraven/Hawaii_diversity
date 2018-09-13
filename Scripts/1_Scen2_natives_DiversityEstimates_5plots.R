#########################
# Island - SAC         ##
# Scenario: 'Het + Age'##
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
## data ##############
######################

datt<-read.csv("Cleaned_Data/HawIslandsAbundance_2SizeClasses_100plus.csv",header=T)

datt<-filter(datt, Plot_Prop_Invaded<=0.75 & SizeClass==5) %>%
  filter(., Native_Status=="native")

#####
#qc #
#####

length(unique(datt$PlotID)) # 429 plots
length(unique(datt$SPP_CODE3A)) # 104 spp   
range(datt$Plot_Area) #  100.0037 1017.8760
quantile(datt$Plot_Area, probs=c(0.5)) # median = 1000


################################################
# Step 1.: calculate area ranges (max. & min)  #
# to ensure sampled plot area is similar across#
# islands  #####################################
################################################

# baseline is Kauai b/c it has the fewest plots

kauai<-filter(datt,geo_entity2=="Kaua'i Island")

set.seed(27)

outt<-list(); 

for(i in 1:100){
  
  d<- kauai %>% group_by(geo_entity2,PlotID) %>% sample_n_groups(5,replace=F)
  d<-ungroup(d)
  
  togg2<-select(d, PlotID, Plot_Area)%>%
    distinct(.)
  togg2<-ungroup(togg2)
  
  togg2$Iteration<-i
  outt[[i]]<-rbind.data.frame(togg2)
}

ka_sel_plots<-do.call(rbind.data.frame,outt)
ka_sel_plots$geo_entity2<-"Kaua'i Island"

ka_plotarea<-dplyr::summarize(group_by(ka_sel_plots, Iteration), Plot_Area=sum(Plot_Area))

ka_plotarea<-cbind.data.frame(Plot_Area=mean(ka_plotarea$Plot_Area), lower10=quantile(ka_plotarea$Plot_Area,probs=0.10),
                              upper10=quantile(ka_plotarea$Plot_Area,probs=0.90))
ka_plotarea$geo_entity2<-"Kaua'i Island"

ka_iteration_plots<-distinct(select(ka_sel_plots, Iteration, PlotID))
ka_iteration_plots$geo_entity2<-"Kaua'i Island"

######################################################################
# Step 2: select plots from other islands within area ranges of Kauai#
######################################################################

datt2<-filter(datt, geo_entity2!="Kaua'i Island")
datt2$geo_entity2<-droplevels(datt2$geo_entity2)

outt<-list();

for(i in 1:3)
{
  for(j in 1:1000)
  {
    
    datt3<-subset(datt2, datt2$geo_entity2==(unique(datt2$geo_entity2))[i])  
    
    d<- datt3 %>% group_by(geo_entity2,PlotID) %>% sample_n_groups(5,replace=F)
    d<-ungroup(d)
    
    togg<-select(d, geo_entity2, PlotID, Plot_Area)%>%
      distinct(.)
    togg<-ungroup(togg)
    
    plotarea<-sum(togg$Plot_Area)    
    
    cat("island", as.character(unique(togg$geo_entity2)), sep=' ','\n')
    cat("iteration", j, sep=' ','\n')
    
    if(plotarea<ka_plotarea$lower10|plotarea>ka_plotarea$upper10) next
    
    togg$Iteration<-j
    
    outt[[j]]<-rbind.data.frame(togg)
  }
}

outt<-do.call(rbind.data.frame, outt)

#### QC: how many iterations per island?

qc<-dplyr::summarize(group_by(outt, geo_entity2), iterations=length(unique(Iteration)))
                     
# randomly select 100 iterations from each island

outt2<-list();

for(i in 1:3){
  
  test<-subset(outt, outt$geo_entity2==(unique(outt$geo_entity2))[i])  
  
  d<- test %>% group_by(geo_entity2,Iteration) %>% sample_n_groups(100,replace=F)
  d<-ungroup(d)
  
  outt2[[i]]<-rbind.data.frame(d)
}

outt2<-do.call(rbind.data.frame, outt2)

iters<-distinct(outt2, geo_entity2, Iteration)

iterss<-iters %>% 
  group_by(geo_entity2) %>% mutate(Iteration2=row_number())

######################
# Step 3: Join data  #
######################

outt3<-left_join(outt2, iterss, by=c('geo_entity2','Iteration'))
outt3<-select(outt3, Iteration=Iteration2, geo_entity2, PlotID, Plot_Area,-Iteration)

scen2_selplots<-rbind.data.frame(outt3,ka_sel_plots)

# QC

qcc<-dplyr::summarize(group_by(scen2_selplots, geo_entity2, Iteration), plotn=length(unique(PlotID)))

qcc2<-dplyr::summarize(group_by(scen2_selplots, geo_entity2), iter=length(unique(Iteration)))


#####################################
# Step 4: estimate diversity stuff  #
#####################################

scen2_selplotss<-select(scen2_selplots, -Plot_Area)
scen2_selplotss$geo_entity2<-factor(scen2_selplotss$geo_entity2, levels=c("Hawai'i Island",
                                                                          "Kaua'i Island","Maui Nui","O'ahu Island"))

curveZ<-list(); orderZ<-list();rangeZ<-list();alphaZ<-list();betaZ<-list(); radZ<-list();

for(i in 1:100){
  
  togg<-filter(scen2_selplotss, Iteration==i)
  togg<-left_join(togg, datt, by=c("geo_entity2","PlotID"))
  
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

##################
# aggregate ######
# across samples #
##################

# All

write.table(orders.tog,"Cleaned_Data/Scen2_Natives_5plots_HillN.csv",sep=",",row.names=F)

write.table(curves.tog,"Cleaned_Data/Scen2_Natives_5plots_curves_estimates.csv",sep=",",row.names=F)

envs<-dplyr::summarize(group_by(rangez.tog, geo_entity2), mean_MAP=mean(mean_MAP),r_MAP=mean(r_MAP),
                       mean_MAT=mean(mean_MAT),r_MAT=mean(r_MAT),
                       mean_PET=mean(mean_PET),r_PET=mean(r_PET),r_Elev=mean(r_Elev),  m_PlotArea=mean(totPlotArea),
                       sd_PlotArea=sd(totPlotArea))

write.table(envs,"Cleaned_Data/Scen2_Natives_5plots_Envconditions_summarized.csv",sep=",",row.names=F)

write.table(beta.tog,"Cleaned_Data/Scen2_Natives_5plots_BetaPIE.csv",sep=",",row.names=F)

write.table(rad.tog,"Cleaned_Data/Scen2_Natives_5plots_RADs.csv",sep=",",row.names=F)

write.csv(ka_plotarea,"Cleaned_Data/Scen2_Natives_Kauai_PlotArea_5plots.csv",row.names=F)
