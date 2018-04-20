#####################
# Island - Scen. 3 ##
# 1. natives       ##
##2. 7 plots      ### 
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

dat3<-read.csv("Cleaned_Data/Scen3_Natives_7plots_SimComms.csv")
dat3$Abundance_ha<-round(dat3$Abundance_ha)

length(unique(dat3$iteration)) # 4940

######################
# select  100 ########
######################

set.seed(27)
un<-data.frame(unique(dat3$iteration))
nn<-dim(un)[1]
un$Iteration2<-seq(from=1,to=nn,by=1)
colnames(un)[1]<-"iteration"

un_sel<-data.frame(Iteration2=sample(un$Iteration2, 100, replace = FALSE))

un_sel<-merge(un_sel,un,by.y="Iteration2")

un_sel$Iteration2<-seq(from=1,to=100,by=1)

#un_sel<-filter(un, Iteration2<101)  # only 100

dat33<-merge(un_sel, dat3,by.y="iteration")

dat33<-select(dat33, Iteration2, geo_entity2, PlotID, SPP_CODE3A, Abundance_ha, r_PET,meanPET, meanMAP,totPlotArea)

############################
# quick data summary & QC # 
###########################

# is there variation in PET range and Plot Area?
summ2<-summarize(group_by(dat33,geo_entity2), r_PET=mean(r_PET),PlotArea=mean(totPlotArea), var_PlotArea=sd(totPlotArea), 
                 meanPET=mean(meanPET),meanMAP=mean(meanMAP))

summ2

# always 7 plots per island per iteration?

summ3<-summarize(group_by(dat33,Iteration2,geo_entity2),PlotN=length(unique(PlotID)))
summ33<-summarize(group_by(summ3,geo_entity2), meanPlotN=mean(PlotN), minP=min(PlotN),maxP=max(PlotN))

summ33
######################

set.seed(27)
curveZ<-list(); orderZ<-list();rangeZ<-list();alphaZ<-list();betaZ<-list(); radZ<-list();

for(i in 1:100){
  
  testt<-subset(dat33, dat33$Iteration2==(unique(dat33$Iteration2))[i])  
  
  togg2<-summarize(group_by(testt, geo_entity2, SPP_CODE3A),Abundance=sum(Abundance_ha))
  
  hcomm2<-dcast(togg2, SPP_CODE3A~geo_entity2, value.var="Abundance",sum)
  rownames(hcomm2)<-hcomm2$SPP_CODE3A
  hcomm2<-select(hcomm2,-SPP_CODE3A)
  
  #togg<-ungroup(togg)
  
  ########
  # SACs #
  ########
  
  h<-iNEXT(hcomm2,q=c(0),size=c(1,100,200,300,400,500,600,700,800,900,1000),datatype="abundance")
  curves<-do.call(rbind.data.frame,h$iNextEst)
  curves$geo_entity2<-rownames(curves)
  curves$geo_entity2 <- gsub("\\..*","",curves$geo_entity2)
  curves$iteration<-i   
  
  #########
  # Hill N#
  #########
  orders<-estimateD(hcomm2,datatype="abundance",base="size", level=1000,conf=0.95 )
  colnames(orders)[1]<-"geo_entity2"
  
  orders$iteration<-i

  ###########
  # RAD fun #
  ###########
  
  datt2<-dcast(testt, geo_entity2~SPP_CODE3A,value.var="Abundance_ha",sum)
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
  
  
  ###########
  # betaPIE #
  ###########
  
  h_comm3<-dcast(testt, PlotID~SPP_CODE3A, value.var="Abundance_ha",sum)
  rownames(h_comm3)<-h_comm3$PlotID
  h_comm3<-select(h_comm3,-PlotID)
  
  # group information
  
  h_attr<-unique(select(testt,PlotID, geo_entity2))
  
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
  
  h_help<-unique(select(testt, geo_entity2, PlotID))
  
  h_betapie<-merge(h_help,h_betapie,by.y="PlotID")
  
  h_betapie<-select(h_betapie, geo_entity2, PlotID, beta_S, beta_ENS_PIE)
  
  h_betapie$Iteration<-i
  
  
  #######
  # env #
  #######
  
  rangezz<-unique(select(testt, geo_entity2, r_PET, meanPET, meanMAP,totPlotArea))
  
  rangezz$Iteration<-i
  #rangezz<-ungroup(rangezz)
  
  
  # output
  cat("progress", i, sep=' ','\n')
  
  curveZ[[i]]<-rbind.data.frame(curves)
  
  orderZ[[i]]<-rbind.data.frame(orders)
  
  rangeZ[[i]]<-rbind.data.frame(rangezz)
  
  # alphaZ[[i]]<-rbind.data.frame(alpha_all)  
  
  betaZ[[i]]<-rbind.data.frame(h_betapie)
  
  radZ[[i]]<-rbind.data.frame(radd)
}

curves.tog<-do.call(rbind.data.frame,curveZ)

orders.tog<-do.call(rbind.data.frame,orderZ)

rangez.tog<-do.call(rbind.data.frame,rangeZ)

#alpha.tog<-do.call(rbind.data.frame,alphaZ)

beta.tog<-do.call(rbind.data.frame,betaZ)

rad.tog<-do.call(rbind.data.frame,radZ)

##################
# aggregate ######
# across samples #
##################


# All

write.table(orders.tog,"Cleaned_Data/Scen3_Natives_7plots_HillN.csv",sep=",",row.names=F)

write.table(curves.tog,"Cleaned_Data/Scen3_Natives_7plots_curves_estimates.csv",sep=",",row.names=F)

envs<-summarize(group_by(rangez.tog, geo_entity2), mean_MAP=mean(meanMAP),
                mean_PET=mean(meanPET),r_PET=mean(r_PET),  m_PlotArea=mean(totPlotArea))

write.table(envs,"Cleaned_Data/Scen3_Native_7plots_EnvConditions_summarized.csv",sep=",",row.names=F)

write.table(beta.tog,"Cleaned_Data/Scen3_Natives_7plots_BetaPIE.csv",sep=",",row.names=F)

write.table(rad.tog,"Cleaned_Data//Scen3_Natives_7plots_RAD.csv",sep=",",row.names=F)
