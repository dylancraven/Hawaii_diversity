################
# rarefaction ##
# all data #####
# for diff. Ns #
################

require(dplyr)
require(reshape2)
require(vegan)
require(stringr)

######################
## data ##############
######################

datt<-read.csv("Cleaned_Data/HawIslandsAbundance_2SizeClasses_100plus.csv",header=T)

datt<-filter(datt, Plot_Prop_Invaded<=0.75 & SizeClass=="all")  
datt$SizeClass<-droplevels(datt$SizeClass)

datt<-filter(datt, Native_Status_HawFlora_simple=="native") 

#####
#qc #
#####

length(unique(datt$PlotIDn)) # 421 plots
length(unique(datt$SPP_CODE3A)) #113 species   
range(datt$Plot_Area) #  100.0037 1017.8760
quantile(datt$Plot_Area, probs=c(0.5)) # median = 1000

#############################
# create site x spp matrix  #
#############################

datt2<-summarize(group_by(datt, geo_entity2, SPP_CODE3A),Abundance=sum(Abundance_ha))

datt2_mat<-dcast(datt2, geo_entity2~SPP_CODE3A, value.var="Abundance",sum)
rownames(datt2_mat)<-datt2_mat$geo_entity2
datt2_mat<-select(datt2_mat,-geo_entity2)

datt2_mat<-as.matrix(datt2_mat)

##################
# Rarefy this ! ##
##################

spp_r<-data.frame(rarefy(datt2_mat,sample=c(100,1000,10000),se=TRUE))
spp_r$Location_metric<-rownames(spp_r)
spp_r$Location<-word(spp_r$Location_metric,1,sep = "\\.")
spp_r$Metric<-word(spp_r$Location_metric,-1,sep = "\\.")

spp_rr<-melt(spp_r, id.vars=c("Location","Metric"),measure.vars = 1:3, variable.name="Rarefy",value.name="Depends")

spp_rrr<-dcast(spp_rr, Location+Rarefy~Metric, value.var="Depends",sum)
spp_rrr$Location<-str_replace_all(spp_rrr$Location,"\\(incl","")
spp_rrr$Location<-str_trim(spp_rrr$Location,side="both")


spp_N<-data.frame(rowSums(datt2_mat > 0))
colnames(spp_N)[1]<-"S"
spp_N$Location<-rownames(spp_N)
spp_N$Location<-str_replace_all(spp_N$Location,"\\(incl. Mokoli'i Islet\\)","")
spp_N$Location<-str_trim(spp_N$Location,side="both")
spp_N$se<-0
spp_N$Rarefy<-"all"

SppR<-rbind.data.frame(spp_rrr,spp_N)
SppR<-arrange(SppR, Location)

write.table(SppR,"Cleaned_Data/RarefySppRichness_alldata.csv",sep=",",row.names=F)
