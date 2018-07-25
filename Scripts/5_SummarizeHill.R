# summarize hill N
require(rms)

## Data

HillN_2<-read.csv("Cleaned_Data/Scen2_Natives_7plots_HillN.csv")

HillN_3<-read.csv("Cleaned_Data/Scen3_Natives_7plots_HillN.csv")

# summarize

HillN_22<- HillN_2 %>%
  group_by(geo_entity2,order) %>%
  do(data.frame(rbind(smean.cl.boot(.$qD, B=1000))))

colnames(HillN_22)[3]<-"qD"
colnames(HillN_22)[4]<-"qD.LCL"
colnames(HillN_22)[5]<-"qD.UCL"

HillN_33<- HillN_3 %>%
  group_by(geo_entity2,order) %>%
  do(data.frame(rbind(smean.cl.boot(.$qD, B=1000))))

colnames(HillN_33)[3]<-"qD"
colnames(HillN_33)[4]<-"qD.LCL"
colnames(HillN_33)[5]<-"qD.UCL"


write.csv(HillN_22,"Cleaned_Data/HillN_Scen2_7plots.csv",row.names=F)
write.csv(HillN_33,"Cleaned_Data/HillN_Scen3_7plots.csv",row.names=F)
