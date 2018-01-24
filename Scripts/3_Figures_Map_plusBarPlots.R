#####################
# Big figure ########
# Plots +
# macroecology +  
# rarefied diversity 
####################

require(sp)
require(rgdal)
require(ggplot2)
require(ggsn) # for north symbols and scale bars
require(dplyr)
require(reshape2)
require(ggsci)
require(cowplot)

# read point data
# file.path() should be platform independent

HW.dt1<-read.table(file=file.path("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Cleaned_Data", "Hawaii_mapdata.csv"),sep=",",header=TRUE, row.names=NULL)


# load Hawaii shapefile
# This shapefile was cropped in ArcMap from the geoentities.RData from Patrick Weigelt
HW_shp <- rgdal::readOGR(dsn   = file.path("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/UFZ_IslandFun/Island_plant_invasions/Hawaii_phylo_map/Data", "Hawaii_shapefile"), 
                         layer = "geoentities_Hawaii_islands")

# Transform to data frame (attributes will be lost, but they are not important here)
# Need to use data.frame with coordinates because ggplot can't understand raw sp objects
HW_df <- ggplot2::fortify(HW_shp) # same as broom::tidy(HW_shp)


# Plot

haw_allplots<-ggplot(data = HW_df) + 
    geom_polygon(aes(x     = long,
                     y     = lat, 
                     group = group),
                 colour = "gray40", 
                 fill   = "white", 
                 size   = .5) +
    geom_point(data = HW.dt1, 
               aes(x      = Long_Dec, 
                   y      = Lat_Dec, 
                   size  = S_rare10,
                   colour=S_rare10),
               alpha = .6) +
    scale_colour_gsea(name="Species diversity (Sn)", trans="log", breaks=c(1,2,4,6),labels=c("1","2","4","6"))+
    
    # Annotate (each island name) 
    
    annotate("text", x = -159.5, y = 21.8, label = "Kaua'i",fontface="bold")+
    annotate("text", x = -158, y = 21.8, label = "O'ahu", fontface="bold")+
    annotate("text", x = -156.1, y = 21.1, label = "Maui Nui", fontface="bold")+
    annotate("text", x = -155.2, y = 20.2, label = "Hawai'i", fontface="bold")+
    
    
    # Add north arrow
    ggsn::north(data     = HW_df,
                location = "topright",
                symbol   = 3, # check northSymbols() for more symbols
                anchor   = c(x = -154.5, y = 22.25)) + 
    # add scale bar
    ggsn::scalebar(data     = HW_df,
                   location = "bottomleft",
                   dist     = 50,
                   height   = 0.01,
                   st.dist  = 0.02,
                   st.size  = 3,
                   dd2km    = TRUE, 
                   model    = 'WGS84') +
    coord_equal() + # same as coord_fixed(ratio = 1)
    #guides(colour=guide_legend(title="% Invadede
    # ggtitle("(a) Area + Het + Age")+
    labs(x="Longitude",y="Latitude")+
    
    guides(size=FALSE)+
    
    theme_bw() +
    #theme(legend.justification = c(1, 0),
    #     legend.title.align = 0,
    #    legend.position = c(0.97, 0.5))
    theme(legend.position=c(0.08,0.3), axis.text=element_text(size=6),
          axis.text.x=element_text(size=6), axis.text.y=element_text(size=6),
          title=element_text(size=6,color="black",face="bold",hjust=0.5),
          legend.title = element_text(size=6, color="black",face="bold"),
          legend.background = element_rect(fill="transparent"),
          plot.margin =margin(t=0.01, r=0.1, b=0.01, l=0.1, unit="cm"))


############################
# Macro-ecology diversity  #
############################

#########
# data ##
#########

macro<-read.csv("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Data/Hawaii_Div_Macro_SIE.csv",sep=",",header=T)

macroo<-melt(macro, id.vars="geo_entity2", measure.vars=c("nativeSIE","native_woody_spp","total_SppN"),variable.name="Div",value.name="Div2")

macroo$geo_entity2<-as.factor(macroo$geo_entity2)
macroo$geo_entity2<-factor(macroo$geo_entity2,levels=c("Hawai'i Island","Maui Nui","O'ahu Island","Kaua'i Island"))

macroo$Div<-as.character(macroo$Div)
macroo$Div<-ifelse(macroo$Div=="nativeSIE","Single Island Endemic",macroo$Div)
macroo$Div<-ifelse(macroo$Div=="total_SppN","Total Woody Spp.",macroo$Div)
macroo$Div<-ifelse(macroo$Div=="native_woody_spp","Native Woody Spp.",macroo$Div)

macroo$Div<-as.factor(macroo$Div)
macroo$Div<-factor(macroo$Div,levels=c("Single Island Endemic","Native Woody Spp.","Total Woody Spp."))

macroo2<-filter(macroo, Div!="Total Woody Spp.")

###########
# figure ##
###########


macro_g<-ggplot(macroo2,aes(x=geo_entity2,y=Div2,colour=Div, fill=Div))+ 
    geom_bar(stat="identity",position=position_dodge())+
    scale_x_discrete(labels=c("Hawai'i Island"="Hawai'i","Maui Nui"="Maui Nui","O'ahu Island"="O'ahu","Kaua'i Island"="Kaua'i"))+
    #scale_color_d3(palette="category20c")+ scale_fill_d3(palette="category20c")+
    scale_fill_manual(values=c("#00BF9A","#008975"))+
    scale_colour_manual(values=c("#00BF9A","#008975"))+
    
    scale_y_continuous(breaks=c(0,50, 100, 150, 200, 250, 300))+
    guides(colour=FALSE,fill=guide_legend(title="",title.position = "top"))+
    labs(x="",y="Species diversity")+
    theme_bw()+theme(legend.position="top", 
                     axis.title.y=element_text(colour="black",face="bold",size=8),
                     axis.title.x=element_text(colour="black",face="bold",size=6),
                     axis.text.x=element_text(colour=c("black"),face="bold",size=8),
                     axis.text.y=element_text(colour=c("black"),face="bold",size=8),
                     legend.text=element_text(colour=c("black"),face="bold",size=7),
                     legend.title = element_text(colour=c("black"),face="bold",size=6),
                     legend.title.align = 0.5,
                     legend.key.size = unit(1,"line"),
                     legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank())


##############################################
# bar graph with rarefied species richness   #
##############################################

#########
# data ##
#########

SppR<-read.csv("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Cleaned_Data/RarefySppRichness_alldata.csv",header=T)

SppR$Rarefy<-factor(SppR$Rarefy, levels=c("N100","N1000","N10000","all"))
SppR$Location<-factor(SppR$Location,levels=c("Hawai'i Island","Maui Nui","O'ahu Island","Kaua'i Island"))

SppR$Loc_Rare<-as.factor(paste(SppR$Location,SppR$Rarefy,sep="_"))

SppR$Loc_Rare<-factor(SppR$Loc_Rare, levels=c("Hawai'i Island_N100","Hawai'i Island_N1000","Hawai'i Island_N10000","Hawai'i Island_all",
                                              "Maui Nui_N100","Maui Nui_N1000","Maui Nui_N10000","Maui Nui_all" ,
                                              "O'ahu Island_N100","O'ahu Island_N1000","O'ahu Island_N10000","O'ahu Island_all",
                                              "Kaua'i Island_N100","Kaua'i Island_N1000","Kaua'i Island_N10000","Kaua'i Island_all"))

SppR$Rarefy<-as.character(SppR$Rarefy)
SppR$Rarefy2<-ifelse(SppR$Rarefy=="N100","100",NA)
SppR$Rarefy2<-ifelse(SppR$Rarefy=="N1000","1,000",SppR$Rarefy2)
SppR$Rarefy2<-ifelse(SppR$Rarefy=="N10000","10,000",SppR$Rarefy2)
SppR$Rarefy2<-ifelse(SppR$Rarefy=="all","all",SppR$Rarefy2)
SppR$Rarefy2<-as.factor(SppR$Rarefy2)
SppR$Rarefy2<-factor(SppR$Rarefy2, levels=c("100","1,000","10,000","all"))

###########
# figure ##
###########

All_bar<-ggplot(SppR,aes(x=Loc_Rare,y=S,colour=Rarefy2, fill=Rarefy2))+ 
    geom_bar(stat="identity",position=position_dodge())+
    geom_errorbar(data=SppR,aes(ymin=S-se,ymax =S+se), width=0.6) +
    scale_x_discrete(labels=c("Hawai'i Island_N100"="","Hawai'i Island_N1000"="Hawai'i","Hawai'i Island_N10000"="","Hawai'i Island_all"="",
                              "Maui Nui_N100"="","Maui Nui_N1000"="Maui Nui","Maui Nui_N10000"="","Maui Nui_all"="" ,
                              "O'ahu Island_N100"="","O'ahu Island_N1000"="O'ahu","O'ahu Island_N10000"="","O'ahu Island_all"="",
                              "Kaua'i Island_N100"="","Kaua'i Island_N1000"="Kaua'i","Kaua'i Island_N10000"="","Kaua'i Island_all"=""))+
    
    scale_fill_manual(values=c("#C5CAE9","#7986CB","#3949AB","#1A2373"))+
    scale_colour_manual(values=c("#C5CAE9","#7986CB","#3949AB","#1A2373"))+
    
    scale_y_continuous(limits=c(0,64),breaks=c(0, 10, 20, 30, 40, 50, 60))+
    guides(colour=FALSE,fill=guide_legend(title="Sample size (individuals)",title.position = "top"))+
    labs(x="",y="Species diversity")+
    theme_bw()+theme(legend.position="top", 
                     axis.title.y=element_blank(),
                     axis.title.x=element_text(colour="black",face="bold",size=6),
                     axis.text.x=element_text(colour=c("black"),face="bold",size=8, hjust=c(0.2,0,0,0)),
                     axis.text.y=element_text(colour=c("black"),face="bold",size=8),
                     legend.text=element_text(colour=c("black"),face="bold",size=7),
                     legend.title = element_text(colour="black",face="bold",size=7),
                     legend.title.align = 0.5,
                     legend.key.size = unit(1,"line"),
                     legend.margin =margin(t=0, r=0, b=0, l=0, unit="cm"),
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank())

############################
# make the age/area arrow  #
############################

x<-seq(from=1, to=10, by=0.1)
y<-rep.int(1,length(x))
y3<-rep.int(seq(from=1, to=5, by=1), length(x))
dff<-cbind.data.frame(x,y,y3)

df<- data.frame(x1=0, x2=10, y1=0.5,y2=0.5, y3=1,y4=1)

white_space<-ggplot(dff, aes(x=x,y=y))+
    geom_segment(data=df, aes(x=x1,y=y1, xend=x2,yend=y2),size=1,arrow = arrow(length = unit(0.5, "cm"),type="closed"))+
    geom_segment(data=df, aes(x=x2,y=y4, xend=x1,yend=y3),size=1,arrow = arrow(length = unit(0.5, "cm"),type="closed"))+
    ylim(c(0.30,1.3))+

    annotate("text", x = 5, y = 0.7, label = "Island Area",fontface="bold")+
    annotate("text", x = 5, y = 1.2, label = "Island Age", fontface="bold")+
    coord_fixed(ratio = 1)+

    theme( axis.title.y=element_blank(),
        axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
         axis.line=element_blank(),
         axis.ticks = element_blank())


#################
# put together ##
#################

require(cowplot)

haw_map<-plot_grid(white_space, haw_allplots, labels=c("","a)"),ncol=1, label_size = 7,rel_heights = c(1,7), rel_widths = c(1,8),vjust=c(1.5,1.3))

div_fig1<-plot_grid(macro_g,All_bar,labels=c("b)","c)"),label_size =7, ncol=2 ,vjust=c(4,4) )

comb_fig1<-plot_grid(haw_map, div_fig1, ncol=1,rel_heights = c(1.8,1.4),rel_widths=c(2,1))

# Save as png file

ggsave(filename = file.path("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Figures", "Big_Fig1.png"), 
       width    = 29.7, 
       height   = 25, 
       units    = "cm")

comb_fig1

dev.off()
