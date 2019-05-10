#####################
# MAP        ########
# plot diversity +  #
# % invaded         #
#####################

require(sp)
require(rgdal)
require(ggplot2)
require(ggsn) # for north symbols and scale bars
require(dplyr)
require(reshape2)
#require(ggsci)
require(viridis)
require(cowplot)

# read point data
# file.path() should be platform independent

HW.dt1<-read.table(file=file.path("/home/dylan/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Cleaned_Data", "Hawaii_mapdata.csv"),sep=",",header=TRUE, row.names=NULL)

# load Hawaii shapefile
# This shapefile was cropped in ArcMap from the geoentities.RData from Patrick Weigelt
HW_shp <- rgdal::readOGR(dsn   = file.path("/home/dylan/Dropbox (iDiv)/Research_projects/UFZ_IslandFun/Island_plant_invasions/Hawaii_phylo_map/Data", "Hawaii_shapefile"), 
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
                 size  = S_n,
                 colour=Plot_Prop_Invaded,
                 fill="white"),
             shape="O") +
    scale_colour_viridis(name="% Invaded", option="D") +
      # Annotate (each island name) 
    
    annotate("text", x = -159.5, y = 21.8, label = "Kaua'i",fontface="bold", size=5)+
    annotate("text", x = -158, y = 21.8, label = "O'ahu", fontface="bold", size=5)+
    annotate("text", x = -156.1, y = 21.1, label = "Maui Nui", fontface="bold", size=5)+
    annotate("text", x = -155.2, y = 20.2, label = "Hawai'i", fontface="bold", size=5)+
    
  labs(x="Longitude",y="Latitude")+
  
  guides(size=FALSE, fill=FALSE)+
  
    # Add north arrow
    ggsn::north(data     = HW_df,
                location = "topright",
                symbol   = 3, # check northSymbols() for more symbols
                anchor   = c(x = -154.5, y = 22.25)) + 
    # add scale bar
    ggsn::scalebar(data     = HW_df,
                   location = "bottomleft",
                   transform=TRUE,
                   dist     = 50,
                   dist_unit="km",
                   height   = 0.01,
                   st.dist  = 0.02,
                   st.size  = 3,
                   #dd2km    = TRUE, 
                   model    = 'WGS84') +
    coord_equal() + # same as coord_fixed(ratio = 1)
   
    theme_bw() +
  
    theme(legend.position=c(0.07,0.3), axis.text=element_text(size=11),
          axis.text.x=element_text(size=11), axis.text.y=element_text(size=11),
          axis.title.x = element_text(size=14),axis.title.y = element_text(size=14),
          title=element_text(size=6,color="black",face="bold",hjust=0.5),
          legend.title = element_text(size=11, color="black",face="bold"),
          legend.background = element_rect(fill="transparent"),
          plot.margin =margin(t=0.01, r=0.1, b=0.01, l=0.1, unit="cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

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

haw_map<-plot_grid(white_space, haw_allplots, ncol=1, rel_heights = c(1,7), rel_widths = c(1,8),vjust=c(1.5,1.3))

# Save as png file

ggsave(filename = file.path("Figures", "Fig1_Map_Plots_LocalSppDiv.png"), 
       width    = 17.8, 
       height   = 15.0, 
       units    = "cm", dpi=900)

haw_map

dev.off()

# Save as pdf


ggsave(filename = file.path("Figures", "Fig1_Map_Plots_LocalSppDiv.pdf"), 
       width    = 17.8, 
       height   = 15.0, 
       units    = "cm", dpi=900)

haw_map

dev.off()
