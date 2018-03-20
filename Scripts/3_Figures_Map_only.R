# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create code frame for a plot for Hawaii area
# Color gradient for points based on numeric column
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(sp)
require(rgdal)
require(ggplot2)
require(ggsn) # for north symbols and scale bars
require(dplyr)
require(reshape2)
require(ggsci)

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
                   colour=PET),
               alpha = .6) +
    scale_colour_gsea(name="PET")+
    
# Annotate (each island name) 
    
    annotate("text", x = -159.5, y = 21.8, label = "Kaua'i",fontface="bold")+
    annotate("text", x = -158, y = 21.8, label = "O'ahu", fontface="bold")+
    annotate("text", x = -156.1, y = 21.0, label = "Maui Nui", fontface="bold")+
    annotate("text", x = -155.2, y = 20.1, label = "Hawai'i", fontface="bold")+
    
    
        
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
    
    coord_fixed(ratio = 1)+
    
    guides(size=FALSE)+
    
    theme_bw() +
    #theme(legend.justification = c(1, 0),
    #     legend.title.align = 0,
    #    legend.position = c(0.97, 0.5))
    theme(legend.position=c(0.05,0.2), 
          axis.title=element_text(size=7, face="bold"),
          axis.text.x=element_text(size=6), axis.text.y=element_text(size=6),
          legend.title = element_text(size=7, color="black",face="bold"),
          plot.margin =margin(t=0.01, r=0.1, b=0.01, l=0.1, unit="cm"))

# Save as png file

ggsave(filename = file.path("/homes/dc78cahe/Dropbox (iDiv)/Research_projects/Veg. monitoring databases/databases and field protocols/database/IslandForests/Hawaii_only/Diversity_Age/Hawaii_diversity/Figures", "Hawaii_allplots_mapdata.png"), 
width    = 29.7, 
height   = 21, 
units    = "cm")

haw_allplots

dev.off()
