
if(!require(plyr)) install.packages("plyr") &
  require(plyr)
if(!require(sf)) install.packages("sf") &
  require(sf)
if(!require(tmap)) install.packages("tmap") &
  require(tmap)
if(!require(rosm)) install.packages("rosm") &
  require(rosm)
if(!require(viridis)) install.packages("viridis") &
  require(viridis)

source("1_Regression.R")

# Map data
Route_poly <- if(UI == "Route_1") read_sf(dsn = "https://raw.githubusercontent.com/Richtea84/Uncovering-Bristol/main/route_1_2023.kml") else 
  if(UI == "Route_2") read_sf(dsn = "https://raw.githubusercontent.com/Richtea84/Uncovering-Bristol/main/route_2_2023.kml") else
    if(UI == "Route_3") read_sf(dsn = "https://raw.githubusercontent.com/Richtea84/Uncovering-Bristol/main/route_3_2023.kml") else
      if(UI == "Route_4") read_sf(dsn = "https://raw.githubusercontent.com/Richtea84/Uncovering-Bristol/main/route_4_2023.kml") else NA

Route_poly_a <- merge(x = Route_poly, y = IMD_scores, by.x = "Name", by.y = "name")
Route_poly_a$Stop.no. <- as.factor(Route_poly_a$Stop.no.)

Weighted_average <- as.data.frame(Weighted_average)
Unweighted_average <- as.data.frame(Unweighted_average)

ifelse(Route_poly_a$Stop.no. == "1", Weighted_average$Weighted_average[1],
       ifelse(Route_poly_a$Stop.no. == "2", Weighted_average$Weighted_average[2],
              ifelse(Route_poly_a$Stop.no. == "3", Weighted_average$Weighted_average[3],
                      ifelse(Route_poly_a$Stop.no. == "4", Weighted_average$Weighted_average[4],
                             ifelse(Route_poly_a$Stop.no. == "5", Weighted_average$Weighted_average[5],
                                    ifelse(Route_poly_a$Stop.no. == "6", Weighted_average$Weighted_average[6],
                                           ifelse(Route_poly_a$Stop.no. == "7", Weighted_average$Weighted_average[7],
                                                  ifelse(Route_poly_a$Stop.no. == "8", Weighted_average$Weighted_average[8],
                                                         ifelse(Route_poly_a$Stop.no. == "9", Weighted_average$Weighted_average[9],
                                                                ifelse(Route_poly_a$Stop.no. == "10", Weighted_average$Weighted_average[10], NA)))))))))) -> Route_poly_a$My_Weighted_Scores
       

ifelse(Route_poly_a$Stop.no. == "1", Unweighted_average$Unweighted_average[1],
       ifelse(Route_poly_a$Stop.no. == "2", Unweighted_average$Unweighted_average[2],
              ifelse(Route_poly_a$Stop.no. == "3", Unweighted_average$Unweighted_average[3],
                     ifelse(Route_poly_a$Stop.no. == "4", Unweighted_average$Unweighted_average[4],
                            ifelse(Route_poly_a$Stop.no. == "5", Unweighted_average$Unweighted_average[5],
                                   ifelse(Route_poly_a$Stop.no. == "6", Unweighted_average$Unweighted_average[6],
                                          ifelse(Route_poly_a$Stop.no. == "7", Unweighted_average$Unweighted_average[7],
                                                 ifelse(Route_poly_a$Stop.no. == "8", Unweighted_average$Unweighted_average[8],
                                                        ifelse(Route_poly_a$Stop.no. == "9", Unweighted_average$Unweighted_average[9],
                                                               ifelse(Route_poly_a$Stop.no. == "10", Unweighted_average$Unweighted_average[10], NA)))))))))) -> Route_poly_a$My_Unweighted_Scores


# Visual output
Route_poly_b <- st_transform(Route_poly_a, 4326)

base_map <- rosm::osm.raster(Route_poly_b)

# Social grade mapping...
Map_1 <- tm_shape(base_map)+
  tm_rgb(saturation = 0)+
  tm_shape(Route_poly_b)+
  tm_polygons("Soc_grad", palette = "RdBu", n = 5,
              title = "No. people in\nSemi & Unskilled Occupations", alpha = 0.6)+
  tm_compass()+
  tm_scale_bar()+
  tm_layout(legend.outside = T,main.title = "Deprivation based on social grade",
            main.title.position = c("left", "top"))

# Unweighted deprivation
Map_2 <- tm_shape(base_map)+
  tm_rgb(saturation = 0)+
  tm_shape(Route_poly_b)+
  tm_polygons("My_Unweighted_Scores", palette = "BrBG", n = 5,
              title = expression(paste("Unweighted ", bar(x), " values")), alpha = 0.6)+
  tm_compass()+
  tm_scale_bar()+
  tm_layout(legend.outside = T,main.title = "Unweighted environmental\nquality scores",
            main.title.position = c("left", "top"))

# Weighted deprivation
Map_3 <- tm_shape(base_map)+
  tm_rgb(saturation = 0)+
  tm_shape(Route_poly_b)+
  tm_polygons("My_Weighted_Scores", palette = "BrBG", n = 5,
              title = expression(paste("Unweighted ", bar(x), " values")), alpha = 0.6)+
  tm_compass()+
  tm_scale_bar()+
  tm_layout(legend.outside = T,main.title = "Weighted environmental\nquality scores",
            main.title.position = c("left", "top"))


tmap_save(tm = Map_1, filename = "Social Grade Map 2.jpg")
tmap_save(tm = Map_2, filename = "Unweighted Scores Map.jpg")
tmap_save(tm = Map_3, filename = "Weighted Scorese Map.jpg")
