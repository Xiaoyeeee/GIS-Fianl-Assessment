library(sf)
library(here)
library(janitor)
library(tidyverse)
library(dplyr)
library(tmap)
library(tmaptools)
library(spdep)

#Part1 Data cleaning
#read geojson from internet
UKDistricts <- st_read("https://opendata.arcgis.com/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson") %>%
  st_transform(.,27700)

LondonDistricts <- UKDistricts %>%
  filter(str_detect(lad15cd, "^E09"))

#read csv
PopbyAge <- read.csv("asm_data/census-2011-population-5year-age-londondistricts.csv") %>%
  clean_names()

#join percentage with LondonDistricts
Proportion <- PopbyAge %>%
  dplyr::select(code,proportion) %>%
  left_join(LondonDistricts, .,
            by = c("lad15cd" ="code"))

#map the proportion of London districts
tm_shape(Proportion) +
  tm_polygons("proportion",
              style="jenks",
              palette="YlGnBu",
              midpoint=NA,
              popup.vars=c("lad15nm", "proportion"))+
  tm_layout(title = "Aging Proportion",
            title.position = c(0.68,0.95),
            legend.position = c("left","bottom"))

palette_explorer()



#-------------------------------------------------------------------------------
#Part2 Spatial Autocorrelation 

#Global Moran's I
#calculate the centroids of all districts in London
centroLondon <- Proportion %>%
  st_centroid() %>%
  st_geometry()

plot(centroLondon, axes = TRUE)

#generate a spatial weights matrix,
#create a neighbours list
LDis_nb <- Proportion %>%
  poly2nb (., queen = TRUE)

#plot centroids and a map underneath
plot(LDis_nb, st_geometry(centroLondon), col = "red")
plot(Proportion$geometry, add = T)

#create a spatial weights object from these weights
LDis.lw <- LDis_nb %>%
  nb2listw(., style = "W")

#Global Moran's I
Global_I_LDis <- Proportion %>%
  pull(proportion) %>%
  as.vector() %>%
  moran.test(., LDis.lw)

Global_I_LDis

#Geary's C
Global_C_LDis <- 
  Proportion %>%
  pull(proportion) %>%
  as.vector()%>%
  geary.test(.,LDis.lw)

Global_C_LDis

Global_C_LDis <- 
  Proportion %>%
  pull(proportion) %>%
  as.vector()%>%
  globalG.test(.,LDis.lw)


#Local Moran's I
#generate local Moran's I for each districts in London
Local_I_LDis_Density <- Proportion %>%
  pull(proportion) %>%
  as.vector() %>%
  localmoran(.,LDis.lw) %>%
  as_tibble()

slice_head(Local_I_LDis_Density, n = 5)  

#join I score and Z score back into dataframe
Proportion <- Proportion %>%
  mutate(proportion_I = as.numeric(Local_I_LDis_Density$Ii)) %>%
  mutate(proportion_Iz = as.numeric(Local_I_LDis_Density$Z.Ii))


#plot a map of local Moran's I 
#set breaks
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)

#plot a interactive map
tmap_mode("plot")
tm_shape(Proportion) +
  tm_polygons("proportion_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA)+
  tm_layout(title = "Local Moran's I, Aging proportion in London",
            title.position = c(0.38,0.95),
            title.size = 1,
            legend.position = c("left","bottom"))


#Getis Ord 
#generate localG
Local_Gi_LDis_proportion <- Proportion %>%
  pull(proportion) %>%
  as.vector() %>%
  localG(.,LDis.lw)
head(Local_Gi_LDis_proportion)

Proportion <- Proportion %>%
  mutate(proportion_G = as.numeric(Local_Gi_LDis_proportion))

#map the Getis Ord outputs
tmap_mode("plot")
tm_shape(Proportion)+
  tm_polygons("proportion_G",
              style="fixed",
              breaks=breaks1,
              palette = "PRGn",
              midpoint=NA)+
  tm_layout(title = "Gi*, Aging proportion in London",
            title.position = c(0.55,0.95),
            title.size = 1,
            legend.position = c("left","bottom"))


  
  
  
  
  
  