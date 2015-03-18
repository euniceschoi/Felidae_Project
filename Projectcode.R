#define read.table for population trend count
df <- read.table(text='Count    PopulationTrend
2  Unknown 
5 Stable
30  Decreasing', header=TRUE)

library("ggplot2")
#plot population trend onto a bar graph
ggplot(data=df, aes(x=PopulationTrend, y=Count)) + 
  geom_bar(aes(fill=PopulationTrend), stat="identity")

#define read.table for threatlevel count 
df <- read.table(text='Count    ThreatLevel
9  Vulnerable
7 Endangered
1 CriticallyEndangered
9 NearThreatened
11  LeastConcerned', header=TRUE)

library("ggplot2")
#order x axis values
ThreatLevel <- c("LeastConcerned", "NearThreatened", "Vulnerable", "Endangered", "CriticallyEndangered")
#plot threatlevel trend onto bar graph
ggplot(data=df, aes(x = ThreatLevel, y=Count)) + 
  scale_x_discrete(limits = ThreatLevel) + 
  geom_bar(aes(fill=ThreatLevel), stat="identity")

#read table with species separated by threat and continent
df <- read.table(text='Count    ThreatLevel  Continent
                3 LeastConcerned  NorthAm
4 NearThreatened  NorthAm
1 Vulnerable  NorthAm
2 LeastConcerned  SouthAm
2 NearThreatened  SouthAm
2 Vulnerable  SouthAm
1 Endangered  SouthAm
2 LeastConcerned  Eu
1 CriticallyEndangered  Eu
5 LeastConcerned  Asia
5 NearThreatened  Asia
5 Vulnerable  Asia
5 Endangered  Asia
3 LeastConcerned  Africa
3 NearThreatened  Africa
3 Vulnerable  Africa
1 NearThreatened  Aus
1 Vulnerable  Aus', header=TRUE)

#plot using ggplot
require(ggplot2)
p <- ggplot(df, aes(Continent, Count, fill=ThreatLevel)) + 
    geom_bar(stat='identity') +
    theme_bw(base_size=16)
print(p)

#load packages
library("ggmap")

#concatenate list of countries for each species
Leopardus_tigrinus <- c("Argentina", "Bolivia, 
Plurinational States of", " Brazil", 
"Colombia", "Costa Rica", "Ecuador",
"French Guiana", "Guyana", " Panama",
"Paraguay", "Peru", "Suriname", "Venezuela")

#geocode countries and save as a table
ll.Leopardus_tigrinus = geocode(Leopardus_tigrinus)

#load packages
library(rgdal)
library(ggplot2)
library(sp)

# read world map shapefile from NaturalEarth
worldmap = readOGR(dsn="ne_110m_land.shp", layer="ne_110m_land")
# convert to dataframe
worldmap_df = fortify(worldmap)

# read graticule shapefile
grat = readOGR("ne_110m_graticules_15.shp", layer="ne_110m_graticules_15") 
grat_df = fortify(grat)

# read bounding box shapefile
bbox = readOGR("ne_110m_wgs84_bounding_box.shp", layer="ne_110m_wgs84_bounding_box") 
bbox_df = fortify(bbox)

# read urban areas shapefile
urban = readOGR("ne_50m_urban_areas.shp", layer="ne_50m_urban_areas")
urban_df = fortify(urban)

#read countries shapefile
countries=readOGR("ne_110m_admin_0_countries.shp", layer ="ne_110m_admin_0_countries")
countries_df = fortify(countries)

#convert all to dataframe

library(ggplot2)
ggplot(bbox_df, aes(x=long,y=lat, group=group)) + #plot map
  geom_polygon(fill="#58D3F7") +
  geom_polygon(data=worldmap_df, aes(x=long,y=lat, group=group, fill=hole)) + #add world map
  geom_path(data=countries_df, aes(x=long,y=lat, group=group, fill=hole), color="#0B610B") + 
  #add countries
  geom_polygon(data=urban_df, aes(x=long,y=lat, group=group, fill=hole), color="#BDBDBD", border="#848484", lwd=0.2, add=TRUE, 
  alpha = 0.1) + #add urban areas
  geom_point(data=ll.Lynx_pardinus, aes(x=lon,y=lat, group=NULL), color="#FE2E9A", lwd=5.0,alpha = 0.7) + #add species points
  labs(title="Critically Endangered Felidae Species") + #add labels
  coord_equal() + #add coordinates
  scale_fill_manual(values=c("#04B404", "#58D3F7"), guide="none") 
  #color land
