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

###Least Concern geocode
Leptailurus_serval =c("Angola", " Benin", " Botswana", " Burkina Faso", " Burundi", " Cameroon", " Central African Republic", " Chad", " Congo", " Congo, The Democratic Republic of the", " Côte d'Ivoire", " Djibouti", " Eritrea", " Ethiopia", " Gabon", " Gambia", " Ghana", " Guinea", " Guinea-Bissau", " Kenya", " Liberia", " Malawi", " Mali", " Morocco", " Mozambique", " Namibia", " Niger", " Nigeria", " Rwanda", " Senegal", " Sierra Leone", " Somalia", " South Africa", " South Sudan", " Sudan", " Swaziland", " Tanzania", "Togo", " Uganda", " Zambia", " Zimbabwe")
ll.Leptailurus_serval=geocode(Leptailurus_serval)

Felis_silvestris =c("Afghanistan", " Albania", " Algeria", " Andorra", " Angola", " Armenia", " Austria", " Azerbaijan", " Belarus", " Belgium", " Benin", " Bosnia and Herzegovina", " Botswana", " Bulgaria", " Burkina Faso", " Burundi", " Cameroon", " Central African Republic", " Chad", " China", " Congo", " Congo", " Croatia", " Czech Republic", " Djibouti", " Egypt", " Eritrea", " Ethiopia", " France", " Gambia", " Georgia", " Germany", " Ghana", " Gibraltar", " Greece", " Guinea", " Guinea-Bissau", " Hungary", " India", " Iran, Islamic Republic of", " Iraq", " Israel", " Italy", " Jordan", " Kazakhstan", " Kenya", " Kuwait", " Kyrgyzstan", " Latvia", " Lebanon", " Lesotho", " Libya", " Lithuania", " Luxembourg", " Macedonia", " Malawi", " Mali", " Mauritania", " Moldova", " Mongolia", " Montenegro", " Morocco", " Mozambique", " Namibia", " Niger", " Nigeria", " Oman", " Pakistan", " Poland", " Portugal", " Romania", " Russian Federation", " Rwanda", " Saudi Arabia", " Senegal", " Serbia", " Sierra Leone", " Slovakia", " Slovenia", " Somalia", " South Africa", " South Sudan", " Spain", " Sudan", " Swaziland", " Switzerland", " Syrian Arab Republic", " Tajikistan", " Tanzania", "Togo", " Tunisia", " Turkey", " Turkmenistan", " Uganda", " Ukraine", " United Arab Emirates", " United Kingdom", " Uzbekistan", " Western Sahara", " Yemen", " Zambia", " Zimbabwe")
ll.Felis_silvestris=geocode(Felis_silvestris)

Herpailurus_yagouaroundi =c("Argentina", " Belize", " Bolivia", " Brazil", " Colombia", " Costa Rica", " Ecuador", " El Salvador", " French Guiana", " Guatemala", " Guyana", " Honduras", " Mexico", " Nicaragua", " Panama", " Paraguay", " Peru", " Suriname", " Venezuela")
ll.Herpailurus_yagouaroundi=geocode(Herpailurus_yagouaroundi)

Felis_chaus=c("Afghanistan", " Armenia", " Azerbaijan", " Bangladesh", " Bhutan", " Cambodia", " China", " Egypt", " Georgia", " India", " Iran, Islamic Republic of", " Iraq", " Israel", " Jordan", " Kazakhstan", " Kyrgyzstan", " Laos", " Lebanon", " Myanmar", " Nepal", " Pakistan", " Russian Federation", " Sri Lanka", " Syrian Arab Republic", " Tajikistan", " Thailand", " Turkey", " Turkmenistan", " Uzbekistan", " Vietnam")
ll.Felis_chaus=geocode(Felis_chaus)

Prionailurus_planiceps =c("Brunei Darussalam", "Kalimantan","Sumatera", "Malaysia", "Sabah", "Sarawak", " Thailand")
ll.Prionailurus_planiceps=geocode(Prionailurus_planiceps)

Puma_concolor =c("Argentina", " Belize", " Bolivia, Plurinational States of", " Brazil", " Canada", " Chile", " Colombia", " Costa Rica", " Ecuador", " El Salvador", " French Guiana", " Guatemala", " Guyana", " Honduras", " Mexico", " Nicaragua", " Panama", " Paraguay", " Peru", " Suriname", " United States", " Venezuela")
ll.Puma_concolor=geocode(Puma_concolor)

Caracal_caracal = c("Afghanistan", " Algeria", " Angola", " Benin", " Botswana", " Burkina Faso", " Cameroon", " Chad", " Congo"," Côte d'Ivoire", " Djibouti", " Egypt", " Eritrea", " Ethiopia", " Gambia", " Ghana", " Guinea", " Guinea-Bissau", " India", " Iran", " Iraq", " Israel", " Jordan", " Kazakhstan", " Kenya", " Kuwait", " Lebanon", " Lesotho", " Libya", " Malawi", " Mali", " Mauritania", " Morocco", " Mozambique", " Namibia", " Niger", " Nigeria", " Oman", " Pakistan", " Saudi Arabia", " Senegal", " Somalia", " South Africa", " South Sudan", " Sudan", " Swaziland", " Syrian Arab Republic", " Tajikistan", " Tanzania", " Togo", " Tunisia", " Turkey", " Turkmenistan", " Uganda", " United Arab Emirates", " Uzbekistan", " Western Sahara", " Yemen", " Zambia", " Zimbabwe")
ll.Caracal_caracal=geocode(Caracal_caracal)

Lynx_lynx = c("Afghanistan", " Albania", " Armenia", " Austria", " Azerbaijan", " Belarus", " Bhutan", " Bosnia and Herzegovina", " Bulgaria", " China", " Croatia", " Czech Republic", " Estonia", " Finland", " France", " Georgia", " Germany", " Greece", " Hungary", " India", " Iran", " Iraq", " Italy", " Kazakhstan", " Korea", " Kyrgyzstan", " Latvia", " Lithuania", " Macedonia, the former Yugoslav Republic of", " Moldova", " Mongolia", " Montenegro", " Nepal", " Norway", " Pakistan", " Poland", " Romania", " Russian Federation", " Serbia", " Slovakia", " Slovenia", " Spain", " Sweden", " Switzerland", " Tajikistan", " Turkey", " Turkmenistan", " Ukraine", " Uzbekistan")
ll.Lynx_lynx=geocode(Lynx_lynx)

Lynx_rufus = c("British Columbia", "Manitoba", "New Brunswick", "Ontario", "Québec", " Mexico", "Alabama", "Arizona", "Arkansas","California", "Colorado", "Connecticut", "Florida", "Georgia", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin, Wyoming")
ll.Lynx_rufus=geocode(Lynx_rufus)

Prionailurus_bengalensis = c("Afghanistan", " Bangladesh", " Bhutan", " Brunei Darussalam", " Cambodia", " China", " Hong Kong", " India", "Jawa", "Kalimantan", "Sumatera", " Japan", " Korea", " Korea", " Laos", " Malaysia", " Myanmar", " Nepal", " Pakistan", " Philippines", "Russia", " Singapore", " Taiwan", " Thailand", " Vietnam")
ll.Prionailurus_bengalensis =geocode(Prionailurus_bengalensis)

Leopardus_pardalis = c("Argentina", " Belize", " Bolivia", " Brazil", " Colombia", " Costa Rica", " Ecuador", " El Salvador", " French Guiana", " Guatemala", " Guyana", " Honduras", " Mexico", " Nicaragua", " Panama", " Paraguay", " Peru", " Suriname", " Trinidad", "Tobago", " United States", " Venezuela")
ll.Leopardus_pardalis =geocode(Leopardus_pardalis)

ggplot(bbox_df, aes(x=long,y=lat, group=group)) +
  geom_polygon(fill="#58D3F7") +
  geom_polygon(data=wmap_df, aes(x=long,y=lat, group=group, fill=hole)) + 
  geom_path(data=countries_df, aes(x=long,y=lat, group=group, fill=hole), color="#0B610B")+
  geom_polygon(data=urban_df, aes(x=long,y=lat, group=group, fill=hole), color="#BDBDBD", border="#848484", lwd=1.0, add=TRUE, alpha=I(2/10)) + 
  geom_point(data=ll.Leptailurus_serval, aes(x=lon,y=lat, group=NULL), color="#0B0B61", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Felis_silvestris, aes(x=lon,y=lat, group=NULL), color="#0101DF", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Herpailurus_yagouaroundi, aes(x=lon,y=lat, group=NULL), color="#5858FA", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Felis_chaus, aes(x=lon,y=lat, group=NULL), color="#81BEF7", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Prionailurus_planiceps, aes(x=lon,y=lat, group=NULL), color="#0080FF", lwd=5.0,alpha = 0.5 ) +
  geom_point(data=ll.Puma_concolor, aes(x=lon,y=lat, group=NULL), color="#084B8A", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Caracal_caracal, aes(x=lon,y=lat, group=NULL), color="#0080FF", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Lynx_lynx, aes(x=lon,y=lat, group=NULL), color="#5858FA", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Lynx_rufus, aes(x=lon,y=lat, group=NULL), color="#0101DF", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Prionailurus_bengalensis, aes(x=lon,y=lat, group=NULL), color="#0B0B61", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Leopardus_pardalis, aes(x=lon,y=lat, group=NULL), color="#0489B1", lwd=5.0,alpha = 0.5) +
  geom_path(data=grat_df, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50") +
  labs(title="Least Concern Felidae Species") + 
  coord_equal() + 
  #theme_opts +
  scale_fill_manual(values=c("#04B404", "#58D3F7"), guide="none")


###Near threatened geocode and map
Catopuma_temminckii.nt1=c("Bangladesh","Bhutan","Cambodia","China","India","Indonesia (Sumatera)","Lao People's Democratic Republic","Malaysia","Myanmar","Nepal","Thailand","Viet Nam")
ll.Catopuma_temminckii.nt1=geocode(Catopuma_temminckii.nt1)

Panthera_pardus.nt2=c("Afghanistan","Algeria","Angola","Armenia","Azerbaijan","Bangladesh","Benin","Bhutan","Botswana","Burkina Faso","Burundi","Cambodia","Cameroon","Central African Republic","Chad","China","Congo","Côte d'Ivoire","Djibouti","Egypt","Equatorial Guinea","Eritrea","Ethiopia","Gabon","Gambia","Georgia","Ghana","Guinea","Guinea-Bissau","India","Jawa","Iran, Islamic Republic of","Israel","Jordan","Kenya","Korea, Democratic People's Republic of","Laos","Liberia","Malawi","Malaysia","Mali","Morocco","Mozambique","Myanmar","Namibia","Nepal","Niger","Nigeria","Oman","Pakistan","Russian Federation","Rwanda","Saudi Arabia","Senegal","Sierra Leone","Somalia","South Africa","South Sudan","Sri Lanka","Sudan","Swaziland","Tajikistan","Tanzania, United Republic of","Thailand","Togo","Turkey","Turkmenistan","Uganda","United Arab Emirates","Uzbekistan","Viet Nam","Yemen","Zambia","Zimbabwe")
ll.Panthera_pardus.nt2=geocode(Panthera_pardus.nt2)

Caracal_aurata.nt3=c("Angola","Burundi","Cameroon","Central African Republic","Congo","Congo, The Democratic Republic of the","Côte d'Ivoire","Equatorial Guinea","Gabon","Ghana","Guinea","Kenya","Liberia","Nigeria","Rwanda","Senegal","Sierra Leone","South Sudan","Uganda")
ll.Caracal_aurata.nt3=geocode(Caracal_aurata.nt3)

Leopardus_colocolo.nt4=c("Argentina","Bolivia, Plurinational States of","Brazil","Chile","Ecuador","Paraguay","Peru","Uruguay")
ll.Leopardus_colocolo.nt4=geocode(Leopardus_colocolo.nt4)

Leopardus_wiedii.nt5=c("Argentina","Belize","Bolivia, Plurinational States of","Brazil","Colombia","Costa Rica","Ecuador","El Salvador","French Guiana","Guatemala","Guyana","Honduras","Mexico","Nicaragua","Panama","Paraguay","Peru","Suriname","Uruguay","Venezuela")
ll.Leopardus_wiedii.nt5=geocode(Leopardus_wiedii.nt5)

Leopardus_geoffroyi.nt6=c("Argentina","Bolivia","Brazil","Chile","Paraguay","Uruguay")
ll.Leopardus_geoffroyi.nt6=geocode(Leopardus_geoffroyi.nt6)

Felis_margarita.nt7=c("Algeria","Egypt","Iran, Islamic Republic of","Israel","Jordan","Kazakhstan","Kuwait","Mauritania","Morocco","Niger","Oman","Pakistan","Saudi Arabia","Syrian Arab Republic","Turkmenistan","United Arab Emirates","Uzbekistan","Western Sahara","Yemen")
ll.Felis_margarita.nt7=geocode(Felis_margarita.nt7)

Panthera_onca.nt8=c("Argentina","Belize","Bolivia, Plurinational States of","Brazil","Colombia","Costa Rica","Ecuador","French Guiana","Guatemala","Guyana","Honduras","Mexico","Nicaragua","Panama","Paraguay","Peru","Suriname","United States","Venezuela")
ll.Panthera_onca.nt8=geocode(Panthera_onca.nt8)

Otocolobus_manul.nt9=c("Afghanistan","Armenia","Azerbaijan","Beijing", "Nei Mongol", "Ningxia", "Qinghai","Shaanxi", "Sichuan", "Tibet", "Xinjiang","India","Iran, Islamic Republic of","Kazakhstan","Kyrgyzstan","Mongolia","Pakistan","Russia","Tajikistan","Turkmenistan","Uzbekistan")
ll.Otocolobus_manul.nt9=geocode(Otocolobus_manul.nt9)

ggplot(bbox_df, aes(x=long,y=lat, group=group)) +
  geom_polygon(fill="#58D3F7") +
  geom_polygon(data=wmap_df, aes(x=long,y=lat, group=group, fill=hole)) + 
  geom_path(data=countries_df, aes(x=long,y=lat, group=group, fill=hole), color="#0B610B")+
  geom_polygon(data=urban_df, aes(x=long,y=lat, group=group, fill=hole), color="#BDBDBD", border="#848484", lwd=1.0, add=TRUE, alpha=I(2/10)) + 
  geom_point(data=ll.Catopuma_temminckii.nt1, aes(x=lon,y=lat, group=NULL), color="#8A2908", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Panthera_pardus.nt2, aes(x=lon,y=lat, group=NULL), color="#B43104", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Caracal_aurata.nt3, aes(x=lon,y=lat, group=NULL), color="#FF4000", lwd=5.0,alpha = 0.5 ) +
  geom_point(data=ll.Leopardus_colocolo.nt4, aes(x=lon,y=lat, group=NULL), color="#FA8258", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Leopardus_wiedii.nt5, aes(x=lon,y=lat, group=NULL), color="#F78181", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Leopardus_geoffroyi.nt6, aes(x=lon,y=lat, group=NULL), color="#FA5858", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Felis_margarita.nt7, aes(x=lon,y=lat, group=NULL), color="#FF0000", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Panthera_onca.nt8, aes(x=lon,y=lat, group=NULL), color="#B40404", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Otocolobus_manul.nt9, aes(x=lon,y=lat, group=NULL), color="#610B0B", lwd=5.0,alpha = 0.5) +
  geom_path(data=grat_df, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50") +
  labs(title="Near Threatened Felidae Species") + 
  coord_equal() + 
  theme_opts +
  scale_fill_manual(values=c("#04B404", "#58D3F7") ,guide="none")

###Vulnerable geocode and map

Acinonyx_jubatus <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Central African Republic", "Chad", "Congo", "Ethiopia", "Iran", "Kenya", "Mozambique", "Namibia", "Niger", "Somalia",  "South Africa", "South Sudan", "Sudan", "Tanzania")
ll.Acinonyx_jubatus <- geocode(Acinonyx_jubatus)

Leopardus_tigrinus <- c("Argentina", " Bolivia, Plurinational States of", " Brazil", " Colombia", " Costa Rica", " Ecuador", " French Guiana", " Guyana", " Panama", " Paraguay", " Peru", " Suriname", " Venezuela")
ll.Leopardus_tigrinus <- geocode(Leopardus_tigrinus)

Neofelis_diardi <- c("Brunei Darussalam", " Indonesia (Kalimantan, Sumatera)", " Malaysia (Sabah, Sarawak)")
ll.Neofelis_diardi <- geocode(Neofelis_diardi)

Prionailurus_rubiginosus <- c("India", " Sri Lanka")
ll.Prionailurus_rubiginosus <- geocode(Prionailurus_rubiginosus)

Leopardus_guigna <- c("Argentina", " Chile")
ll.Leopardus_guigna <- geocode(Leopardus_guigna)

Pardofelis_marmorata <- c("Bhutan", " Brunei Darussalam", " Cambodia", " China", " India", " Indonesia (Kalimantan, Sumatera)", " Lao People's Democratic Republic", " Malaysia (Peninsular Malaysia, Sabah, Sarawak)", " Myanmar", " Nepal", " Thailand", " Viet Nam")
ll.Pardofelis_marmorata <- geocode(Pardofelis_marmorata)

Panthera_leo <- c("Angola (Angola)", " Benin", " Botswana", " Burkina Faso", " Cameroon", " Central African Republic", " Chad", " Congo, The Democratic Republic of the", " CÃ´te d'Ivoire", " Ethiopia", " Ghana", " Guinea", " Guinea-Bissau", " India", " Kenya", " Malawi", " Mali", " Mozambique", " Namibia", " Niger", " Nigeria", " Rwanda", " Senegal", " Somalia", " South Africa", " South Sudan", " Swaziland", " Tanzania, United Republic of", " Uganda", " Zambia", " Zimbabwe")
ll.Panthera_leo <- geocode(Panthera_leo)

Felis_nigripes <- c("Angola (Angola)", " Botswana", " Namibia", " South Africa", " Zimbabwe")
ll.Felis_nigripes <- geocode(Felis_nigripes)

Neofelis_nebulosa <- c("Bangladesh", " Bhutan", " Cambodia", " China", " India", " Lao People's Democratic Republic", " Malaysia (Peninsular Malaysia)", " Myanmar", " Nepal", " Thailand", " Viet Nam")
ll.Neofelis_nebulosa <- geocode(Neofelis_nebulosa)


ggplot(bbox_df, aes(x=long,y=lat, group=group)) +
  geom_polygon(fill="#58D3F7") +
  geom_polygon(data=wmap_df, aes(x=long,y=lat, group=group, fill=hole)) + 
  geom_path(data=countries_df, aes(x=long,y=lat, group=group, fill=hole), color="#0B610B")+
  geom_polygon(data=urban_df, aes(x=long,y=lat, group=group, fill=hole), color="#BDBDBD", border="#848484", lwd=1.0, add=TRUE, alpha=I(2/10)) + 
  geom_point(data=ll.Acinonyx_jubatus, aes(x=lon,y=lat, group=NULL), color="#380B61", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Leopardus_tigrinus, aes(x=lon,y=lat, group=NULL), color="#7401DF", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Neofelis_diardi, aes(x=lon,y=lat, group=NULL), color="#29088A", lwd=5.0,alpha = 0.5 ) +
  geom_point(data=ll.Prionailurus_rubiginosus, aes(x=lon,y=lat, group=NULL), color="#4000FF", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Leopardus_guigna, aes(x=lon,y=lat, group=NULL), color="#9A2EFE", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Pardofelis_marmorata, aes(x=lon,y=lat, group=NULL), color="#8181F7", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Panthera_leo, aes(x=lon,y=lat, group=NULL), color="#BE81F7", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Felis_nigripes, aes(x=lon,y=lat, group=NULL), color="#29088A", lwd=5.0,alpha = 0.5) +
  geom_point(data=ll.Neofelis_nebulosa , aes(x=lon,y=lat, group=NULL), color="#4000FF", lwd=5.0,alpha = 0.5) +
  geom_path(data=grat_df, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50") +
  labs(title="Vulnerable Felidae Species") + 
  coord_equal() + 
  theme_opts +
  scale_fill_manual(values=c("#04B404", "#58D3F7"), guide="none")

###Endangered geocode and map
Leopardus_jacobita <-c("Argentina", " Bolivia, Plurinational States of", " Chile", " Peru")
ll.Leopardus_jacobita <- geocode(Leopardus_jacobita)

Panthera_uncia <-c("Afghanistan", " Bhutan", " China (Gansu, Qinghai, Sichuan, Tibet [or Xizang], Xinjiang, Yunnan - Regionally Extinct)", " India (Himachal Pradesh, Jammu-Kashmir, Sikkim, Uttaranchal)", " Kazakhstan", " Kyrgyzstan", " Mongolia", " Nepal", " Pakistan", " Russian Federation", " Tajikistan", " Uzbekistan")
ll.Panthera_uncia <- geocode(Panthera_uncia)

Prionailurus_viverrinus <-c("Bangladesh", " Bhutan", " Cambodia", " India", " Indonesia (Jawa)", " Lao People's Democratic Republic", " Myanmar", " Nepal", " Sri Lanka", " Thailand", " Viet Nam")
ll.Prionailurus_viverrinus <- geocode(Prionailurus_viverrinus)

Panthera_tigris <-c("Bangladesh", " Bhutan", " Cambodia", " China (Anhui - Regionally Extinct, Beijing - Regionally Extinct, Chongqing - Regionally Extinct, Fujian - Possibly Extinct, Guangdong - Possibly Extinct, Guangxi - Regionally Extinct, Guizhou - Regionally Extinct, Hebei - Regionally Extinct, Heilongjiang, Henan - Regionally Extinct, Hubei - Regionally Extinct, Hunan - Possibly Extinct, Jiangsu - Regionally Extinct, Jiangxi - Possibly Extinct, Jilin, Liaoning - Regionally Extinct, Shaanxi - Possibly Extinct, Shandong - Regionally Extinct, Shanghai - Regionally Extinct, Shanxi - Regionally Extinct, Sichuan - Regionally Extinct, Tianjin - Regionally Extinct, Tibet [or Xizang], Xinjiang - Regionally Extinct, Yunnan, Zhejiang - Possibly Extinct)", " India", " Indonesia (Bali - Regionally Extinct, Jawa - Regionally Extinct, Sumatera)", " Lao People's Democratic Republic", " Malaysia (Peninsular Malaysia)", " Myanmar", " Nepal", " Russian Federation", " Thailand", " Viet Nam")
ll.Panthera_tigris <- geocode(Panthera_tigris)

Catopuma_badia <- c("Indonesia (Kalimantan)", " Malaysia (Sabah, Sarawak)")
ll.Catopuma_badia <- geocode(Catopuma_badia)

ggplot(bbox_df, aes(x=long,y=lat, group=group)) +
  geom_polygon(fill="#58D3F7") +
  geom_polygon(data=wmap_df, aes(x=long,y=lat, group=group, fill=hole)) + 
  geom_path(data=countries_df, aes(x=long,y=lat, group=group, fill=hole), color="#0B610B")+
  geom_polygon(data=urban_df, aes(x=long,y=lat, group=group, fill=hole), color="#BDBDBD", border="#848484", lwd=1.0, add=TRUE, alpha=I(2/10)) + 
  geom_point(data=ll.Leopardus_jacobita, aes(x=lon,y=lat, group=NULL), color="#F3F781", lwd=5.0,alpha = 0.7) +
  geom_point(data=ll.Panthera_uncia, aes(x=lon,y=lat, group=NULL), color="#FFBF00", lwd=5.0,alpha = 0.7) +
  geom_point(data=ll.Prionailurus_viverrinus, aes(x=lon,y=lat, group=NULL), color="#FAAC58", lwd=5.0,alpha = 0.7 ) +
  geom_point(data=ll.Panthera_tigris, aes(x=lon,y=lat, group=NULL), color="#F2F5A9", lwd=5.0,alpha = 0.7) +
  geom_point(data=ll.Catopuma_badia, aes(x=lon,y=lat, group=NULL), color="#F5D0A9", lwd=5.0,alpha = 0.7) +
  geom_path(data=grat_df, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50") +
  labs(title="Endangered Felidae Species") + 
  coord_equal() + 
  scale_fill_manual(values=c("#04B404", "#58D3F7"), guide="none")
