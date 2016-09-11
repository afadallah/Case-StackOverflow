################################################################################################
#### Loading Packages
################################################################################################
library(stringr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(jsonlite, warn.conflicts = FALSE)
library(rgeos, warn.conflicts = FALSE)
library(sp, warn.conflicts = FALSE)
library(maptools, warn.conflicts = FALSE)
library(ggplot2) 
library(ggrepel) 
library(seriation) 
library(broom)

################################################################################################
### Extracting Data
################################################################################################

setwd("d:/data/AFA21296/Desktop/Data Science/0.DataR/navara-case/")

sonl <- fromJSON("SONL.txt")
skills <- fromJSON("SONL_skills.txt")


################################################################################################
### Cleaning Data
################################################################################################

sonl$creation_date = sonl$creation_date %>% as.POSIXlt(origin = "1970-01-01") %>% as.character()
sonl$last_access_date = sonl$last_access_date  %>% as.POSIXlt(origin = "1970-01-01") %>% as.character()
sonl$last_modified_date = sonl$last_modified_date  %>% as.POSIXlt(origin = "1970-01-01") %>% as.character()
sonl$Plaatsen = sonl$location %>% tolower() %>% str_replace_all("the netherlands","") %>% str_replace_all("netherlands","") %>% str_trim() %>%
  str_replace_all("[&,|:#;/-]"," ") %>% str_replace_all("[&,|:#;/-]"," ") %>%  str_replace_all("[&,|:#;/-]"," ") %>% str_replace_all("[&,|:#;/-]"," ") %>% 
  str_replace_all("[&,|:#;/-]"," ") %>% str_replace_all("[,]"," ") %>% str_replace_all("[\\.]"," ") %>% str_replace_all("the hague","den haag") %>% 
  str_replace_all("ijsselsteyn", "ijsselstein") %>%  str_replace_all("zeeewolde","zeewolde") %>% str_replace_all("antilles    sweden  france", "antillen") %>%
  str_replace_all("zuid holland", "rotterdam") %>% str_replace_all("gelderland", "arnhem") %>% str_replace_all("noord holland", "amsterdam") %>%
  str_replace_all("north holland", "amsterdam") %>% str_replace_all("noord holland", "amsterdam")  %>%str_replace_all("pynacker", "pijnacker") %>%
  str_replace_all("nymegen", "nijmegen") %>% str_replace_all("nederland", "") %>%  str_replace_all("elst arnhem", "elst") %>%  str_replace_all("curacao   antilles", "antillen") %>% 
  str_replace_all("noord braband", "'s hertogenbosch") %>% str_replace_all("north holland", "amsterdam") %>% str_replace_all("drenthe", "assen") %>% str_replace_all("brazil", "") %>% str_replace_all("amp", "") %>%
  str_replace_all("noord holland", "amsterdam") %>% str_replace_all("south holland", "rotterdam") %>% str_replace_all("province zuid holland", "rotterdam") %>%   str_replace_all("antilles", "antillen") %>%
  str_replace_all("overijsel", "lelystad") %>% str_replace_all("leiden  rotterdam","rotterdam") %>% str_replace_all("haarlem amsterdam","amsterdam") %>% str_replace_all("utrecht amsterdam","amsterdam") %>%
  str_replace_all("venlo  maastricht","maastricht") %>% str_replace_all("s-hertogenbosch","'s hertogenbosch") %>% str_replace_all("s hertogenbosch","'s hertogenbosch") %>% str_replace_all("'s hertogenbosch","'s hertogenbosch") %>%
  str_replace_all("gelderland|gem  asten|'|royal kingdom of|kingdom of|\\(|\\)|israel|overijsel|(overijssel)|playa del carmen  mexico|z  252 rich  switzerland|wilgestraat 9  4431 cj    39|washington dc usa and|and canterlot castle  equestria|limburg|the|usa|uk  south africa  amp|uk|the stronghold of|the royal kingdom of|sweet lake city|south holland |south african living in|slovakia|somewhere in|sko|spain|area|province  zuid holland|portugal|or south africa|new|near|madrid  spain|haven|south holland|(overijsel)|europe|germany and|germany amp|fom institute amolf|or nijmegen|earth  milky way|earth  europe    limburg|discworld|university of technology|cura  231 ao  antilles|close to |noord brabant|berlin  germany|spain  sweden  france|st maarten|and  malaysia|127799   127799;&#127799;|  friesland|39|(hallo alllemaal!  d)|(bennekom)|the kingdom of|noord brabant|limburg|frysl  226 n|north holland|north brabant|germany|drenthe|friesland|127799   127799   127799|sweden  france", "")   %>%
  str_trim() %>% as.factor()

sonl$badge_counts_gold = sonl$badge_counts$gold
sonl$badge_counts_silver = sonl$badge_counts$silver
sonl$badge_counts_bronze = sonl$badge_counts$bronze
sonl$badge_counts <-NULL



################################################################################################
### Stack Overflow Users 
################################################################################################  

# get a ranking of dutch cities that have most stack overflow users
Plaatsen<- table(sonl$Plaatsen) %>% as.data.frame() %>% arrange(desc(Freq))
names(Plaatsen)[1]<-"City"
Plaatsen
head(Plaatsen)

source('D:/Data/AFA21296/Desktop/Data Science/1. ExtractR/Extract_html_Wikipedia.R')
df_PGC = getExternalData()
df_PGC$Plaatsen = df_PGC$Plaatsen %>% tolower()

sonl$ExactMatch = ""
sonl$ExactMatch = ifelse(sonl$Plaatsen %in% df_PGC$Plaatsen, "Match Found", "No Match Found")
#test = stringdist_full_join(sonl,df_PGC,max_dist=2)

sonl_in<-inner_join(sonl,df_PGC)
sonl_out<-anti_join(sonl,df_PGC)
sonl_in <- sonl_in %>%  filter(Provincien!="NA")

# sonl_in$Provincies = sonl_in$Provincies %>% str_replace_all("Fryslân","Friesland") %>% str_replace_all("Drenthe, Groningen","Drenthe") %>%
#   str_replace_all("Drenthe Groningen","Drenthe") %>% str_replace_all("Drenthe, Overijssel","Drenthe") %>% 
#   str_replace_all("Friesland (Fryslân)","Friesland") %>% str_replace_all("provincie","") %>% str_replace_all("\\(","") %>% str_replace_all("\\)","") %>%
#   str_replace_all("Limburg/ Gelderland","Limburg") %>% str_replace_all("Noord-Brabant en Antwerpen","Noord-Brabant") %>%
#   str_replace_all("Antwerpen","Noord-Brabant") %>% str_replace_all("Utrecht /  Zuid-Holland","Utrecht") %>%
#   str_replace_all("Zuid-Holland,  Gelderland en  Utrecht","Gelderland") %>% str_trim()


################################################################################################
### Univariate Analysis 
################################################################################################  

### Temporal Analysis 

sonl_in$creationYear = year(sonl_in$creation_date)
sonl_in$creationYear <- as.factor(sonl_in$creationYear)
table(sonl_in$creationYear)

sonl_in$creationMonth = month(sonl_in$creation_date)
sonl_in$creationMonth <- as.factor(sonl_in$creationMonth)
table(sonl_in$creationMonth)

sonl_in$creationSeason = sonl_in$creationMonth %>% as.character() %>% as.numeric()
sonl_in$creationSeason = cut(sonl_in$creationSeason,c(1,2,5,8,11,12),labels = c("Winter","Spring","Summer","Autumn","Winter2"))
sonl_in$creationSeason <- sonl_in$creationSeason %>% str_replace_all("Winter2","Winter") %>% as.factor()
table(sonl_in$creationSeason)

sonl_in$creationDay = wday(sonl_in$creation_date,label = TRUE, abbr = TRUE)
sonl_in$creationDay <- as.factor(sonl_in$creationDay)
table(sonl_in$creationDay)



### Spatial Analysis 

#download the shapefile from: http://download.geofabrik.de/europe/netherlands.html
# for dowcumentation, see http://download.geofabrik.de/osm-data-in-gis-formats-free.pdf
setwd("D:/data/AFA21296/Downloads/")
dir.create(file.path("Shapefiles"),showWarnings = FALSE)
download.file("http://www.imergis.nl/shp/Bestuurlijkegrenzen-provincies-actueel-shp.zip")
file.copy(from = str_c(getwd(),"/Bestuurlijkegrenzen-provincies-actueel-shp.zip"), to = str_c(getwd(),"/Shapefiles/Bestuurlijkegrenzen-provincies-actueel-shp.zip"))
setwd("D:/data/AFA21296/Downloads/Shapefiles")
unzip("Bestuurlijkegrenzen-provincies-actueel-shp.zip")


ShpFile=readShapePoly("TopGrenzen-prov-actueel.shp")
ShpProv = fortify(gBuffer(ShpFile,byid = TRUE, width = 0), region = "Provincien")
ShpFile@data$id = ShpFile@data$Provincien
# stat_sonl = sonl_in %>% group_by(Provincien) %>% summarise(aantal =n()) %>% as.data.frame()
# stat_sonl$Provincien = stat_sonl$Provincien %>% as.character()
# ShpFile@data$Provincien = ShpFile@data$Provincien %>% as.character()
# ShpFile@data = inner_join(ShpFile@data,stat_sonl)


nl_df = inner_join(ShpFile@data,ShpProv, by = "id")
           
# Provinties_centroids = group_by(nl_df, id) %>% summarize(long = mean(long), lat = mean(lat))

TopITBedrijven = getMultinationals()
source('D:/Data/AFA21296/Desktop/Data Science/1. ExtractR/Extract_json_GoogleMaps.R')

address <- TopITBedrijven
locations <- ldply(address, function(x) geoCode(x))
names(locations) <- c("lat","lon","location_type", "forAddress")

ggplot(nl_df) + 
  aes(long, lat, fill = id) + 
  geom_polygon() +
  geom_path(color = "white") +
  coord_equal() 
  

# ShpFile=readShapePoly("gis.osm_landuse_a_free_1.shx")
# ShpFile=readShapePoly("gis.osm_buildings_a_free_1.shp")
# ShpFile=readShapePoly("gis.osm_natural_a_free_1.shp")
# ShpFile=readShapePoly("gis.osm_pofw_a_free_1.shp")
# ShpFile=readShapePoly("gis.osm_pois_a_free_1.shp")
# ShpFile=readShapePoly("gis.osm_railways_a_free_1.shp")
# ShpFile=readShapePoly("gis.osm_traffic_a_free_1.shp")
# ShpFile=readShapePoly("gis.osm_transport_a_free_1.shp")
# ShpFile=readShapePoly("gis.osm_water_a_free_1.shp")

sonl_in$Provincien = sonl_in$Provincien %>% as.factor()
t=table(sonl_in$Provincien) %>% as.data.frame() %>% arrange(desc(Freq)) 




### Tag Analysis 

skills$tag_name = skills$tag_name %>% as.factor()
tags1 = table(skills$tag_name) %>% as.data.frame() %>% arrange(desc(Freq)) %>% filter(Freq>=50) %>% filter(!(Var1 %in% c("arrays","string","forms", "performance", "image","multithreading","function","apache","oop","list","date","unit-testing","algorithm", "validation", "datetime", "file", "class", "variables", "facebook", "debugging", "if-statement", "loops", "ipad", "object", "user-interface","sorting", "exception", "parsing", "table", "firefox", "uitableview", "for-loop", "post", "generics", "math")))
tags2 = tags1 %>% filter(Var1 %in% c("javascript", "html", "sql", "python", "json", "html5", "regex", "xml", "linux", "css3", "sql-server", "twitter-bootstrap","bash", "api", "rest", "excel","shell","mongodb","postgresql","git", "r" ))
tags2$Var1 = tags2$Var1 %>% str_replace_all("sql-server","sql") %>% str_replace_all("html5","html") %>% str_replace_all("rest","api") %>% str_replace_all("shell","bash") %>% str_replace_all("postgresql","sql") %>% str_replace_all("twitter-","") 

TagFrequency = tags2 %>% group_by(Var1) %>% summarise(Freq=sum(Freq)) %>% arrange(desc(Freq)) %>% as.data.frame() %>% select(Technology = Var1, Frequency=Freq)

skills = skills %>%  filter(tag_name %in% c("javascript", "html", "sql", "python", "json", "html5", "regex", "xml", "linux", "css3", "sql-server", "twitter-bootstrap","bash", "api", "rest", "excel","shell","mongodb","postgresql","git", "r" )) 
skills$tag_name = skills$tag_name %>% str_replace_all("sql-server","sql") %>% str_replace_all("html5","html") %>% str_replace_all("rest","api") %>% str_replace_all("shell","bash") %>% str_replace_all("postgresql","sql") %>% str_replace_all("twitter-","") 

sub_sonl = sonl_in %>% select(user_id,user_type,account_id,display_name, is_employee, link, reputation,Provincien, Plaatsen, Coordinaten)

skillsNew = inner_join(skills,sub_sonl)
skillsNew$tag_name = skillsNew$tag_name %>% as.factor()
skillsNew$user_id = skillsNew$user_id %>% as.factor()
skillsNew$user_type = skillsNew$user_type %>% as.factor()
skillsNew$account_id = skillsNew$account_id %>% as.factor()
skillsNew$Plaatsen = skillsNew$Plaatsen %>% as.factor()
skillsNew$Coordinaten = skillsNew$Coordinaten %>% as.character()

ggplot(skillsNew, aes(Provincien, tag_name)) + 
  geom_tile(aes(fill = reputation), colour = "white") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "steelblue") + 
  coord_fixed(ratio = .9)

barplot(sub_sonl$reputation)
boxplot(reputation ~ tag_name, data=skillsNew, main="Repution across skills")
#id = which(skillsNew$reputation > 3*mean(skillsNew$reputation))
#text(id, skillsNew$reputation[id], skillsNew$display_name[id], pos = 1)


boxplot(reputation ~ tag_name, data=skillsNew[], main="Repution across skills")

cut_labels = c("Marinier 3e Klasse", "Marinier 2e Klasse", "Marinier 1ste Klasse", "Korporaal","Sergeant","Sergeant-Majoor","Adjundant","Luitenant", "Kapitein", "Majoor", "Kolonel", "Brigade Generaal", "Luitenant Generaal", "Generaal")
cut_values = 100*c(0,1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192)

skillsNew$Rank = cut(skillsNew$reputation, breaks = cut_values, labels = cut_labels) %>% as.factor()

PivotTable = table(skillsNew$rank) %>% as.data.frame() %>% select(Rank = Var1, Aantal = Freq)

RankedProvince = skillsNew %>% group_by(Provincien, Rank) %>% summarise(Aantal = n_distinct(user_id))
ggplot(RankedProvince, aes(Provincien, Rank)) + 
  geom_tile(aes(fill = Aantal), colour = "white") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "steelblue") + 
  geom_text(aes(label = sprintf('%d', Aantal)), size = 4.5) +
  coord_fixed(ratio = .9) +
  ggtitle("Spreiding van Stackoverflow leden over Rangen en Provincien")
ggsave("Sprdng_Stckvflw_ldn_Rangen_n_Provincies.jpg", width = 20, height = 20, units = "cm")

RankedTags = skillsNew %>% group_by(Rank,tag_name) %>% summarise(Aantal = n_distinct(user_id))
ggplot(RankedTags, aes(tag_name, Rank)) + 
  geom_tile(aes(fill = Aantal), colour = "white") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "red") + 
  geom_text(aes(label = sprintf('%d', Aantal)), size = 4.5) +
  coord_fixed(ratio = .9) +
  ggtitle("Spreiding van Stackoverflow leden over Rangen en Provincien")
ggsave("Sprdng_Stckvflw_ldn_Rangen_n_TechnologischeOnderwerpen.jpg", width = 20, height = 20, units = "cm")

ProvinceTags = skillsNew %>% group_by(Provincien,tag_name) %>% summarise(Aantal = n_distinct(user_id))
ggplot(ProvinceTags, aes(tag_name, Provincien)) + 
  geom_tile(aes(fill = Aantal), colour = "white") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "green") + 
  geom_text(aes(label = sprintf('%d', Aantal)), size = 4.5) +
  coord_fixed(ratio = .9) +
  ggtitle("Spreiding van Stackoverflow leden over de Technologische Onderwerpen en Provincien")
ggsave("Sprdng_Stckvflw_ldn_TechnologischeOnderwerpen_n_Provincies.jpg", width = 20, height = 20, units = "cm")


################################################################################################
### Bivariate Analysis 
################################################################################################  

### Relationships between Continuous variables



### Comparing groups: tables and visualizations


### Comparing groups: statistical tests and visualizations


### Identifying drivers of outcomes: linear regression modelling






################################################################################################
### Bivariate Analysis 
################################################################################################  







