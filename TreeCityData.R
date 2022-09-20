library(dplyr)
library(readr)
library(rvest)
library(tidyr)
library(stringr)

###Missouri City Geoid###
city<-read_html("http://mcdc.missouri.edu/applications/geocodes/?state=29#places")%>%
        html_nodes(css = "#places+ .geoList p")%>%
        html_text()%>%
        as_tibble()
city<- separate(city, value, sep = "\\. ", into = c("PLACE", "BASENAME"))
city<- separate(city, BASENAME, sep = ", ", into = c("BASENAME", "STATE"))
city<- separate(city, STATE, sep = " ", into = c("STATE", "POPULATION"))
city$STATE<-as.character(29)
city$BASENAME<-gsub(pattern = " city$", replacement = "", x = city$BASENAME)
city$BASENAME<-gsub(pattern = " village$", replacement = "", x = city$BASENAME)
city$BASENAME<-gsub(pattern = " town$", replacement = "", x = city$BASENAME)
city$BASENAME<-gsub(pattern = " CDP$", replacement = "", x = city$BASENAME)
city$POPULATION<-gsub(pattern = "[\\(\\)]", replacement = "", x = city$POPULATION)
city$POPULATION<-gsub(pattern = "\\,", replacement = "", x= city$POPULATION)
city$POPULATION<-as.numeric(city$POPULATION)
city$GEOID<- paste(city$STATE, city$PLACE, sep = "")
city<-city[which(!is.na(city$POPULATION)),]
city$BASENAME[128]<-"Bull Creek Village"

###Tree City USA Data###
trees<-read_html("https://www.arborday.org/programs/treecityusa/treecities.cfm?chosenstate=Missouri")
mocities<-html_nodes(x = trees, css = "tr > :nth-child(1)")%>%
        html_text()%>%
        as_tibble()
moyears<-html_nodes(x = trees, css = "tr > :nth-child(2)")%>%
        html_text()%>%
        as_tibble()
mopop<-html_nodes(x = trees, css = "tr > :nth-child(3)")%>%
        html_text()%>%
        as_tibble
arbor<-bind_cols(mocities, moyears, mopop)%>%
        rename(BASENAME = value...1)%>%
        rename(YEARS = value...2)%>%
        rename(POPULATION = value...3)
arbor<-arbor[-1,]
arbor$YEARS<-as.integer(arbor$YEARS)
arbor<-arbor[,-3]
arbor$TreeCity[1]<-1
arbor$BASENAME<-gsub("St ", "Saint ", x = arbor$BASENAME)
arbor$BASENAME[91]<-"Saint James"
arbor$BASENAME[73]<-"Peculiar"
arbor$BASENAME[35]<-"Glen Echo Park"
arbor$BASENAME[67]<-"O Fallon"

###Median Single Family Homes Data###
con = file("City_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_mon.csv", "r")
chunk <- read.csv(file = con)
mohouse<- chunk[chunk$State == 'MO',]
close.connection(con)
rm(chunk)
mohouse<-mohouse[,c("RegionName", "RegionType", "Metro", "CountyName", "X2020.01.31")]
mohouse$BASENAME<-mohouse$RegionName
mohouse$BASENAME[which(mohouse$BASENAME=="Bellrive")]<-"Bellrive Acres"


###Joining median house price with tree cities###
moTreeWealth<-full_join(arbor, mohouse)
moTreeWealth$TreeCity[111:1009]<-0
factor(moTreeWealth$TreeCity)
moTreeWealth<-full_join(moTreeWealth, city, by = "BASENAME")
geoID<-filter(moTreeWealth, is.na(GEOID) == FALSE & is.na(RegionType) == FALSE )

###Write Data###
write_csv(moTreeWealth, path = "Rich-Tree-City.csv" )
write_csv(geoID, path = "geoID_Rich-Tree-City.csv")

