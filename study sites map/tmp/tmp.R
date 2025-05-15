#--------------------------------------------------------------
#Ben Neely
#99/99/9999
#Description of what the script does
#--------------------------------------------------------------

## Clear R
cat("\014")  
rm(list=ls())

## Install and load packages
## Checks if package is installed, installs if not, activates for current session
if("FSA" %in% rownames(installed.packages()) == FALSE) {install.packages("FSA")}
library(FSA)

if("rio" %in% rownames(installed.packages()) == FALSE) {install.packages("rio")}
library(rio)

if("geosphere" %in% rownames(installed.packages()) == FALSE) {install.packages("geosphere")}
library(geosphere)

if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
library(tidyverse)

## Set ggplot theme
pubtheme=theme_classic()+
  theme(panel.grid=element_blank(), 
        panel.background=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(fill="transparent"),
        axis.title=element_text(size=22,color="black",face="bold"),
        axis.text=element_text(size=18,color="black"),
        legend.position="none")
options(scipen=999)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Desktop/CCF analyses/CCF GIS/")

## Read in data with import
loc=import("locdat.csv")
zips=import("zips.csv")

## Add home and site coordinates
## These coordinates are the center of zip code and center of lake
dat=loc%>%
  left_join(zips,by="zip")%>%
  mutate(site_long=case_when(code=="WICP" ~ -97.49692,
                             code=="WICQ" ~ -97.49454,
                             code=="SGCG" ~ -97.41857,
                             code=="SGCH" ~ -97.41349),
         site_lat=case_when(code=="WICP" ~ 37.75732,
                            code=="WICQ" ~ 37.75696,
                            code=="SGCG" ~ 37.72134,
                            code=="SGCH" ~ 37.72179),
         long=as.numeric(long),
         lat=as.numeric(lat),
         site_long=as.numeric(site_long),
         site_lat=as.numeric(site_lat))
         
## Calculate straight-line distance between home and lake for each record         
dat$dist_m=mapply(
  function(lon1,lat1,lon2,lat2){
    distHaversine(c(lon1,lat1),c(lon2,lat2))
  },
  dat$long,dat$lat,dat$site_long,dat$site_lat
)

## Convert to km
dat$dist_km=dat$dist/1000

## Export
export(dat,"out.csv")
