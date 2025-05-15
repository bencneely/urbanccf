#--------------------------------------------------------------
#Ben Neely
#03/04/2025
#Check out where Wichita urban anglers came from
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

if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
library(lubridate)

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
dat=import("out.csv")

## Clean up data a little bit to focus on catfish angling parties
dat1=dat%>%
  filter(pref %in% c("Catfish","Blue Catfish","Channel Catfish","***No Fish***"))%>%
  mutate(date=as.Date(date,format="%m/%d/%Y"),
         yr=factor(year(date)),
         code=factor(code))%>%
  group_by(code,yr,dist_km)%>%
  select(code,yr,dist_km)%>%
  drop_na()

###################################################################
## Define boxplot summary function
f=function(x) {
  r=quantile(x,probs=c(0.05,0.25,0.5,0.75,0.95))
  names(r)=c("ymin","lower","middle","upper","ymax")
  r
}
###################################################################

## Create plot
ggplot(dat1,aes(x=code,y=dist_km,fill=yr,color=yr))+
         stat_summary(fun.data=f,geom="boxplot",position=position_dodge(width=1.1))+
         geom_jitter(width=0.2,alpha=0.5)+
  scale_y_log10()
