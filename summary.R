#--------------------------------------------------------------
#Ben Neely
#04/30/2025
#Investigate urban Channel Catfish stockings in Kansas
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

if("patchwork" %in% rownames(installed.packages()) == FALSE) {install.packages("patchwork")}
library(patchwork)

if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
library(tidyverse)

## Set ggplot theme
pubtheme=theme_classic()+
  theme(panel.grid=element_blank(), 
        panel.background=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(fill="transparent"),
        axis.title=element_text(size=16,color="black",face="bold"),
        axis.text=element_text(size=12,color="black"),
        legend.position.inside=c(0.01,0.01),
        legend.justification=c("left","bottom"),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background=element_rect(fill="transparent"))
options(scipen=999)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Desktop/2025 CCF analyses/TKAS analyses")

## Read in data with import and expand rows so each fish gets a line
dat=import("creelfish.csv")%>%
  expandCounts(~group_count)

## Add year and clean up data columns
dat1=dat%>%
  mutate(yr=year(sample_date),
         marked=case_when(species_subgroup != "" ~ 1,
                          TRUE ~ 0))%>%
  select(id=interview_id,
         code=impoundment_code,
         impd=impoundment,
         sdate=sample_date,
         yr,
         spp=species,
         spp_grp=species_subgroup,
         marked,
         tl=length,
         kr=keep_release,
         hrs=hours_fishing,
         angs=number_of_anglers,
         pref_spp=preferred_species,
         zip=primary_angler_zipcode)

## Total number of interviews
length(unique(dat1$id))
#1473 angling parties

## Total number of anglers
tmp=dat1%>%
  group_by(id)%>%
  slice(1)%>%
  ungroup
sum(tmp$angs)
#2471 anglers

## Proportion of anglers targeting catfish or anything
nrow(subset(tmp,pref_spp=="Blue Catfish" | pref_spp=="Channel Catfish" |
              pref_spp=="Catfish" | pref_spp=="***No Fish***"))
#821 angling parties targeting catfish or anything
821/1473
#56% of effort

## Total number of Channel Catfish caught
nrow(filter(dat1,spp=="Channel Catfish"))
#513 Channel Catfish observed

## Total number of marked Channel Catfish caught
nrow(filter(dat1,spp=="Channel Catfish" & marked==1))
#211 Marked Channel Catfish

## Percentage of marked
211/513

## Harvested and released Channel Catfish
xtabs(~kr,subset(dat1,spp=="Channel Catfish"))
# 219 released and 294 harvested
294/513
#57% harvest

## Total estimated catch at each impd
tmp=bind_cols(tibble(impd=rep(c("Blackbird","Sunflower","Horseshoe","Vics"),2)),
              tibble(yr=c(rep(2023,4),rep(2024,4))),
              tibble(catch=c(1248,892,1203,183,195,1376,582,1921)))

mean(tmp$catch)
sd(tmp$catch)
     