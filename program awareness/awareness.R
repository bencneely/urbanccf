#--------------------------------------------------------------
#Ben Neely
#05/08/2025
#Program awareness -- Using pre-summarized data
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
        axis.title=element_text(size=14,color="black",face="bold"),
        axis.text=element_text(size=12,color="black"),
        legend.position=c(0.01,0.99),
        legend.justification=c("left","top"),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.background=element_rect(fill="transparent"))
options(scipen=999)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Desktop/2025 CCF analyses/TKAS analyses")

## Read in data with import and turn program and awareness into factors
dat=import("program awareness/awareness.csv")%>%
  mutate(program=factor(program),
         awareness=factor(awareness),
         proportion=as.numeric(proportion))

## Create barplot
ggplot(dat,aes(x=program,y=proportion,fill=awareness))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values=c("gray","black"))+
  scale_x_discrete(labels=c("Community Fisheries Assistance Program"="Community Fisheries\nAssistance Program",
                            "Stocking Channel Catfish"="Stocking\nChannel Catfish",
                            "Urban Fishing Program"="Urban Fishing\nProgram"),
                   name="")+
  scale_y_continuous(limits=c(0,60),
                     breaks=seq(0,60,10),
                     labels=scales::label_percent(scale=1),
                     name="Percentage of respondents",
                     expand=c(0,0))+
  pubtheme

ggsave(plot=last_plot(),"program awareness/awareness.png",width=8,height=5,bg="white")
