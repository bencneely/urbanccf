#--------------------------------------------------------------
#Ben Neely
#05/08/2025
#Catch timing and proportion harvested per stocking
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
        legend.position=c(0.99,0.99),
        legend.justification=c("right","top"),
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
  mutate(yr=year(sample_date))%>%
  select(id=interview_id,
         code=impoundment_code,
         impd=impoundment,
         sdate=sample_date,
         yr,
         spp=species,
         spp_grp=species_subgroup,
         tl=length,
         kr=keep_release,
         hrs=hours_fishing,
         angs=number_of_anglers,
         pref_spp=preferred_species,
         zip=primary_angler_zipcode)

################################################################################
## Harvest and release trends
dat9=dat1%>%
  filter(spp=="Channel Catfish")

## Proportion of Channel Catfish released and harvested
prop.table(xtabs(~kr,dat9))

## Proportion harvested at each impoundment regardless of stocking year
bb2=filter(dat9,code=="WICP")
prop.table(xtabs(yr~kr,bb2))

sf2=filter(dat9,code=="WICQ")
prop.table(xtabs(yr~kr,sf2))

hs2=filter(dat9,code=="SGCH")
prop.table(xtabs(yr~kr,hs2))

vl2=filter(dat9,code=="SGCG")
prop.table(xtabs(yr~kr,vl2))

################################################################################
## Mean length and length range of harvested and released fish
dat8=dat1%>%
  filter(spp=="Channel Catfish")%>%
  drop_na(tl)

## Range of harvested lengths
mean(subset(dat8,kr==1)$tl)
sd(subset(dat8,kr==1)$tl)
range(subset(dat8,kr==1)$tl)

## Range of released lengths
mean(subset(dat8,kr==0)$tl)
sd(subset(dat8,kr==0)$tl)
range(subset(dat8,kr==0)$tl)

################################################################################
## Length frequency of harvested and released fish
## Manipulate data to add a fish length in mm randomly selected from inch group
## Note that there are 37 instances of captured Channel Catfish having no TL
## Also note that this analysis includes Channel Catfish captured by anglers targeting other species
## Look at number released and harvested for figure legend
xtabs(~kr,dat8)

## KS test
ks.test(tl~factor(kr),dat8)
#D=0.179, P=0.001

## Location of D
x=subset(dat8,kr==0)$tl               #Isolate tl values associated with release
y=subset(dat8,kr==1)$tl               #Isolate tl values associated with harvest
ecdf_x=ecdf(x)                        #Create ECDFs
ecdf_y=ecdf(y)                        #Create ECDFs
grid=sort(unique(c(x,y)))             #Sorted grid of combined values
diffs=abs(ecdf_x(grid)-ecdf_y(grid))  #Look for differences at each tl between ECDFs
(d_loc=grid[which.max(diffs)])        #tl where difference is maximized
#Dloc=14

## Create English plot
ggplot(dat8,aes(x=tl,fill=factor(kr)))+
  geom_histogram(position=position_dodge(width=0.9),
                 binwidth=1)+
  scale_fill_manual(values=c("gray","black"),
                    labels=c("Release: N = 185","Harvest: N = 291"),
                    name="")+
  scale_y_continuous(limits=c(0,80.1),
                     breaks=seq(0,80,10),
                     name="Count",
                     expand=c(0,0))+
  scale_x_continuous(breaks=seq(4,23,1),
                     name="Total length (in)")+
  annotate("text",x=18.7,y=65.5,label="italic(P) == 0.001",parse=T,hjust=0,vjust=1,size=4.5)+
  annotate("text",x=18.7,y=61.5,label="italic(D) == 0.179",parse=T,hjust=0,vjust=1,size=4.5)+
  annotate("text",x=18.7,y=57.5,label="italic(D)[loc] == '14 in'",parse=T,hjust=0,vjust=1,size=4.5)+
  pubtheme

ggsave(plot=last_plot(),"catch timing and harvest patterns/lf_harv.png",width=8,height=4,bg="white")

################################################################################
## When is the best time to stock?
## Only keep data from the year of stocking and harvested Channel Catfish
## We only keep harvested here because of uncertainty with angler observations
dat2=dat1%>%
  mutate(use=case_when(code=="WICP" & yr==2023 ~ 1,
                       code=="WICQ" & yr==2024 ~ 1,
                       code=="SGCH" & yr==2023 ~ 1,
                       code=="SGCG" & yr==2024 ~ 1,
                       TRUE ~ 0))%>%
  filter(kr==1,
         use==1,
         spp=="Channel Catfish")

## Look at number of each fin clip observed
dat3=as_tibble(as.data.frame(xtabs(~code+spp_grp,dat2)))%>%
  mutate(spp_grp=case_when(spp_grp=="" ~ "none",
                           spp_grp=="Adipose clip" ~ "adipose",
                           spp_grp=="Lower caudal" ~ "lower",
                           spp_grp=="Upper caudal" ~ "upper",
                           spp_grp=="Left pectoral" ~ "left",
                           spp_grp=="Right pectoral" ~ "right"))%>%
  pivot_wider(names_from=spp_grp,values_from=Freq)%>%
  select(code,none,adipose,lower,upper,left,right)

## Add columns for number stocked with each clip
dat4=dat3%>%
  mutate(adipose_st=c(399,400,200,200),
         lower_st=c(399,466,200,200),
         upper_st=c(532,599,200,200),
         left_st=c(532,599,200,200),
         right_st=c(333,333,200,200))

## Add proportion of harvest from each stocking
dat5=dat4%>%
  mutate(adipose_prop=adipose/adipose_st,
         lower_prop=lower/lower_st,
         upper_prop=upper/upper_st,
         left_prop=left/left_st,
         right_prop=right/right_st)

## Plot proportion of each stocking that was harvested
dat6=dat5%>%
  select(code,adipose_prop,lower_prop,upper_prop,left_prop,right_prop)%>%
  pivot_longer(cols=adipose_prop:right_prop,names_to="clip")%>%
  mutate(stock_day=c(106,132,162,239,274,
                     103,131,159,254,272,
                     103,131,159,254,272,
                     106,132,162,239,274),
         code=factor(code,levels=c("WICP","WICQ","SGCH","SGCG")))

ggplot(dat6)+
  geom_point(aes(x=stock_day,y=value,color=code),size=6)+
  geom_line(aes(x=stock_day,y=value,color=code),linewidth=1.5)+
  scale_color_manual(values=c("#B12A90","#F0F921","#F1605D","#440154"),
                     labels=c("Blackbird (stocked 2023)",
                              "Sunflower (stocked 2024)",
                              "Horseshoe (stocked 2023)",
                              "Vic's (stocked 2024)"))+
  scale_x_continuous(limits=c(97,283),
                     breaks=seq(100,280,10),
                     name="Day of year",
                     expand=c(0,0))+
  scale_y_continuous(limits=c(0,0.145),
                     breaks=seq(0,0.14,0.02),
                     name="Observed proportion of stocked fish",
                     expand=c(0,0))+
  pubtheme

ggsave(plot=last_plot(),"catch timing and harvest patterns/stockcatch.png",height=4,width=8,units="in",bg="white")

################################################################################
## Look at harvest/day at large
dat7=dat2%>%
  mutate(syear=case_when(code=="WICP" | code=="SGCH" ~ 2023,
                         code=="WICQ" | code=="SGCG" ~ 2024),
         
         sday=case_when(code=="WICP" & spp_grp=="Adipose clip" ~ 103,
                        code=="WICP" & spp_grp=="Lower caudal" ~ 131,
                        code=="WICP" & spp_grp=="Upper caudal" ~ 159,
                        code=="WICP" & spp_grp=="Left pectoral" ~ 254,
                        code=="WICP" & spp_grp=="Right pectoral" ~ 272,
           
                        code=="SGCH" & spp_grp=="Adipose clip" ~ 103,
                        code=="SGCH" & spp_grp=="Lower caudal" ~ 131,
                        code=="SGCH" & spp_grp=="Upper caudal" ~ 159,
                        code=="SGCH" & spp_grp=="Left pectoral" ~ 254,
                        code=="SGCH" & spp_grp=="Right pectoral" ~ 272,
                        
                        code=="WICQ" & spp_grp=="Adipose clip" ~ 106,
                        code=="WICQ" & spp_grp=="Lower caudal" ~ 132,
                        code=="WICQ" & spp_grp=="Upper caudal" ~ 162,
                        code=="WICQ" & spp_grp=="Left pectoral" ~ 239,
                        code=="WICQ" & spp_grp=="Right pectoral" ~ 274,
                        
                        code=="SGCG" & spp_grp=="Adipose clip" ~ 106,
                        code=="SGCG" & spp_grp=="Lower caudal" ~ 132,
                        code=="SGCG" & spp_grp=="Upper caudal" ~ 162,
                        code=="SGCG" & spp_grp=="Left pectoral" ~ 239,
                        code=="SGCG" & spp_grp=="Right pectoral" ~ 274),
         
         cday=yday(sdate),
         dal=cday-sday)%>%
  filter(dal>=0)%>%
  select(code,yr,spp,spp_grp,tl,syear,dal)

## Look at when fish were caught
ggplot(dat7)+
  geom_bar(aes(x=dal,fill=code),
           width=1.2)+
  stat_ecdf(aes(x=dal,y=after_stat(ecdf)/0.091),linewidth=1.1,alpha=0.25)+
  scale_fill_manual(values=c("#B12A90","#F0F921","#F1605D","#440154"),
                    labels=c("Blackbird (stocked 2023)",
                             "Sunflower (stocked 2024)",
                             "Horseshoe (stocked 2023)",
                             "Vic's (stocked 2024)"))+
  scale_y_continuous(name="Number harvested",
                     breaks=seq(0,11.1,1),
                     sec.axis=sec_axis(~.*0.091,
                                       breaks=seq(0,1,0.1),
                                       name="Cumulative distribution"))+
  scale_x_continuous(name="Days at large",
                     breaks=seq(0,200,20))+
  coord_cartesian(xlim=c(-1,201),
                  ylim=c(0,11.1),
                  expand=F)+
  pubtheme+
  theme(legend.position=c(0.99,0.655))

ggsave(plot=last_plot(),"catch timing and harvest patterns/cumfreq.png",width=8,height=4,bg="white")

