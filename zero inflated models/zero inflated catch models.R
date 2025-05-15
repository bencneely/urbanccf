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

if("glmmTMB" %in% rownames(installed.packages()) == FALSE) {install.packages("glmmTMB")}
library(glmmTMB)

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
        legend.position=c(0.01,0.01),
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

## Identify anglers targeting catfish or anything
cftargs=c("Catfish","***No Fish***","Channel Catfish","Blue Catfish")

## Only retain anglers targeting catfish as specified above and add stocked status
dat2=dat1%>%
  filter(pref_spp %in% cftargs)%>%
  mutate(stocked=case_when(code=="SGCH" & yr==2023 ~ 1,
                           code=="SGCG" & yr==2024 ~ 1,
                           code=="WICP" & yr==2023 ~ 1,
                           code=="WICQ" & yr==2024 ~ 1,
                           TRUE ~ 0))

## Get angler hours for each fish record by multiplying number of anglers and hours
## Also convert year to a factor so the models don't blow up
dat3=dat2%>%
  mutate(anghrs=hrs*angs,
         yr=factor(yr))

## Combine all fishing trips by interview_id and count the number of CCF
## Ultimately, we're working toward CCF/angler hour for each interview_id (angling party)
dat4=dat3%>%
  mutate(ccf=case_when(spp=="Channel Catfish" ~ 1,
                       TRUE ~ 0))%>%
  group_by(id,code,yr,stocked,pref_spp,angs,hrs,anghrs)%>%
  summarize(tot_ccf=sum(ccf))%>%
  ungroup()%>%
  mutate(ccf_anghr=tot_ccf/anghrs)

################################################################################
################################################################################
## Blackbird
wicp=filter(dat4,code=="WICP")%>%
  select(id,code,ccf_anghr,yr)

## Zero-inflated gamma generalized linear model with a long link
## Model using glmmTMB package and gamma distribution with a log link for positive values
wicp_mod=glmmTMB(ccf_anghr~yr,data=wicp,family=ziGamma(link="log"),zi= ~ yr)
(wicp_modsum=summary(wicp_mod))
confint(wicp_mod)

## Extract parameter estimates for consistent calculations
## 2023 catch rate
wicp_cr_int_2023=wicp_modsum$coefficients$cond[1,"Estimate"]      #2023 conditional intercept
wicp_cr_int_se_2023=wicp_modsum$coefficients$cond[1,"Std. Error"] #2023 conditional intercept se
wicp_cr_int_lci_2023=wicp_cr_int_2023+(1.96*wicp_cr_int_se_2023)  #2023 conditional intercept lci
wicp_cr_int_uci_2023=wicp_cr_int_2023-(1.96*wicp_cr_int_se_2023)  #2023 conditional intercept uci

##2024 catch rate
wicp_cr_int_2024=wicp_modsum$coefficients$cond[1,"Estimate"]+
  wicp_modsum$coefficients$cond[2,"Estimate"]                     #2024 conditional intercept
wicp_cr_int_se_2024=wicp_modsum$coefficients$cond[2,"Std. Error"] #2024 conditional intercept se
wicp_cr_int_lci_2024=wicp_cr_int_2024+(1.96*wicp_cr_int_se_2024)  #2024 conditional intercept lci
wicp_cr_int_uci_2024=wicp_cr_int_2024-(1.96*wicp_cr_int_se_2024)  #2024 conditional intercept uci

## 2023 probability of zero catch
wicp_zi_int_2023=wicp_modsum$coefficients$zi[1,"Estimate"]        #2023 zero-inflated intercept
wicp_zi_int_se_2023=wicp_modsum$coefficients$zi[1,"Std. Error"]   #2023 zero-inflated intercept se
wicp_zi_int_lci_2023=wicp_zi_int_2023-(1.96*wicp_zi_int_se_2023)  #2023 zero-inflated intercept lci
wicp_zi_int_uci_2023=wicp_zi_int_2023+(1.96*wicp_zi_int_se_2023)  #2023 zero-inflated intercept uci

## 2024 probability of zero catch
wicp_zi_int_2024=wicp_modsum$coefficients$zi[1,"Estimate"]+
  wicp_modsum$coefficients$zi[2,"Estimate"]                       #2024 zero-inflated intercept
wicp_zi_int_se_2024=wicp_modsum$coefficients$zi[2,"Std. Error"]   #2024 zero-inflated intercept se
wicp_zi_int_lci_2024=wicp_zi_int_2024-(1.96*wicp_zi_int_se_2024)  #2024 zero-inflated intercept lci
wicp_zi_int_uci_2024=wicp_zi_int_2024+(1.96*wicp_zi_int_se_2024)  #2024 zero-inflated intercept uci

## Z-values and P-values for models
wicp_cr_z=wicp_modsum$coefficients$cond[2,"z value"]
wicp_cr_p=wicp_modsum$coefficients$cond[2,"Pr(>|z|)"]

wicp_zi_z=wicp_modsum$coefficients$zi[2,"z value"]
wicp_zi_p=wicp_modsum$coefficients$zi[2,"Pr(>|z|)"]

################################################################################
## First interpret the conditional model
## This is catch rate by anglers that actually caught fish

## 2023 (reference year) catch rate
## Since we are using the gamma distribution with a log link, the actual value is exp(intercept)
(wicp_cr_2023=exp(wicp_cr_int_2023))
#Expected catch rate in 2023 is 1.656/hr

## 2023 catch rate confidence interval
## Analogous to estimate +- 1.96(SE)
(wicp_cr_l95_2023=exp(wicp_cr_int_lci_2023))
(wicp_cr_u95_2023=exp(wicp_cr_int_uci_2023))
#1.369/hr to 2.001/hr

## 2024 catch rate
## exp(2023 intercept plus estimate for intercept change in 2024) 
## This is calculated as exp(cond.(Intercept)+cond.yr2024) from the model summary
(wicp_cr_2024=exp(wicp_cr_int_2024))
#Expected catch rate in 2024 is 1.361/hr
(wicp_cr_z)
(wicp_cr_p)
#Difference is not significant (z=-0.972, p=0.331)

## 2024 catch rate confidence interval
## First get the estimate for catch by summing the reference (2023) intercept and change for 2024
## Add and subtract 1.96*SE from the conditional model to get upper and lower bounds
## 95% CI estimates are 1/exp(the number calculated above)
(wicp_cr_l95_2024=exp(wicp_cr_int_lci_2024))
(wicp_cr_u95_2024=exp(wicp_cr_int_uci_2024))
#0.917/hr to 2.020/hr

################################################################################
## Now interpret the zero-inflation model to look at probability of zero catch
## Probability of observing a zero catch in reference year (2023)
## Intercept in the zero-inflation model is log-odds of catching zero fish in 2023 (reference year)

## Convert log-odds to odds to probability of zero catch in 2023
(wicp_zi_2023=exp(wicp_zi_int_2023)/(1+exp(wicp_zi_int_2023)))
#0.516 probability of zero catch in 2023

## Calculate 95% confidence intervals for zero catch in 2023
(wicp_zi_lci_2023=exp(wicp_zi_int_lci_2023)/(1+exp(wicp_zi_int_lci_2023)))
(wicp_zi_uci_2023=exp(wicp_zi_int_uci_2023)/(1+exp(wicp_zi_int_uci_2023)))
#0.393 to 0.637

## Now calculate 2024 probability of zero catch
## Convert log odds to odds to probability of zero catch in 2024
(wicp_zi_2024=exp(wicp_zi_int_2024)/(1+exp(wicp_zi_int_2024)))
#0.550 probability of zero catch in 2024
(wicp_zi_z)
(wicp_zi_p)
#Difference is not significant (z=0.264,p=0.792)

## Calculate 95% confidence intervals for zero catch in 2024
(wicp_zi_lci_2024=exp(wicp_zi_int_lci_2024)/(1+exp(wicp_zi_int_lci_2024)))
(wicp_zi_uci_2024=exp(wicp_zi_int_uci_2024)/(1+exp(wicp_zi_int_uci_2024)))
#0.308 to 0.771

## Output graphing data
wicpout2023=tibble(impd="Blackbird",year=2023,stocked=2023,pzero=wicp_zi_2023,
                   pzero_lci=wicp_zi_lci_2023,pzero_uci=wicp_zi_uci_2023,
                   cr=wicp_cr_2023,cr_lci=wicp_cr_l95_2023,cr_uci=wicp_cr_u95_2023,
                   pzero_z=wicp_zi_z,pzero_p=wicp_zi_p,cr_z=wicp_cr_z,cr_p=wicp_cr_p)

wicpout2024=tibble(impd="Blackbird",year=2024,stocked=2023,pzero=wicp_zi_2024,
                   pzero_lci=wicp_zi_lci_2024,pzero_uci=wicp_zi_uci_2024,
                   cr=wicp_cr_2024,cr_lci=wicp_cr_l95_2024,cr_uci=wicp_cr_u95_2024,
                   pzero_z=wicp_zi_z,pzero_p=wicp_zi_p,cr_z=wicp_cr_z,cr_p=wicp_cr_p)

wicpout=bind_rows(wicpout2023,wicpout2024)

################################################################################
################################################################################
## Sunflower
wicq=filter(dat4,code=="WICQ")%>%
  select(id,code,ccf_anghr,yr)

## Hurdle model using glmmTMB package and gamma distribution with a log link for positive values
wicq_mod=glmmTMB(ccf_anghr~yr,data=wicq,family=ziGamma(link="log"),zi= ~ yr)
(wicq_modsum=summary(wicq_mod))
confint(wicq_mod)

## Extract parameter estimates for consistent calculations
## 2023 catch rate
wicq_cr_int_2023=wicq_modsum$coefficients$cond[1,"Estimate"]      #2023 conditional intercept
wicq_cr_int_se_2023=wicq_modsum$coefficients$cond[1,"Std. Error"] #2023 conditional intercept se
wicq_cr_int_lci_2023=wicq_cr_int_2023+(1.96*wicq_cr_int_se_2023)  #2023 conditional intercept lci
wicq_cr_int_uci_2023=wicq_cr_int_2023-(1.96*wicq_cr_int_se_2023)  #2023 conditional intercept uci

##2024 catch rate
wicq_cr_int_2024=wicq_modsum$coefficients$cond[1,"Estimate"]+
  wicq_modsum$coefficients$cond[2,"Estimate"]                     #2024 conditional intercept
wicq_cr_int_se_2024=wicq_modsum$coefficients$cond[2,"Std. Error"] #2024 conditional intercept se
wicq_cr_int_lci_2024=wicq_cr_int_2024+(1.96*wicq_cr_int_se_2024)  #2024 conditional intercept lci
wicq_cr_int_uci_2024=wicq_cr_int_2024-(1.96*wicq_cr_int_se_2024)  #2024 conditional intercept uci

## 2023 probability of zero catch
wicq_zi_int_2023=wicq_modsum$coefficients$zi[1,"Estimate"]        #2023 zero-inflated intercept
wicq_zi_int_se_2023=wicq_modsum$coefficients$zi[1,"Std. Error"]   #2023 zero-inflated intercept se
wicq_zi_int_lci_2023=wicq_zi_int_2023-(1.96*wicq_zi_int_se_2023)  #2023 zero-inflated intercept lci
wicq_zi_int_uci_2023=wicq_zi_int_2023+(1.96*wicq_zi_int_se_2023)  #2023 zero-inflated intercept uci

## 2024 probability of zero catch
wicq_zi_int_2024=wicq_modsum$coefficients$zi[1,"Estimate"]+
  wicq_modsum$coefficients$zi[2,"Estimate"]                       #2024 zero-inflated intercept
wicq_zi_int_se_2024=wicq_modsum$coefficients$zi[2,"Std. Error"]   #2024 zero-inflated intercept se
wicq_zi_int_lci_2024=wicq_zi_int_2024-(1.96*wicq_zi_int_se_2024)  #2024 zero-inflated intercept lci
wicq_zi_int_uci_2024=wicq_zi_int_2024+(1.96*wicq_zi_int_se_2024)  #2024 zero-inflated intercept uci

## Z-values and P-values for models
wicq_cr_z=wicq_modsum$coefficients$cond[2,"z value"]
wicq_cr_p=wicq_modsum$coefficients$cond[2,"Pr(>|z|)"]

wicq_zi_z=wicq_modsum$coefficients$zi[2,"z value"]
wicq_zi_p=wicq_modsum$coefficients$zi[2,"Pr(>|z|)"]

################################################################################
## First interpret the conditional model
## This is catch rate by anglers that actually caught fish

## 2023 (reference year) catch rate
## Since we are using the gamma distribution with a log link, the actual value is exp(intercept)
(wicq_cr_2023=exp(wicq_cr_int_2023))
#Expected catch rate in 2023 is 1.400/hr

## 2023 catch rate confidence interval
## Analogous to estimate +- 1.96(SE)
(wicq_cr_l95_2023=exp(wicq_cr_int_lci_2023))
(wicq_cr_u95_2023=exp(wicq_cr_int_uci_2023))
#1.020/hr to 1.920/hr

## 2024 catch rate
## exp(2023 intercept plus estimate for intercept change in 2024) 
## This is calculated as exp(cond.(Intercept)+cond.yr2024) from the model summary
(wicq_cr_2024=exp(wicq_cr_int_2024))
#Expected catch rate in 2024 is 1.355/hr
(wicq_cr_z)
(wicq_cr_p)
#Difference is not significant (z=-0.169, p=0.866)

## 2024 catch rate confidence interval
## First get the estimate for catch by summing the reference (2023) intercept and change for 2024
## Add and subtract 1.96*SE from the conditional model to get upper and lower bounds
## 95% CI estimates are 1/exp(the number calculated above)
(wicq_cr_l95_2024=exp(wicq_cr_int_lci_2024))
(wicq_cr_u95_2024=exp(wicq_cr_int_uci_2024))
#0.928/hr to 1.978/hr

################################################################################
## Now interpret the zero-inflation model to look at probability of zero catch
## Probability of observing a zero catch in reference year (2023)
## Intercept in the zero-inflation model is log-odds of catching zero fish in 2023 (reference year)

## Convert log-odds to odds to probability of zero catch in 2023
(wicq_zi_2023=exp(wicq_zi_int_2023)/(1+exp(wicq_zi_int_2023)))
#0.652 probability of zero catch in 2023

## Calculate 95% confidence intervals for zero catch in 2023
(wicq_zi_lci_2023=exp(wicq_zi_int_lci_2023)/(1+exp(wicq_zi_int_lci_2023)))
(wicq_zi_uci_2023=exp(wicq_zi_int_uci_2023)/(1+exp(wicq_zi_int_uci_2023)))
#0.505 to 0.775

## Now calculate 2024 probability of zero catch
## Convert log odds to odds to probability of zero catch in 2024
(wicq_zi_2024=exp(wicq_zi_int_2024)/(1+exp(wicq_zi_int_2024)))
#0.500 probability of zero catch in 2024
(wicq_zi_z)
(wicq_zi_p)
#Difference is not significant (z=-1.624,p=0.104)

## Calculate 95% confidence intervals for zero catch in 2024
(wicq_zi_lci_2024=exp(wicq_zi_int_lci_2024)/(1+exp(wicq_zi_int_lci_2024)))
(wicq_zi_uci_2024=exp(wicq_zi_int_uci_2024)/(1+exp(wicq_zi_int_uci_2024)))
#0.319 to 0.681

## Output graphing data
wicqout2023=tibble(impd="Sunflower",year=2023,stocked=2024,pzero=wicq_zi_2023,
                   pzero_lci=wicq_zi_lci_2023,pzero_uci=wicq_zi_uci_2023,
                   cr=wicq_cr_2023,cr_lci=wicq_cr_l95_2023,cr_uci=wicq_cr_u95_2023,
                   pzero_z=wicq_zi_z,pzero_p=wicq_zi_p,cr_z=wicq_cr_z,cr_p=wicq_cr_p)

wicqout2024=tibble(impd="Sunflower",year=2024,stocked=2024,pzero=wicq_zi_2024,
                   pzero_lci=wicq_zi_lci_2024,pzero_uci=wicq_zi_uci_2024,
                   cr=wicq_cr_2024,cr_lci=wicq_cr_l95_2024,cr_uci=wicq_cr_u95_2024,
                   pzero_z=wicq_zi_z,pzero_p=wicq_zi_p,cr_z=wicq_cr_z,cr_p=wicq_cr_p)

wicqout=bind_rows(wicqout2023,wicqout2024)

################################################################################
################################################################################
## Horseshoe
sgch=filter(dat4,code=="SGCH")%>%
     select(id,code,ccf_anghr,yr)

## Hurdle model using glmmTMB package and gamma distribution with a log link for positive values
sgch_mod=glmmTMB(ccf_anghr~yr,data=sgch,family=ziGamma(link="log"),zi= ~ yr)
(sgch_modsum=summary(sgch_mod))
confint(sgch_mod)

## Extract parameter estimates for consistent calculations
## 2023 catch rate
sgch_cr_int_2023=sgch_modsum$coefficients$cond[1,"Estimate"]      #2023 conditional intercept
sgch_cr_int_se_2023=sgch_modsum$coefficients$cond[1,"Std. Error"] #2023 conditional intercept se
sgch_cr_int_lci_2023=sgch_cr_int_2023+(1.96*sgch_cr_int_se_2023)  #2023 conditional intercept lci
sgch_cr_int_uci_2023=sgch_cr_int_2023-(1.96*sgch_cr_int_se_2023)  #2023 conditional intercept uci

##2024 catch rate
sgch_cr_int_2024=sgch_modsum$coefficients$cond[1,"Estimate"]+
  sgch_modsum$coefficients$cond[2,"Estimate"]                     #2024 conditional intercept
sgch_cr_int_se_2024=sgch_modsum$coefficients$cond[2,"Std. Error"] #2024 conditional intercept se
sgch_cr_int_lci_2024=sgch_cr_int_2024+(1.96*sgch_cr_int_se_2024)  #2024 conditional intercept lci
sgch_cr_int_uci_2024=sgch_cr_int_2024-(1.96*sgch_cr_int_se_2024)  #2024 conditional intercept uci

## 2023 probability of zero catch
sgch_zi_int_2023=sgch_modsum$coefficients$zi[1,"Estimate"]        #2023 zero-inflated intercept
sgch_zi_int_se_2023=sgch_modsum$coefficients$zi[1,"Std. Error"]   #2023 zero-inflated intercept se
sgch_zi_int_lci_2023=sgch_zi_int_2023-(1.96*sgch_zi_int_se_2023)  #2023 zero-inflated intercept lci
sgch_zi_int_uci_2023=sgch_zi_int_2023+(1.96*sgch_zi_int_se_2023)  #2023 zero-inflated intercept uci

## 2024 probability of zero catch
sgch_zi_int_2024=sgch_modsum$coefficients$zi[1,"Estimate"]+
  sgch_modsum$coefficients$zi[2,"Estimate"]                       #2024 zero-inflated intercept
sgch_zi_int_se_2024=sgch_modsum$coefficients$zi[2,"Std. Error"]   #2024 zero-inflated intercept se
sgch_zi_int_lci_2024=sgch_zi_int_2024-(1.96*sgch_zi_int_se_2024)  #2024 zero-inflated intercept lci
sgch_zi_int_uci_2024=sgch_zi_int_2024+(1.96*sgch_zi_int_se_2024)  #2024 zero-inflated intercept uci

## Z-values and P-values for models
sgch_cr_z=sgch_modsum$coefficients$cond[2,"z value"]
sgch_cr_p=sgch_modsum$coefficients$cond[2,"Pr(>|z|)"]

sgch_zi_z=sgch_modsum$coefficients$zi[2,"z value"]
sgch_zi_p=sgch_modsum$coefficients$zi[2,"Pr(>|z|)"]

################################################################################
## First interpret the conditional model
## This is catch rate by anglers that actually caught fish

## 2023 (reference year) catch rate
## Since we are using the gamma distribution with a log link, the actual value is exp(intercept)
(sgch_cr_2023=exp(sgch_cr_int_2023))
#Expected catch rate in 2023 is 1.061/hr

## 2023 catch rate confidence interval
## Analogous to estimate +- 1.96(SE)
(sgch_cr_l95_2023=exp(sgch_cr_int_lci_2023))
(sgch_cr_u95_2023=exp(sgch_cr_int_uci_2023))
#0.787/hr to 1.433/hr

## 2024 catch rate
## exp(2023 intercept plus estimate for intercept change in 2024) 
## This is calculated as exp(cond.(Intercept)+cond.yr2024) from the model summary
(sgch_cr_2024=exp(sgch_cr_int_2024))
#Expected catch rate in 2024 is 1.004/hr
(sgch_cr_z)
(sgch_cr_p)
#Difference is not significant (z=-0.232, p=0.816)

## 2024 catch rate confidence interval
## First get the estimate for catch by summing the reference (2023) intercept and change for 2024
## Add and subtract 1.96*SE from the conditional model to get upper and lower bounds
## 95% CI estimates are 1/exp(the number calculated above)
(sgch_cr_l95_2024=exp(sgch_cr_int_lci_2024))
(sgch_cr_u95_2024=exp(sgch_cr_int_uci_2024))
#0.628/hr to 1.605/hr

################################################################################
## Now interpret the zero-inflation model to look at probability of zero catch
## Probability of observing a zero catch in reference year (2023)
## Intercept in the zero-inflation model is log-odds of catching zero fish in 2023 (reference year)

## Convert log-odds to odds to probability of zero catch in 2023
(sgch_zi_2023=exp(sgch_zi_int_2023)/(1+exp(sgch_zi_int_2023)))
#0.700 probability of zero catch in 2023

## Calculate 95% confidence intervals for zero catch in 2023
(sgch_zi_lci_2023=exp(sgch_zi_int_lci_2023)/(1+exp(sgch_zi_int_lci_2023)))
(sgch_zi_uci_2023=exp(sgch_zi_int_uci_2023)/(1+exp(sgch_zi_int_uci_2023)))
#0.612 to 0.775

## Now calculate 2024 probability of zero catch
## Convert log odds to odds to probability of zero catch in 2024
(sgch_zi_2024=exp(sgch_zi_int_2024)/(1+exp(sgch_zi_int_2024)))
#0.702 probability of zero catch in 2024
(sgch_zi_z)
(sgch_zi_p)
#Difference is not significant (z=0.037,p=0.971)

## Calculate 95% confidence intervals for zero catch in 2024
(sgch_zi_lci_2024=exp(sgch_zi_int_lci_2024)/(1+exp(sgch_zi_int_lci_2024)))
(sgch_zi_uci_2024=exp(sgch_zi_int_uci_2024)/(1+exp(sgch_zi_int_uci_2024)))
#0.562 to 0.813

## Output graphing data
sgchout2023=tibble(impd="Horseshoe",year=2023,stocked=2023,pzero=sgch_zi_2023,
                   pzero_lci=sgch_zi_lci_2023,pzero_uci=sgch_zi_uci_2023,
                   cr=sgch_cr_2023,cr_lci=sgch_cr_l95_2023,cr_uci=sgch_cr_u95_2023,
                   pzero_z=sgch_zi_z,pzero_p=sgch_zi_p,cr_z=sgch_cr_z,cr_p=sgch_cr_p)

sgchout2024=tibble(impd="Horseshoe",year=2024,stocked=2023,pzero=sgch_zi_2024,
                   pzero_lci=sgch_zi_lci_2024,pzero_uci=sgch_zi_uci_2024,
                   cr=sgch_cr_2024,cr_lci=sgch_cr_l95_2024,cr_uci=sgch_cr_u95_2024,
                   pzero_z=sgch_zi_z,pzero_p=sgch_zi_p,cr_z=sgch_cr_z,cr_p=sgch_cr_p)

sgchout=bind_rows(sgchout2023,sgchout2024)

################################################################################
################################################################################
## Vics
sgcg=filter(dat4,code=="SGCG")%>%
     select(id,code,ccf_anghr,yr)

## Hurdle model using glmmTMB package and gamma distribution with a log link for positive values
sgcg_mod=glmmTMB(ccf_anghr~yr,data=sgcg,family=ziGamma(link="log"),zi= ~ yr)
(sgcg_modsum=summary(sgcg_mod))
confint(sgcg_mod)

## Extract parameter estimates for consistent calculations
## 2023 catch rate
sgcg_cr_int_2023=sgcg_modsum$coefficients$cond[1,"Estimate"]      #2023 conditional intercept
sgcg_cr_int_se_2023=sgcg_modsum$coefficients$cond[1,"Std. Error"] #2023 conditional intercept se
sgcg_cr_int_lci_2023=sgcg_cr_int_2023+(1.96*sgcg_cr_int_se_2023)  #2023 conditional intercept lci
sgcg_cr_int_uci_2023=sgcg_cr_int_2023-(1.96*sgcg_cr_int_se_2023)  #2023 conditional intercept uci

##2024 catch rate
sgcg_cr_int_2024=sgcg_modsum$coefficients$cond[1,"Estimate"]+
  sgcg_modsum$coefficients$cond[2,"Estimate"]                     #2024 conditional intercept
sgcg_cr_int_se_2024=sgcg_modsum$coefficients$cond[2,"Std. Error"] #2024 conditional intercept se
sgcg_cr_int_lci_2024=sgcg_cr_int_2024+(1.96*sgcg_cr_int_se_2024)  #2024 conditional intercept lci
sgcg_cr_int_uci_2024=sgcg_cr_int_2024-(1.96*sgcg_cr_int_se_2024)  #2024 conditional intercept uci

## 2023 probability of zero catch
sgcg_zi_int_2023=sgcg_modsum$coefficients$zi[1,"Estimate"]        #2023 zero-inflated intercept
sgcg_zi_int_se_2023=sgcg_modsum$coefficients$zi[1,"Std. Error"]   #2023 zero-inflated intercept se
sgcg_zi_int_lci_2023=sgcg_zi_int_2023-(1.96*sgcg_zi_int_se_2023)  #2023 zero-inflated intercept lci
sgcg_zi_int_uci_2023=sgcg_zi_int_2023+(1.96*sgcg_zi_int_se_2023)  #2023 zero-inflated intercept uci

## 2024 probability of zero catch
sgcg_zi_int_2024=sgcg_modsum$coefficients$zi[1,"Estimate"]+
  sgcg_modsum$coefficients$zi[2,"Estimate"]                       #2024 zero-inflated intercept
sgcg_zi_int_se_2024=sgcg_modsum$coefficients$zi[2,"Std. Error"]   #2024 zero-inflated intercept se
sgcg_zi_int_lci_2024=sgcg_zi_int_2024-(1.96*sgcg_zi_int_se_2024)  #2024 zero-inflated intercept lci
sgcg_zi_int_uci_2024=sgcg_zi_int_2024+(1.96*sgcg_zi_int_se_2024)  #2024 zero-inflated intercept uci

## Z-values and P-values for models
sgcg_cr_z=sgcg_modsum$coefficients$cond[2,"z value"]
sgcg_cr_p=sgcg_modsum$coefficients$cond[2,"Pr(>|z|)"]

sgcg_zi_z=sgcg_modsum$coefficients$zi[2,"z value"]
sgcg_zi_p=sgcg_modsum$coefficients$zi[2,"Pr(>|z|)"]

################################################################################
## First interpret the conditional model
## This is catch rate by anglers that actually caught fish

## 2023 (reference year) catch rate
## Since we are using the gamma distribution with a log link, the actual value is exp(intercept)
(sgcg_cr_2023=exp(sgcg_cr_int_2023))
#Expected catch rate in 2023 is 0.471/hr

## 2023 catch rate confidence interval
## Analogous to estimate +- 1.96(SE)
(sgcg_cr_l95_2023=exp(sgcg_cr_int_lci_2023))
(sgcg_cr_u95_2023=exp(sgcg_cr_int_uci_2023))
#0.290/hr to 0.766/hr

## 2024 catch rate
## exp(2023 intercept plus estimate for intercept change in 2024) 
## This is calculated as exp(cond.(Intercept)+cond.yr2024) from the model summary
(sgcg_cr_2024=exp(sgcg_cr_int_2024))
#Expected catch rate in 2024 is 1.113/hr
(sgcg_cr_z)
(sgcg_cr_p)
#Difference is significant (z=3.017, p=0.003)

## 2024 catch rate confidence interval
## First get the estimate for catch by summing the reference (2023) intercept and change for 2024
## Add and subtract 1.96*SE from the conditional model to get upper and lower bounds
## 95% CI estimates are 1/exp(the number calculated above)
(sgcg_cr_l95_2024=exp(sgcg_cr_int_lci_2024))
(sgcg_cr_u95_2024=exp(sgcg_cr_int_uci_2024))
#0.637/hr to 1.946/hr

################################################################################
## Now interpret the zero-inflation model to look at probability of zero catch
## Probability of observing a zero catch in reference year (2023)
## Intercept in the zero-inflation model is log-odds of catching zero fish in 2023 (reference year)

## Convert log-odds to odds to probability of zero catch in 2023
(sgcg_zi_2023=exp(sgcg_zi_int_2023)/(1+exp(sgcg_zi_int_2023)))
#0.931 probability of zero catch in 2023

## Calculate 95% confidence intervals for zero catch in 2023
(sgcg_zi_lci_2023=exp(sgcg_zi_int_lci_2023)/(1+exp(sgcg_zi_int_lci_2023)))
(sgcg_zi_uci_2023=exp(sgcg_zi_int_uci_2023)/(1+exp(sgcg_zi_int_uci_2023)))
#0.884 to 0.959

## Now calculate 2024 probability of zero catch
## Convert log odds to odds to probability of zero catch in 2024
(sgcg_zi_2024=exp(sgcg_zi_int_2024)/(1+exp(sgcg_zi_int_2024)))
#0.824 probability of zero catch in 2024
(sgcg_zi_z)
(sgcg_zi_p)
#Difference is significant (z=-3.146,p=0.002)

## Calculate 95% confidence intervals for zero catch in 2024
(sgcg_zi_lci_2024=exp(sgcg_zi_int_lci_2024)/(1+exp(sgcg_zi_int_lci_2024)))
(sgcg_zi_uci_2024=exp(sgcg_zi_int_uci_2024)/(1+exp(sgcg_zi_int_uci_2024)))
#0.708 to 0.900

## Output graphing data
sgcgout2023=tibble(impd="Vics",year=2023,stocked=2024,pzero=sgcg_zi_2023,
                   pzero_lci=sgcg_zi_lci_2023,pzero_uci=sgcg_zi_uci_2023,
                   cr=sgcg_cr_2023,cr_lci=sgcg_cr_l95_2023,cr_uci=sgcg_cr_u95_2023,
                   pzero_z=sgcg_zi_z,pzero_p=sgcg_zi_p,cr_z=sgcg_cr_z,cr_p=sgcg_cr_p)

sgcgout2024=tibble(impd="Vics",year=2024,stocked=2024,pzero=sgcg_zi_2024,
                   pzero_lci=sgcg_zi_lci_2024,pzero_uci=sgcg_zi_uci_2024,
                   cr=sgcg_cr_2024,cr_lci=sgcg_cr_l95_2024,cr_uci=sgcg_cr_u95_2024,
                   pzero_z=sgcg_zi_z,pzero_p=sgcg_zi_p,cr_z=sgcg_cr_z,cr_p=sgcg_cr_p)

sgcgout=bind_rows(sgcgout2023,sgcgout2024)

################################################################################
################################################################################
## Create plots
plotdat=bind_rows(wicpout,wicqout,sgchout,sgcgout)%>%
  mutate(stocked=factor(stocked),
         pzero_line=case_when(pzero_p <= 0.05 ~ "sig",
                              TRUE ~ "nosig"),
         cr_line=case_when(cr_p<=0.05 ~ "sig",
                           TRUE ~ "nosig"))

################################################################################
## Zero catch plots

## Create Blackbird and Sunflower plot
bs=filter(plotdat,impd=="Blackbird" | impd=="Sunflower")

bs_zi=ggplot(bs,aes(x=year,y=pzero,ymin=pzero_lci,ymax=pzero_uci,color=impd))+
  geom_pointrange(position=position_dodge(width=0.05),size=2,linewidth=2)+
  geom_line(aes(linetype=pzero_line),linewidth=1.2,show.legend=F)+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_color_manual(values=c("#B12A90","#F0F921"),
                     labels=c("Blackbird (stocked 2023)",
                              "Sunflower (stocked 2024)"))+
  scale_x_continuous(limits=c(2022.9,2024.1),
                     breaks=c(2023,2024),
                     name="",
                     expand=c(0,0))+
  scale_y_continuous(limits=c(0,1.01),
                     breaks=seq(0,1,0.1),
                     name="Probability of excess zeroes",
                     expand=c(0,0))+
  pubtheme

## Create Horseshoe and Vic's plot
hv=filter(plotdat,impd=="Horseshoe" | impd=="Vics")

hv_zi=ggplot(hv,aes(x=year,y=pzero,ymin=pzero_lci,ymax=pzero_uci,color=impd))+
  geom_pointrange(position=position_dodge(width=0.05),size=2,linewidth=2)+
  geom_line(aes(linetype=pzero_line),linewidth=1.2,show.legend=F)+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_color_manual(values=c("#F1605D","#440154"),
                     labels=c("Horseshoe (stocked 2023)",
                              "Vic's (stocked 2024)"))+
  scale_x_continuous(limits=c(2022.9,2024.1),
                     breaks=c(2023,2024),
                     name="",
                     expand=c(0,0))+
  scale_y_continuous(limits=c(0,1.01),
                     breaks=seq(0,1,0.1),
                     name="",
                     expand=c(0,0))+
  pubtheme

################################################################################
## Catch rate plots

## Create Blackbird and Sunflower plot
bs_cr=ggplot(bs,aes(x=year,y=cr,ymin=cr_lci,ymax=cr_uci,color=impd))+
  geom_pointrange(position=position_dodge(width=0.05),size=2,linewidth=2)+
  geom_line(aes(linetype=pzero_line),linewidth=1.2,show.legend=F)+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_color_manual(values=c("#B12A90","#F0F921"),
                     labels=c("Blackbird (stocked 2023)",
                              "Sunflower (stocked 2024)"))+
  scale_x_continuous(limits=c(2022.9,2024.1),
                     breaks=c(2023,2024),
                     name="",
                     expand=c(0,0))+
  scale_y_continuous(limits=c(0,2.21),
                     breaks=seq(0,2.2,0.2),
                     name="Successful angler catch rate (fish/hr)",
                     expand=c(0,0))+
  pubtheme+
  theme(legend.position="blank")

## Create Horseshoe and Vic's plot
hv_cr=ggplot(hv,aes(x=year,y=cr,ymin=cr_lci,ymax=cr_uci,color=impd))+
  geom_pointrange(position=position_dodge(width=0.05),size=2,linewidth=2)+
  geom_line(aes(linetype=pzero_line),linewidth=1.2,show.legend=F)+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_color_manual(values=c("#F1605D","#440154"),
                     labels=c("Horseshoe (stocked 2023)",
                              "Vic's (stocked 2024)"))+
  scale_x_continuous(limits=c(2022.9,2024.1),
                     breaks=c(2023,2024),
                     name="",
                     expand=c(0,0))+
  scale_y_continuous(limits=c(0,2.21),
                     breaks=seq(0,2.2,0.2),
                     name="",
                     expand=c(0,0))+
  pubtheme+
  theme(legend.position="blank")

################################################################################
## Combine plots and export
out=(bs_zi|hv_zi)/(bs_cr|hv_cr)
ggsave(plot=out,"zigamma_log.png",width=8,height=8,units="in",bg="white")