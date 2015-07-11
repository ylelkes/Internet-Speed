rm(list=c(ls()))
library(data.table)
require(bit64)
library(dplyr)

### Need 5 Year 2008-2012 ACS PUMA Data
### http://www2.census.gov/acs2013_5yr/pums/csv_pus.zip
ss13_1 <- fread("csv_pus/ss13pusa.csv")
puma1 <- with(ss13_1,data.frame(ST,AGEP,RAC1P,SCHL,HISP,SEX,PUMA10,PUMA00))
rm(ss13_1)
ss13_2 <- fread("csv_pus/ss13pusb.csv")
puma2 <- with(ss13_2,data.frame(ST,AGEP,RAC1P,SCHL,HISP,SEX,PUMA10,PUMA00))
rm(ss13_2)
ss13_3 <- fread("csv_pus/ss13pusc.csv")
puma3 <- with(ss13_3,data.frame(ST,AGEP,RAC1P,SCHL,HISP,SEX,PUMA10,PUMA00))
rm(ss13_3)
ss13_4 <- fread("csv_pus/ss13pusd.csv")
puma4 <- with(ss13_4,data.frame(ST,AGEP,RAC1P,SCHL,HISP,SEX,PUMA10,PUMA00))
puma <- data.table(rbind(puma1,puma2,puma3,puma4))

## Merge CBSA Crosswalk with PUMA Data, PUMA data prior to 2010 uses different puma codes that need to be harmonized

geocorr <- read.csv("Data/Crosswalks/pumatocsba.csv")
geocorr$cbsa <- as.character(geocorr$cbsa)
geocorr$PUMA10 <- geocorr$puma12
puma <- data.table(puma)
geocorr$state_puma10 <- paste(geocorr$state,geocorr$PUMA10)
geocorr$state_puma2k <- paste(geocorr$state,geocorr$puma2k)
geocorr$puma12 <- stringr::str_pad(geocorr$puma12,side="left",width = 5,pad = "0")
geocorr$puma2k <- stringr::str_pad(geocorr$puma2k,side="left",width = 5,pad = "0")
geocorr$state_puma12 <- paste(geocorr$state,geocorr$puma12)
geocorr$state_puma2k <- paste(geocorr$state,geocorr$puma2k)

puma$PUMA10 <- stringr::str_pad(puma$PUMA10,side="left",width = 5,pad = "0")
puma$PUMA00 <- stringr::str_pad(puma$PUMA00,side="left",width = 5,pad = "0")

puma$state_puma12 <- paste(puma$ST,puma$PUMA10)
puma$state_puma2k <- paste(puma$ST,puma$PUMA00)

puma$state_puma12 <- stringr::str_pad(puma$state_puma12,side="left",width = 8,pad = "0")
puma$state_puma2k <- stringr::str_pad(puma$state_puma2k,side="left",width = 8,pad = "0")

puma$cbsa1 <- geocorr$cbsa[match(puma$state_puma2k,as.character(geocorr$state_puma2k))]
puma$cbsa <- geocorr$cbsa[match(puma$state_puma12,as.character(geocorr$state_puma12))]
puma$cbsa[which(puma$PUMA10=="000-9")] <- puma$cbsa1[which(puma$PUMA10=="000-9")]
puma <- subset(puma,cbsa!="     ")
save(puma,file="Data/Post-Stratification/pumacbsa.RData")

load("Data/Post-Stratification/pumacbsa.RData")
## Limit Sample to >18 (like CCES)
puma <- subset(puma,AGEP>=18)

puma$age_group <-   car::recode(as.numeric(puma$AGEP),"18:34='18-34';35:54='35-54';55:120='55+';else=NA")
puma$female <- car::recode(puma$SEX,"1='Male';2='Female'")
puma$race4 <- car::recode(puma$RAC1P,"1='White';2='Black';else='Else'")
puma$race4[puma$HISP!=1]='Hispanic'
puma$educ <-   car::recode(as.numeric(puma$SCHL),"1:17='HS';18:20='Some College';21:24='College+'")
puma$race.female <- interaction(puma$female,puma$race4)
puma$educ_age <- interaction(puma$educ,puma$age_group)
puma$ST <- stringr::str_pad(puma$ST,side="left",width = 2,pad = "0")


puma %>% 
  group_by (cbsa) %.%
  mutate (total = .N) %.%
  group_by(cbsa,race4,female,age_group,educ,total) %>%
  summarise (n = n()) %>%
  mutate (rel.freq = n / total) -> formrp

## External Check: http://www.memphischamber.com/Articles/DoBusiness/pdfMemphis_MSA_Demographics.aspx
## Percecnt White in 2014, 49 percent, percent black 42 percent
sum(formrp$rel.freq[formrp$cbsa=="32820" & formrp$race4=='White'],na.rm=T) 
sum(formrp$rel.freq[formrp$cbsa=="32820" & formrp$race4=='Black'],na.rm=T) 

sum(formrp$rel.freq[formrp$cbsa=="12060" & formrp$race4=='White'],na.rm=T) 
sum(formrp$rel.freq[formrp$cbsa=="12060" & formrp$race4=='Black'],na.rm=T) 


save(formrp,file="Data/Post-Stratification/formrp_cbsa.RData")
formrp$race.female <-   interaction(formrp$female,formrp$race4) 
formrp$educ_age <- interaction(formrp$educ,formrp$age_group) 


save(formrp,file="Data/Post-Stratification/formrp_cbsa.RData")


####
load("Data/Post-Stratification/pumacbsa.RData")
## Limit Sample to >18 (like CCES)
puma <- subset(puma,AGEP>=18)

puma$age_group <-   car::recode(as.numeric(puma$AGEP),"18:34='18-34';35:54='35-54';55:120='55+';else=NA")
puma$female <- car::recode(puma$SEX,"1='Male';2='Female'")
puma$race4 <- car::recode(puma$RAC1P,"1='White';2='Black';else='Else'")
puma$race4[puma$HISP!=1]='Hispanic'
puma$educ <-   car::recode(as.numeric(puma$SCHL),"1:17='HS';18:20='Some College';21:24='College+'")
puma$race.female <- interaction(puma$female,puma$race4)
puma$educ_age <- interaction(puma$educ,puma$age_group)
puma$ST <- stringr::str_pad(puma$ST,side="left",width = 2,pad = "0")


puma %>% 
  group_by (cbsa) %.%
  mutate (total = .N) %.%
  group_by(cbsa,educ,total) %>%
  summarise (n = n()) %>%
  mutate (rel.freq = n / total) -> educ_cbsa

save(educ_cbsa,file="Data/Post-Stratification/educ_cbsa.RData")

