library(reshape2)                      
library(dplyr)
library(plm)
## Okla Speed
ookla <- read.csv("Data/Ookla/netindex_by_year_r3.csv")
ookla1 <- ookla[,c(18:24,32)]
ooklalong <- melt(ookla1,id.vars = "zip")
ooklalong$year <-as.numeric(gsub(x = ooklalong$variable,pattern="download_kbps_",replacement = ""))
zipcbsa <- read.csv("Data/Crosswalks/zipcode2county.csv")
ooklalong$zcta5 <- stringr::str_pad(ooklalong$zip,pad="0",side="left",width=5)
ooklalong <- merge(ooklalong,zipcbsa,by="zcta5")

ooklalong %>% 
  group_by(cbsa,year) %>%
  summarise(speed=mean(value,na.rm=T)) ->
  speedcbsa
 
## 
load("Data/mrpscores.RData")
mrpdf <- data.frame(dn=dimnames(mrpscore),mrpscore)
mrpdf$year <- unlist(lapply(strsplit(as.character(mrpdf$c....2008....10140.2008....10180.2008....10300.2008....10420.2008...),split = ".",fixed=T),function(x)x[2]))
mrpdf$cbsa <- unlist(lapply(strsplit(as.character(mrpdf$c....2008....10140.2008....10180.2008....10300.2008....10420.2008...),split = ".",fixed=T),function(x)x[1]))

pdata <- plm.data(na.omit(mrp_speed[c("speed","cbsa","year","mrpscore")]), indexes =c("cbsa","year"))
pooling <- 
  summary(lm(mrpscore ~ log(speed), data=pdata) )

mrp_speed <- merge(speedcbsa,mrpdf,by=c("cbsa","year"))

mrp_speed$speed
library(plm)
z2 <-   pgmm(mrpscore ~ lag(mrpscore, 1)+ lag(log(speed), 0:1)  | lag(mrpscore ,2:99) + lag(log(speed), 2:99),  data = mrpspeed, effect = "twoways", model = "onestep",transformation="ld")
mrpspeed <- na.omit(mrp_speed)
library(lfe)
summary(lm(mrpscore~speed,mrp_speed))
mrpspeed$cbsa <- droplevels(mrpspeed$cbsa)
fe1 <- plm(mrpscore ~ speed,              index=c('cbsa', 'year'), data=mrpspeed)
