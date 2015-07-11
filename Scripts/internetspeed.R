library(reshape2)                      
library(dplyr)
library(plm)
library(ggplot2)
library(ggthemr)
## Okla Speed
ookla <- read.csv("Data/Ookla/netindex_by_year_r3.csv")
ookla1 <- ookla[,c(18:24,32)]
ooklalong <- na.omit(melt(ookla1,id.vars = "zip"))
ooklalong$year <-as.numeric(gsub(x = ooklalong$variable,pattern="download_kbps_",replacement = ""))
zipcbsa <- read.csv("Data/Crosswalks/zipcode2county.csv")
ooklalong$zcta5 <- stringr::str_pad(ooklalong$zip,pad="0",side="left",width=5)
ooklalong <- merge(ooklalong,zipcbsa,by="zcta5")

ooklalong %>% 
  group_by(cbsa,year) %>%
  summarise(speed=mean(value,na.rm=T)) ->
  speedcbsa
t <- poly((unique(speed_loweduc$year)), 3)
speed_loweduc[,paste("ot", 1:3, sep="")] <- t[as.numeric(as.factor(speed_loweduc$year)), 1:3]
speed_loweduc$ot1
ggthemr('fresh')
educ_cbsa_low <- subset(educ_cbsa,educ=='HS')
educ_cbsa_low$hs50 <- ifelse(educ_cbsa_low$educ=='HS' & educ_cbsa_low$rel.freq>.50,1,0)
stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, colour="black", geom=geom, size = 1, ...)
}
speedcbsa$speed <- speedcbsa$speed/1000
speed_loweduc <- merge(speedcbsa,educ_cbsa_low,by="cbsa")
ggplot(speedcbsa,aes(y=(speed),x=year))+geom_line(aes(group=cbsa),alpha=.05)+xlab("Year")+ylab("Download Speed MBPS/Minute")+geom_smooth(method="loess")
ggsave("Graphs/speed_by_cbsa.png")

ggplot(speed_loweduc,aes(y=log(speed),x=year))+geom_line()+xlab("Year")+ylab("Download Speed")
c
library(dplyr)
speedcbsa %>% 
  group_by(year) %>%
  summarise(mean(speed,na.rm=T))
  
speedcbsa$year
speed_loweduc$cbsa

  randomtime <- lmer(log(speed)~scale(year,center = T)+(1+scale(year,center = T)|cbsa),speedcbsa)
norandomtime <- lmer(log(speed)~scale(year,center = T)+(1|cbsa),speedcbsa)

cbsa=dimnames(ranef(randomtime)[[1]])[[1]]
timeff=(ranef(randomtime))[[1]][,2]+coefficients(summary(randomtime))[2,1]
out$cbsa

out <- data.frame(cbsa,timeff)
head(out[sort(out$timeff)],)
anova(randomtime,norandomtime)

 576.26 versus summary(randomtime)
## 
load("Data/mrpscores.RData")
mrpdf <- data.frame(dn=dimnames(mrpscore),mrpscore)
mrpdf$year <- unlist(lapply(strsplit(as.character(mrpdf$c....2008....10140.2008....10180.2008....10300.2008....10420.2008...),split = ".",fixed=T),function(x)x[2]))
mrpdf$cbsa <- unlist(lapply(strsplit(as.character(mrpdf$c....2008....10140.2008....10180.2008....10300.2008....10420.2008...),split = ".",fixed=T),function(x)x[1]))


mrp_speed <- merge(speedcbsa,mrpdf,by=c("cbsa","year"))

mrp_speed$cbsa[mrp_speed$cbsa==" "]=NA
mrp_speed$cbsa <- droplevels(mrp_speed$cbsa)
pdata <- plm.data(na.omit(mrp_speed[c("speed","cbsa","year","mrpscore")]), indexes =c("cbsa","year"))
mrp_speed$mrpscore <- as.vector(mrp_speed$mrpscore)

library(plm)
mrpplm <- plm.data(mrp_speed,indexes = c("cbsa","year"))
z2 <-   pgmm(mrpscore ~ lag(mrpscore, 1)+ lag(log(speed), 0:1)  | lag(mrpscore ,2:99),  data = mrpplm,effect = "twoway", model = "twostep")
summary(z2)
lm(mrpscore~(speed),mrp_speed)
z2 <-   pgmm(mrpscore ~ lag(mrpscore, 1)+ lag(log(speed), 0:1)  | lag(mrpscore ,2:99),  data = mrp_speed,effect = "twoway", model = "twostep")



summary(z2)


effect = "twoways", model = "twosteps")
mrpspeed <- na.omit(mrp_speed)
library(lfe)
summary(lm(mrpscore~speed,mrp_speed))
mrpspeed$cbsa <- droplevels(mrpspeed$cbsa)
fe1 <- plm(mrpscore ~ speed,              index=c('cbsa', 'year'), data=mrpspeed)
