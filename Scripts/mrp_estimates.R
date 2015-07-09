### G 
rm(list=c(ls()))
library(dplyr)
library(lme4)
library(data.table)
source("~/Dropbox/func.R")
mrp_year <- rbind(data.frame(formrp,year=2008),data.frame(formrp,year=2009),data.frame(formrp,year=2010),data.frame(formrp,year=2011),data.frame(formrp,year=2012))
mrp_year$cbsa_year <- interaction(mrp_year$cbsa,mrp_year$year)

load("Data/CCES/cces20102012_withpk.Rdata")
source("../../func.R")
cces201012$pk_z1 <- zero1(cces201012$pk_z1)

cbsacounty <- read.csv("Data/Crosswalks/cbsacounty.csv")
statefips <- read.table("Data/Crosswalks/state.txt",sep="|",header=T)
cces201012$statefips <- sprintf("%02d",statefips$STATE[match(cces201012$state_pre,statefips$STATE_NAME)])
cces201012$countyfips <- paste(cces201012$statefips,sprintf("%03d", cces201012$county_fips_pre),sep="")
cces201012$county <- sprintf("%05d", cces201012$county_fips_pre)
cces201012$county[which(cces201012$year==2011|cces201012$year==2008|cces201012$year==2009)]<- cces201012$countyfips[which(cces201012$year==2011|cces201012$year==2008|cces201012$year==2009)]
cces201012 <- merge(cces201012,cbsacounty,by="county")
cces201012$cbsa_year <- interaction(cces201012$cbsa,cces201012$year)

individual.model <- lmer(formula = pk_z1 ~ (1|race.female) + (1|age_group) + (1|educ) + (1|educ_age) + (1|cbsa_year)+(1|year),data=subset(cces201012))
table(cces201012$state_pre,cces201012$statefips)
# Create vector of county ranefs and then fill in missing ones
district.name <-   unique(as.character(mrp_year$cbsa_year))
district.ranefs.1 <- array(NA,c(length(district.name),1)); dimnames(district.ranefs.1) <- list(c(district.name),"effect")

for(j in district.name){
  district.ranefs.1[j,1] <- ranef(individual.model)$cbsa_year[j,1]
}


cells.1 <- (fixef(individual.model)['(Intercept)']
            + ranef(individual.model)$race.female[mrp_year$race.female,1]
            + ranef(individual.model)$age_group[mrp_year$age_group,1]
            + ranef(individual.model)$educ[mrp_year$educ,1]
            + ranef(individual.model)$educ_age[mrp_year$educ_age,1]
            + district.ranefs.1[as.character(mrp_year$cbsa_year),1]
)

#weights the prediction by the freq of cell                                       
cellpredweighted <- cells.1 * mrp_year$rel.freq
mrpscore <-tapply(cellpredweighted, mrp_year$cbsa_year, sum)
ss
mrpdf <- na.omit(data.frame(cbsa=dimnames(mrpscore),mrpscore))
mrpdf$cbsa <- mrpdf$c....2008....10140.2008....10180.2008....10300.2008....10420.2008...
head(mrpdf$mrpscore[order(mrpdf$mrpscore)])
save(mrpscore,file="Data/mrpscores.RData")
