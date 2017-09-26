####################################################################################################
##
## LTREB Survival Analysis - Bryant Dossman, Colin Studds, Peter P. Marra
##
####################################################################################################

## Libraries
library(tidyverse)
library(marked)
library(reshape)

## Reading Data

band <- read.csv("./data/complete banding.csv")
band$CAPDATE <- as.POSIXct(band$CAPDATE, format="%d-%b-%y")

## 
select <- band %>% 
  group_by(SPECIES) %>%
  filter(BNDCREW=="LTREB") %>%
  summarise(n=n()) %>% 
  filter(n > 90) %>% 
  select(SPECIES)

select <- as.character(select$SPECIES)


df <- band %>% filter(SPECIES %in% select, BNDCREW=="LTREB") %>% 
  group_by(BAND, YEAR) %>% 
  select(SPECIES, BAND, YEAR, HABITAT)

df <- df[!duplicated(df),]

df$detect = 1

df$status <- ifelse(df$SPECIES %in% c("AMRE","BAWW","NOWA","OVEN","PRAW","SWWA","SWWA"), "Migrant","Resident")

df <- melt(df,id.var=c("SPECIES","BAND","YEAR", "status","HABITAT"),measure.var="detect") %>%
  cast(SPECIES + status+ BAND + HABITAT ~YEAR)

df[is.na(df)]=0

df <- data.frame(ch=paste(df[,5],df[,6],df[,7],df[,8],df[,9],df[,10],df[,11],df[,12], df[,13], df[,14], sep=""), 
                 status = df$status, spp = df$SPECIES, hab=df$HABITAT)
df$ch <- as.character(df$ch) 
df$spp <- as.factor(as.character(df$spp))
df$status <- as.factor(as.character(df$status))
df$hab <- as.factor(as.character(df$hab))
#df <- df[-which(df$ch == "0000000001"),]  #removing all entries of birds banded in the final years



df.proc=process.data(df)

df.ddl=make.design.data(df.proc)

Phi.spp=list(formula=~1)
model=crm(df.proc,df.ddl,model.parameters=list(Phi=Phi.spp,p=list(formula=~hab)), accumulate=FALSE)




fit.models=function(){
  Phi.constant=list(formula=~1)
  p.time=list(formula=~time)
  p.constant=list(formula=~1)
  p.spp=list(formula=~spp)
  p.spp_time=list(formula=~spp+time)
  p.spptime=list(formula=~spp*time)
  p.hab=list(formula=~hab)
  p.hab_spp=list(formula=~hab+spp)
  p.hab_spp_time=list(formula=~hab+spp+time)
  p.hab_time=list(formula=~hab+time)
  p.habtime=list(formula=~hab*time)
  p.habspp=list(formula=~hab*spp)
  p.hab_spptime=list(formula=~hab+spp*time)
  p.habspp_time=list(formula=~hab*spp+time)
  cml=create.model.list(c("Phi","p"))
  results=crm.wrapper(cml,data=df.proc,ddl=df.ddl,external=FALSE,accumulate=FALSE, hessian=T)
return(results)
}

models <- fit.models()

models

ggplot(data=models[[8]]$results$reals$p[,], aes(x=as.integer(time), y=estimate, factor=spp, colour=spp)) + 
  geom_point() + geom_line(aes(linetype=hab)) + geom_errorbar(aes(ymin=estimate-se, ymax=estimate+se), width=.1) +
  theme_classic() + scale_x_continuous(breaks=c(1:9),labels=c(2009:2017)) + xlab("")+ ylim(0,1) + 
  ylab("") + facet_wrap(~hab)



