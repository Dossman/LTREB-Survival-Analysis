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
  summarise(n=n()) %>% 
  filter(n > 200) %>% 
  select(SPECIES)

select <- as.character(select$SPECIES)


df <- band %>% filter(SPECIES %in% select, BNDCREW=="LTREB") %>% 
  group_by(BAND, YEAR) %>% 
  select(SPECIES, BAND, YEAR)

df <- df[!duplicated(df),]

df$detect = 1

df$status <- ifelse(df$SPECIES %in% c("AMRE","BAWW","NOWA","OVEN","PRAW","SWWA","SWWA"), "Migrant","Resident")

df <- melt(df,id.var=c("SPECIES","BAND","YEAR", "status"),measure.var="detect") %>%
  cast(SPECIES + status+BAND ~YEAR)

df[is.na(df)]=0

df <- data.frame(ch=paste(df[,4],df[,5],df[,6],df[,7],df[,8],df[,9],df[,10],df[,11], df[,12], df[,13], sep=""), 
                 status = df$status, spp = df$SPECIES)
df$ch <- as.character(df$ch) 
df$spp <- as.factor(as.character(df$spp))
df$status <- as.factor(as.character(df$status))

#df <- df[-which(df$ch == "0000000001"),]  #removing all entries of birds banded in the final years



df.proc=process.data(df)

df.ddl=make.design.data(df.proc)

Phi.spp=list(formula=~status)
model=crm(df.proc,df.ddl,model.parameters=list(Phi=Phi.spp), accumulate=FALSE)




fit.models=function(){
  Phi.spp=list(formula=~spp*time*status)
  Phi.time=list(formula=~time+spp)
  p.sex=list(formula=~spp*time)
  p.dot=list(formula=~1)
  cml=create.model.list(c("Phi","p"))
  results=crm.wrapper(cml,data=df.proc,ddl=df.ddl,external=FALSE,accumulate=FALSE, hessian=T)
return(results)
}

models <- fit.models()

models[[1]]

ggplot(data=models[[1]]$results$reals$Phi[,], aes(x=as.integer(time), y=estimate, factor=spp, colour=spp)) + 
  geom_point() + geom_path() #+ geom_errorbar(aes(ymin=estimate-se, ymax=estimate+se), width=.1) +
  theme_classic() + scale_x_continuous(breaks=c(1:10),labels=c(2008:2017)) + xlab("")



