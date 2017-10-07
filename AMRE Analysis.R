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

## Creating the Capture History

df <- band %>% filter(SPECIES == "AMRE", BNDCREW=="LTREB") %>% 
  group_by(BAND, YEAR) %>%
  select(BAND, YEAR, HABITAT, AGE, SEX, CAPMAS, CAPFAT, BILLEN, BILLDEP, BILLWID,WING,TARSUS,TAIL)

df %>% group_by(BAND) %>% summarise(n=n()) %>% arrange(desc(n
                                                                 ))

df <- df[!duplicated(df[,1:2]),]

df$detect = 1

df <- melt(df,id.var=c("BAND","YEAR"),measure.var="detect") %>%
  cast(BAND~YEAR)

ch <- df[,2:11]
ch[is.na(ch)]=0

df[,2:11] <- ch

df <- data.frame(ch=paste(df[,2],df[,3],df[,4],df[,5],df[,6],df[,7],df[,8], df[,9], df[,10], df[,11], sep=""), 
                 band=df$BAND)
df$ch <- as.character(df$ch)

## 
df.proc=process.data(df)

df.ddl=make.design.data(df.proc)
