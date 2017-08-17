####################################################################################################
##
## LTREB Survival Analysis - Bryant Dossman, Colin Studds, Peter P. Marra
##
####################################################################################################

## Libraries
library(tidyverse)

## Reading Data

band <- read.csv("./data/banding_all.csv")

group_by(band, SPECIES) %>% filter(BNDCREW=="LTREB") %>% summarise(n = n()) %>% arrange(desc(n))
