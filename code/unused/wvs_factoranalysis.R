### ----------------------------------------------------------------------------
# The behavioral consequences of election fraud for democratic consolidation 
# A survey experiment in Venezuela and Russia
# Lion Behrens and Viktoriia Semenova
# Reproduction file
### ----------------------------------------------------------------------------

library(readstata13) # to load .dta files
library(dplyr)       # for data manipulation
library(sjlabelled)  # to handle variable labels
library(stringr)     # for strings

# load data  
wvs6 <- readRDS("wvs6.RDS")
gwf <- read.dta13("GWF_AllPoliticalRegimes.dta")
gwf <- gwf[gwf$year == 2010,]
gwf$gwf_country[which(gwf$gwf_country=="USA")] <- "United States"

# data preparation
wvs6$V2 <- as_label(wvs6$V2)

wvs6_inst <- 
  wvs6[,c("V2",
          "V109", # armed forces
          "V113", # the police
          "V114", # the courts
          "V115", # the government
          "V116", # political parties
          "V117"  # parliament 
          )
       ]

wvs6_inst <- wvs6_inst %>% 
  mutate(V109 = replace(V109, which(V109<0), NA)) %>% 
  mutate(V113 = replace(V113, which(V113<0), NA)) %>% 
  mutate(V114 = replace(V114, which(V114<0), NA)) %>% 
  mutate(V115 = replace(V115, which(V115<0), NA)) %>% 
  mutate(V116 = replace(V116, which(V116<0), NA)) %>% 
  mutate(V117 = replace(V117, which(V117<0), NA)) 

wvs6_inst <- wvs6_inst[complete.cases(wvs6_inst),] # listwise deletion

# classification democracy vs. autocracy
wvs6_auto <- wvs6_inst[is.element(wvs6_inst$V2, 
                                  gwf$gwf_country[which(gwf$gwf_nonautocracy == "NA")]), ]
wvs6_demo <- wvs6_inst[is.element(wvs6_inst$V2, 
                        gwf$gwf_country[which(gwf$gwf_nonautocracy == "democracy")]), ]

# maximum likelihood exploratory factor analysis
fa_auto <- factanal(wvs6_auto[,-1], 1, rotation="varimax")
fa_demo <- factanal(wvs6_demo[,-1], 1, rotation="varimax")



