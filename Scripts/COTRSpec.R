## ---------------------------
##
## Script name: 
## Purpose of script: COTRSpec
## Author: Meyrick Chapman
## Date Created: 2022-04-20
## Copyright (c) Hedge Analytics Ltd, 2022
## Email: meyrick@hedge-analytics.com
##
## ---------------------------
## Notes:
##   
## ---------------------------

## set working directory

## project directory = default 

## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------

## load up the packages we will need:  (uncomment as required)
library(tidyr)
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(readr)
library(ggpubr)
library(gridExtra)
library("grid")
library("ggplotify")
library(stringr)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory
source("~/Library/Mobile Documents/com~apple~CloudDocs/Meyrick/R/Scripts/ggstdplots.R")
## ---------------------------

SummaryNames <- readr::read_csv("Data/MarketNames.csv")
target <- c(SummaryNames$Code)

Spec <- readr::read_csv("Data/COTRSpec.csv")
Spec$Report_Date_as_MM_DD_YYYY<-as.Date(ymd(Spec$Report_Date_as_MM_DD_YYYY))
Spec<-arrange(Spec, Report_Date_as_MM_DD_YYYY)
df <- data.frame(Spec$Market_and_Exchange_Names,Spec$CFTC_Contract_Market_Code)
write_csv(df,"Data/mktcodes&names.csv")

# Spec$CFTC_Contract_Market_Code <- as.numeric(Spec$CFTC_Contract_Market_Code)
OUTOFDATE<-(Sys.Date()>(Spec$Report_Date_as_MM_DD_YYYY[nrow(Spec)]+10))

if(OUTOFDATE==TRUE) {
  url <- "https://www.cftc.gov/dea/newcot/deacom.txt"
  destfile <- "Data/latestFinCOTRSpec.txt"
  download.file(url, destfile)
  
  latest <-readr::read_csv(here::here("Data/latestFinCOTRSpec.txt"))
  names(latest)<-names(Spec)
  latest$Report_Date_as_MM_DD_YYYY <-
    as.Date(ymd(latest$Report_Date_as_MM_DD_YYYY))
  Spec <- rbind (Spec, latest)
  #  write.csv(unique(latest$Market_and_Exchange_Names),"Data/MarketNames.csv",row.names = FALSE)
}  

Spec<-arrange(Spec, Report_Date_as_MM_DD_YYYY)
write.csv(Spec,"Data/COTRSpec.csv", row.names = FALSE)
 
Spec <-
   Spec %>% 
   filter(Report_Date_as_MM_DD_YYYY>=ymd('2015-01-01'))


Spec$Comm_Net<-Spec$Comm_Positions_Long_All - Spec$Comm_Positions_Short_All
Spec$NonComm_Net<-Spec$NonComm_Positions_Long_All - Spec$NonComm_Positions_Short_All
Spec$NonRept_Net<-Spec$NonRept_Positions_Long_All - Spec$NonRept_Positions_Short_All

Spec$Pct_Comm_Net<-Spec$Pct_of_OI_Comm_Long_All - Spec$Pct_of_OI_Comm_Short_All
Spec$Pct_NonComm_Net<-Spec$Pct_of_OI_NonComm_Long_All - Spec$Pct_of_OI_NonComm_Short_All
Spec$Pct_NonRept_Net<-Spec$Pct_of_OI_NonRept_Long_All - Spec$Pct_of_OI_NonRept_Short_All

plot_list = list()

 Spec$CFTC_Contract_Market_Code <-  str_replace(Spec$CFTC_Contract_Market_Code, "^0+" ,"")
# target <-  str_replace(target,"^0+","")

for(series in target){
  
mkt <- Spec[which(Spec$CFTC_Contract_Market_Code == series),]

mktname <- mkt$Market_and_Exchange_Names[nrow(mkt)]
  mkt <- mkt[,c('Report_Date_as_MM_DD_YYYY',
                'Pct_NonComm_Net',
                'Pct_NonRept_Net')]
  
  colnames(mkt)[1] <- 'Date'
  colnames(mkt)[2] <- 'Large Spec'
  colnames(mkt)[3] <- 'Small Spec'
  
  mktlng <-
    mkt %>%
    pivot_longer(c(-Date), names_to = 'key', values_to = 'value')
  
  p <- ggstandard(mktlng,mktname,"source: CFTC", "2 years", "Net long/short: % of OI")
  
  plot_list[[mktname]] <- as.grob(p)
  ggsave(paste0("Spec_",mktname,".png"), path = "Output/Spec")
  
  
}

 mydir <- getwd()
 rmdfiles <- list.files(path=mydir, pattern="SpecPositioning.Rmd", full.names=FALSE)
 
 for(mdown in rmdfiles){
   print(mdown)
   rmarkdown::render(mdown, 
                     output_format = 'pdf_document')
 }
 