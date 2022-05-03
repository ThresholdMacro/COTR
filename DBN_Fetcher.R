# get data series from DB.Nomics 
library(rdbnomics)
library(tidyr)
library(dplyr)
library(lubridate)

Code_map<-read.csv("Data_maps/DBN.csv")

for (row in 1:nrow(Code_map)) {
    filter1 <- as.character(Code_map$Series_Code[row])
    df <- rdb(as.character(Code_map$Provider_ID[row]),as.character(Code_map$Series_Dataset[row]),id=filter1)
   df1<- assign(as.character(Code_map$Short_name[row]),df)
    write.csv(df1,file=paste0('Data/Janes/',as.character(Code_map$Type[row]),'/', as.character(Code_map$Short_name[row]), '.csv'),row.names = FALSE)
    
}




