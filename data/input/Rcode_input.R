#---------------------------------------------------------------------------------------------------------#
#THIS SCRIPT INPUTS INDIVIDUAL LEVEL DATA FROM THE CPS
#---------------------------------------------------------------------------------------------------------#
#clear all
rm(list = ls())

#define working directory (personalize)
personal_dir<-"/Users/matthewstaiger/Google Drive EOQ"

#define directory for input code
code_dir<-"EOQ/Code/MS/eoq/data/input"
#defin directories for data
cps_dir<-"EOQ/Data/CPS"
intermediate_dir<-"EOQ/Data/intermediate"
#define directory for R packages
packages_dir<-"EOQ/Code/MS/eoq/renv"
  
#set working directory
setwd(file.path(personal_dir,code_dir))
getwd()

#package managements
renv::restore()

#---------------------------------------------------------------------------------------------------------#
#CPS: birng in and clean data from CPS
#---------------------------------------------------------------------------------------------------------#
#bring in data
cps<-read.csv(file=paste(file.path(personal_dir,cps_dir),"/cps_00018.csv",sep=""),colClasses=c(COUNTY="character",STATEFIP="character"),header=TRUE, sep=",")
attach(cps)
#clean data
cps<-cps[ which(STATEFIP=="6" & nchar(COUNTY)>=4) , ]
cps$STATEFIP <- sub("^", "0", cps$STATEFIP)
cps$COUNTY <- substr(cps$COUNTY,2,4)
#rename variables
cps<-dplyr::rename(cps,state=STATEFIP)

colnames(cps)[colnames(cps)=="STATEFIP"] <- "state"
colnames(cps)[colnames(cps)=="COUNTY"] <- "county"
colnames(cps)[colnames(cps)=="YEAR"] <- "year"
#save final data
finalvars<-c("state","county","year")
cps_final<-cps[finalvars]
write.csv(cps_final, file = paste(file.path(personal_dir,intermediate_dir),"/cps.csv",sep=""))
detach(cps)
