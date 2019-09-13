#THIS CODE INPUTS THE DATA
rm(list = ls())

#define working directory (personalize)
code_dir<-"/Users/matthewstaiger/Google Drive EOQ/EOQ/Code/MS/eoq/data/input"

#defin directories for code
cps_dir<-"/Users/matthewstaiger/Google Drive EOQ/EOQ/Data/CPS"

#set working directory
setwd(file.path(code_dir))
getwd()

#install packages
install.packages('ipumsr')
install.packages("tidyverse")




#---------------------------------------------------------------------------------------------------------#
#CPS: birng in and clean data from CPS
#---------------------------------------------------------------------------------------------------------#
#bring in data
setwd(file.path(cps_dir))
cps<-read.csv(file="cps_00018.csv",colClasses=c(COUNTY="character",STATEFIP="character"),header=TRUE, sep=",")
setwd(file.path(code_dir))
attach(cps)
#clean data
cps<-cps[ which(STATEFIP=="6" & nchar(COUNTY)>=4) , ]
cps$STATEFIP <- sub("^", "0", cps$STATEFIP)
cps$COUNTY <- sub("^", "0", cps$COUNTY)
#rename variables
colnames(cps)[colnames(cps)=="STATEFIP"] <- "STATE"
#save final data
finalvars<-c("STATE","COUNTY")
cps_final<-cps[finalvars]
detach(cps)