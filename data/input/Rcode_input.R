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
setwd(file.path(cps_dir))
cps<-read.csv(file="cps_00018.csv",colClasses=c(COUNTY="character"),header=TRUE, sep=",")
setwd(file.path(code_dir))
attach(cps)
cps<-cps[ which(STATEFIP=="6" & nchar(COUNTY)==4) , ]
cps$COUNTY <- sub("^", "0", cps$COUNTY)
detach(cps)




