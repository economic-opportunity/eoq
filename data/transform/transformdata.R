#---------------------------------------------------------------------------------------------------------#
#THIS SCRIPT COMBINES INPUT DATA TO PRODUCE THE ANLAYSIS DATASET
#---------------------------------------------------------------------------------------------------------#
#clear all
rm(list = ls())

#define working directory (personalize)
personal_dir<-"/Users/matthewstaiger/Google Drive EOQ"

#define directory for input code
code_dir<-"EOQ/Code/MS/eoq/data/input"
#defin directories for data
cps_dir<-"EOQ/Data/CPS"
bls_dir<-"EOQ/Data/BLS"
qwi_dir<-"EOQ/Data/QWI"
intermediate_dir<-"/EOQ/Data/intermediate"

#set working directory
setwd(file.path(personal_dir,code_dir))
getwd()

#install packages
install.packages('ipumsr')
install.packages("tidyverse")



cps<-read.csv(file=paste(file.path(personal_dir,intermediate_dir),"/cps.csv",sep=""),header=TRUE, sep=",",colClasses=c(county="character",state="character"))

bls<-read.csv(file=paste(file.path(personal_dir,intermediate_dir),"/bls.csv",sep=""),header=TRUE, sep=",",colClasses=c(county="character",state="character"))

all = dplyr::right_join(cps,bls,by=c("state","county","year"))

head(all)