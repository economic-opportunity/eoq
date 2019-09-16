#---------------------------------------------------------------------------------------------------------#
#THIS SCRIPT PRODUCES THE ANLAYSIS DATASET
#---------------------------------------------------------------------------------------------------------#
#clear all#clear all
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


bls<-read.csv(file=paste(file.path(personal_dir,bls_dir),"/California_All_Counties_Unemployment_Rate_2010-2019.csv",sep=""),header=TRUE, sep=",")

qwi<-read.csv(file=paste(file.path(personal_dir,qwi_dir),"/qwi_se_quarterly.csv",sep=""),header=TRUE, sep=",",colClasses=c(county="character",state="character"))



