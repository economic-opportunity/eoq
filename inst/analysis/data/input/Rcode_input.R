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
bls_dir<-"EOQ/Data/BLS"
qwi_dir<-"EOQ/Data/QWI"
intermediate_dir<-"/EOQ/Data/intermediate"
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
#subset data to population of interest
cps<-dplyr::filter(cps,EMPSTAT==10)
cps<-dplyr::filter(cps,INCWAGE!=9999999,INCWAGE!=9999998,INCWAGE!=0)
cps<-cps[ which(STATEFIP=="6" & nchar(COUNTY)>=4) , ]
cps$STATEFIP <- sub("^", "0", cps$STATEFIP)
cps$COUNTY <- substr(cps$COUNTY,2,4)
#rename variables
cps<-dplyr::rename(cps,state=STATEFIP)
cps<-dplyr::rename(cps,county=COUNTY)
cps<-dplyr::rename(cps,year=YEAR)
cps<-dplyr::rename(cps,month=MONTH)
cps<-dplyr::rename(cps,incwage=INCWAGE)
cps<-dplyr::rename(cps,occ=OCC)
cps<-dplyr::rename(cps,ind=IND)
cps<-dplyr::rename(cps,age=AGE)
cps<-dplyr::rename(cps,sex=SEX)
cps<-dplyr::rename(cps,race=RACE)
#save final data
finalvars<-c("state","county","year","month","incwage","occ","ind","age","sex","race","CPSIDP")
cps_final<-cps[finalvars]
write.csv(cps_final, file = paste(file.path(personal_dir,intermediate_dir),"/cps.csv",sep=""),row.names=FALSE)
detach(cps)


#---------------------------------------------------------------------------------------------------------#
#UNEMPLOYMENT DATA FROM BLS
#---------------------------------------------------------------------------------------------------------#
#bring in data
bls<-read.csv(file=paste(file.path(personal_dir,bls_dir),"/annual averages/laucnty18.csv",sep=""),header=TRUE, sep=",",colClasses=c(county="character",state="character"))
attach(bls)
bls<-bls[ which(state=="06") , ]
#save final data
finalvars<-c("state","county","year","unemployment_rate")
bls_final<-bls[finalvars]
write.csv(bls_final, file = paste(file.path(personal_dir,intermediate_dir),"/bls.csv",sep=""),row.names=FALSE)
detach(bls)


#---------------------------------------------------------------------------------------------------------#
#QWI
#---------------------------------------------------------------------------------------------------------#
#bring in data
qwi<-read.csv(file=paste(file.path(personal_dir,qwi_dir),"/qwi_se_quarterly.csv",sep=""),header=TRUE, sep=",",colClasses=c(county="character",state="character"))

#save final data
finalvars<-c("state","county","year","HirAEndR")
