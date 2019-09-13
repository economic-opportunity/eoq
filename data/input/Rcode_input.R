# THIS CODE INPUTS THE DATA

#define working directory (personalize)
code_dir<-"/Users/matthewstaiger/Google Drive EOQ/EOQ/Code/MS/eoq/data/input"

#defin directories for code
cps_dir<-"/Users/matthewstaiger/Google Drive EOQ/EOQ/Data/CPS"

#set working directory
setwd(file.path(code_dir))
getwd()

#install packages
install.packages('ipumsr')


# birng in data from CPS
setwd(file.path(cps_dir))
ddi <- read_ipums_ddi("cps_00016.dat")
data <- read_ipums_micro(ddi)
setwd(file.path(code_dir))