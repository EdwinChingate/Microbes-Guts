closeAllConnections()
rm(list=ls()) #to clean memory, better to always run it

home <- "~/LRZ Sync+Share/GitHubProjects/Codding/Microbes-Guts" #getwd()
setwd(home)
source('Functions/Guts.R')
eval <- Guts(home,Parameters_folder='ParametersRun',Models_folder='MicroModelsRun',SaveResults=TRUE) #Default values Parameters_folder='Parameters',Models_folder='MicroModel'


