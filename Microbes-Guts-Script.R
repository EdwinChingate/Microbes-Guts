closeAllConnections()
rm(list=ls()) #to clean memory, better to always run it

home <- "/home/edwin/0-GitHubProjects/Codding/Microbes-Guts-Simulator" #getwd()
setwd(home)
source('Functions/Guts.R')
eval <- Guts(home,Parameters_folder='Parameters',Models_folder='MicroModel',SaveResults=TRUE) #Default values Parameters_folder='Parameters',Models_folder='MicroModel'
