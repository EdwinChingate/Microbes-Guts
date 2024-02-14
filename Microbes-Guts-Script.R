closeAllConnections()
rm(list=ls())

home <- "~/LRZ Sync+Share/GitHubProjects/Codding/Microbes-Guts" #getwd()
setwd(home)
source('Functions/Guts.R')
eval <- Guts(home) #LMFP
