closeAllConnections()
rm(list=ls()) #to clean memory, better to always run it

home <- "~/LRZ Sync+Share/GitHubProjects/Codding/Microbes-Guts" #getwd()
setwd(home)
source('Functions/Guts.R')
eval <- Guts(home,Parameters_folder='ParametersRun',Models_folder='MicroModelsRun',SaveResults=TRUE) #Default values Parameters_folder='Parameters',Models_folder='MicroModel'

ParametersLocation <- paste(home,'/',Parameters_folder,'/','ControlPanel.xlsx',sep='')
Parameters_folder='ParametersRun'
Parameters <- read_excel(ParametersLocation)
Models_folder='MicroModelsRun'
MenuLocation <- paste(home,'/',Parameters_folder,'/',Parameters[2,2],sep='')
ModelsFolder <- paste(home,'/',Models_folder,sep='')
ModelCommunityLocation <- paste(home,'/',Parameters_folder,'/',Parameters[1,2],sep='')
arena <- BacArena::Arena(n=10,m=10,Lx=1,Ly=1,tstep=1)
arena <- Inoculation(ModelCommunityLocation,ModelsFolder,arena)
arena <- Menu(MenuLocation,arena,CellVolume=1)
eval <- BacArena::simEnv(arena,time=4)
arena2 <- BacArena::getArena(eval)
