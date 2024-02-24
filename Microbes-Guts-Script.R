closeAllConnections()
rm(list=ls()) #to clean memory, better to always run it

home <- "~/LRZ Sync+Share/GitHubProjects/Codding/Microbes-Guts" #getwd()
setwd(home)
source('Functions/Guts.R')
eval <- Guts(home,Parameters_folder='ParametersRun',Models_folder='MicroModelsRun',SaveResults=FALSE) #Default values Parameters_folder='Parameters',Models_folder='MicroModel'
MenuLocation <- paste(home,'/','ParametersRun','/InfantFormulaDiet.xlsx',sep='')
Absorption <- MetabolitesAbsorbed(MenuLocation)
IDsabsorp <- Absorption[[1]]             
Ex_id <-Ex_id <- paste('EX_',IDsabsorp[1],sep='') 
print(IDsabsorp)
GridSize <- 10
Hydraulic_retention_time <- 10
dt <- 1
source('Functions/NutrientsAbsorption.R')
eval<-NutrientsAbsorption(eval,IDsabsorp,GridSize,Hydraulic_retention_time,dt,AbsorptionEfficiency=0.9)

#corner for checking the units
arena <- BacArena::Arena(n=2,m=1,Lx=1,Ly=1,tstep=1)
arena <- addSubs(object=arena, smax=10, mediac='er',addAnyway=TRUE,add=FALSE,unit='mmol/cell',difunc='pde')
