closeAllConnections()
rm(list=ls()) #to clean memory, better to always run it

home <- "~/LRZ Sync+Share/GitHubProjects/Codding/Microbes-Guts" #getwd()
setwd(home)
source('Functions/Guts.R')
eval <- Guts(home,Parameters_folder='ParametersRun',Models_folder='MicroModelsRun',SaveResults=TRUE) #Default values Parameters_folder='Parameters',Models_folder='MicroModel'
MenuLocation <- paste(home,'/','ParametersRun','/InfantFormulaDiet.xlsx',sep='')
Absorption <- MetabolitesAbsorbed(MenuLocation)
IDsabsorp <- Absorption[[1]]             
Ex_id <-Ex_id <- paste('EX_',IDsabsorp[1],sep='') 
print()
#Evaluate each time for each reactor

IdealFlux <- function(eval,IDsabsorp,Hydraulic_retention_time,Length,Width,GridSize,steps)

IdealFlux <- function(eval,IDsabsorp,Hydraulic_retention_time,Length,Width,GridSize,steps){
  Cells_Number <- GridSize^2
  last_step <- length(eval@medlist)
  TRANSFER_AREA <- 4*Length*Width
  Transfer_area <- TRANSFER_AREA/steps
  dt <- Hydraulic_retention_time/steps
  AbsorbedSubstances_Number <- lenght(IDsabsorp)
  IdealVector_coefficient_area_dt <- matrix(0,nrow=AbsorbedSubstances_Number,ncol=1)
  counter <- 1
  for (id in IDsabsorp){
    Ex_id <- paste('EX_',id,sep='')
    TotalMass <- sum(eval@medlist[[last_step]][[Ex_id]])  
    TotalAbsorptionFlux <- TotalMass/(Hydraulic_retention_time*TRANSFER_AREA)
    FlatConcentration <- TotalMass/Cells_Number
    Ideal_mass_transfer_coefficient <- TotalAbsorptionFlux/FlatConcentration
    Ideal_coefficient_area_dt <- Ideal_mass_transfer_coefficient*Transfer_area*dt
    IdealVector_coefficient_area_dt[counter] <- Ideal_coefficient_area_dt
    counter <- counter+1
  }
  return(IdealVector_coefficient_area)
}


arena <- BacArena::Arena(n=2,m=1,Lx=1,Ly=1,tstep=1)
arena <- addSubs(object=arena, smax=10, mediac='er',addAnyway=TRUE,add=FALSE,unit='mmol/cell',difunc='pde')
