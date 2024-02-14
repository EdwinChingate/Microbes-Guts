library("BacArena")
library("sybil")
library ("readxl")
library("here")
source('Functions/Geometry.R')
source('Functions/Inoculation.R')
source('Functions/Menu.R')
source('Functions/MetabolitesAbsorbed.R')
source('Functions/Bioreactor.R')

Guts <- function(home,Parameters_folder='Parameters',Models_folder='MicroModel'){
  ModelsFolder <- paste(home,'/',Models_folder,sep='')
  #Add a function to verify that what we have in the folder is the same as in the excel table
  ParametersLocation <- paste(home,'/',Parameters_folder,'/','ControlPanel.xlsx',sep='')
  Parameters <- read_excel(ParametersLocation)
  ModelCommunityLocation <- paste(home,'/',Parameters_folder,'/',Parameters[1,2],sep='')
  MenuLocation <- paste(home,'/',Parameters_folder,'/',Parameters[2,2],sep='')
  Flow_rate <- as.numeric(Parameters[3,2])*1000/24 #cm3/h
  #Reactors <- c('Duodenum','Jejunum','Ileum','Large intestine')
  for (reactor_id in 1:4) {
    print(reactor_id)
    ParametersGeometry <- Geometry(reactor_id,Parameters)
    Width <- ParametersGeometry[2]
    GridSize <- ParametersGeometry[1]
    steps <- ParametersGeometry[3]
    Length <- ParametersGeometry[4] #cm
    Flow_area <- Width^2 #cm2
    Speed <- Flow_rate/Flow_area #cm/h
    Hydraulic_retention_time <- Length/Speed #h
    dt <- Hydraulic_retention_time/steps
    #Update parameters in the arena
    
    if (reactor_id==1){
      arena <- BacArena::Arena(n=GridSize,m=GridSize,Lx=Width,Ly=Width,tstep=dt) #Define the 2D space geometry
      arena <- Inoculation(ModelCommunityLocation,ModelsFolder,arena) #Add the microorganisms to the arena
      arena <- Menu(MenuLocation,arena)
      eval <- BacArena::simEnv(arena, sec_obj='mtf',time=1)
      steps <-steps-1
    }
    Absorption <- MetabolitesAbsorbed(MenuLocation)
    IDsabsorp <- Absorption[[reactor_id]]
    eval <- Bioreactor(steps,eval,IDsabsorb,GridSize)
  }
  return(eval)
}