library("BacArena")
library("sybil")
library ("readxl")
library("here")
library("writexl")
source('Functions/Geometry.R')
source('Functions/Inoculation.R')
source('Functions/Menu.R')
source('Functions/MetabolitesAbsorbed.R')
source('Functions/Bioreactor.R')
source('Functions/SaveProfiles.R')
source('Functions/AdaptArena.R')

Guts <- function(home,Parameters_folder='Parameters',Models_folder='MicroModel',SaveResults=TRUE){
  ModelsFolder <- paste(home,'/',Models_folder,sep='')
  #Add a function to verify that what we have in the folder is the same as in the excel table
  ParametersLocation <- paste(home,'/',Parameters_folder,'/','ControlPanel.xlsx',sep='')
  Parameters <- read_excel(ParametersLocation)
  ModelCommunityLocation <- paste(home,'/',Parameters_folder,'/',Parameters[1,2],sep='')
  MenuLocation <- paste(home,'/',Parameters_folder,'/',Parameters[2,2],sep='')
  

  GridSize <- as.numeric(Parameters[5,2]) #Update! not anymore
  Reactors <- c('Duodenum','Jejunum','Ileum','Large intestine')
  ReactorSpaceLocation <- 'Stomach'
  StepsLocation <- 1
  HRTprogress <- 0
  Zposprogress <- 0
  ZPosition <- 0
  HRT <- 0
  GeometricMatrix <- Geometry(Parameters)
  arena <- BacArena::Arena(n=GridSize,m=GridSize)
  arena <- Inoculation(ModelCommunityLocation,ModelsFolder,arena)
  for (reactor_id in 1:4) {
    print(reactor_id)

   # for (s in 1:steps){
  #    ReactorSpaceLocation <- c(ReactorSpaceLocation,Reactors[reactor_id])
  #    StepsLocation <- c(StepsLocation,length(ReactorSpaceLocation))
  #    HRTprogress <- HRTprogress + dt
  #    Zposprogress <- Zposprogress + dt*Speed
  #    HRT <- c(HRT,HRTprogress)
  #    ZPosition <- c(ZPosition,Zposprogress)
  #  }
    arena <-AdaptArena(reactor_id,GeometricMatrix,MenuLocation,arena)
    Absorption <- MetabolitesAbsorbed(MenuLocation)
    IDsabsorp <- Absorption[[reactor_id]] 
    #consider only the ones that can be consumed by the microbial community I need to add another function for it
    arena <- Bioreactor(steps,arena,IDsabsorp,GridSize,Hydraulic_retention_time,dt)
  }
  SpaceLocationInf <- data.frame('TimeSteps'=StepsLocation ,'Guts section'=ReactorSpaceLocation,'Longitudinal progress (cm)'= ZPosition, 'HRT (h)' = HRT)
  if(SaveResults){
    SaveProfiles(eval,SpaceLocationInf,CellVolume,GridSize,MicrobeMass)
  }
  return(eval)
}