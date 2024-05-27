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

Guts <- function(home,Parameters_folder='Parameters',Models_folder='MicroModel',SaveResults=TRUE){
  ModelsFolder <- paste(home,'/',Models_folder,sep='')
  #Add a function to verify that what we have in the folder is the same as in the excel table
  ParametersLocation <- paste(home,'/',Parameters_folder,'/','ControlPanel.xlsx',sep='')
  Parameters <- read_excel(ParametersLocation)
  ModelCommunityLocation <- paste(home,'/',Parameters_folder,'/',Parameters[1,2],sep='')
  MenuLocation <- paste(home,'/',Parameters_folder,'/',Parameters[2,2],sep='')
  Flow_rate <- as.numeric(Parameters[3,2])*1000/24 #cm3/h
  MicrobeVolume <- as.numeric(Parameters[17,2]) #um3
  MicrobeMass <- as.numeric(Parameters[18,2])
  Reactors <- c('Duodenum','Jejunum','Ileum','Large intestine')
  ReactorSpaceLocation <- 'Stomach'
  StepsLocation <- 1
  HRTprogress <- 0
  Zposprogress <- 0
  ZPosition <- 0
  HRT <- 0
  for (reactor_id in 1:4) {
    print(reactor_id)
    ParametersGeometry <- Geometry(reactor_id,Parameters)
    Width <- ParametersGeometry[2] #cm
    GridSize <- ParametersGeometry[1]
    steps <- ParametersGeometry[3]
    Length <- ParametersGeometry[4] #cm
    Flow_area <- Width^2 #cm2
    Speed <- Flow_rate/Flow_area #cm/h
    Hydraulic_retention_time <- Length/Speed #h
    dt <- Hydraulic_retention_time/steps
    CellLengt <- dt*Speed
    width <- Width/GridSize #cm
    CellArea <- width**2 #cm2
    CellVolume <- CellArea*CellLengt*1e12 #um3
    MaxMicrobesNumber <- CellVolume/MicrobeVolume
    MaxBiomass <- MaxMicrobesNumber*MicrobeMass
    if (reactor_id==1){
      arena <- BacArena::Arena(n=GridSize,m=GridSize,Lx=Width,Ly=Width,tstep=dt) #Define the 2D space geometry
      arena <- Inoculation(ModelCommunityLocation,ModelsFolder,arena) #Add the microorganisms to the arena
      arena <- Menu(MenuLocation,arena,CellVolume)
      eval <- BacArena::simEnv(arena,time=1) #, sec_obj='mtf'
      CellVolumeVec <- CellVolume
      CellVolume0 <- CellVolume
      steps <-steps-1
    }    
    for (s in 1:steps){
      ReactorSpaceLocation <- c(ReactorSpaceLocation,Reactors[reactor_id])
      StepsLocation <- c(StepsLocation,length(ReactorSpaceLocation))
      HRTprogress <- HRTprogress + dt
      Zposprogress <- Zposprogress + dt*Speed
      HRT <- c(HRT,HRTprogress)
      ZPosition <- c(ZPosition,Zposprogress)
      CellVolumeVec <- c(CellVolumeVec,CellVolume)
    }

    #return(eval)
    eval@tstep <- dt
    eval@n <- GridSize
    eval@m <- GridSize
    eval@Lx <- Width
    eval@Ly <- Width
    SpecsNumbers <- length(eval@specs)
    for (s in 1:SpecsNumbers){
      eval@specs[[s]]@maxweight <- MaxBiomass
    }
    Absorption <- MetabolitesAbsorbed(MenuLocation)
    IDsabsorp <- Absorption[[reactor_id]] 
    #consider only the ones that can be consumed by the microbial community I need to add another function for it
    eval <- Bioreactor(steps,eval,IDsabsorp,GridSize,Hydraulic_retention_time,dt)
  }
  SpaceLocationInf <- data.frame('TimeSteps'=StepsLocation ,'Guts section'=ReactorSpaceLocation,'Longitudinal progress (cm)'= ZPosition, 'HRT (h)' = HRT)
  if(SaveResults){
    SaveProfiles(eval,SpaceLocationInf,CellVolume0,GridSize,MicrobeMass,CellVolumeVec)
  }
  return(eval)
}