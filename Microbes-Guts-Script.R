library("BacArena")
library("sybil")
library ("readxl")
library("here")

#source('funciones.R')


Inoculation <- function(ModelCommunityLocation,ModelsFolder,arena) {
  ModelCommunity <- read_excel(ModelCommunityLocation)
  NSpecies <- nrow(ModelCommunity)  
  for (bacteriaPosition in 1:NSpecies){
    bacteria <- as.character(ModelCommunity[bacteriaPosition,1])
    amount <- ModelCommunity[bacteriaPosition,1]
    ModelName <- paste(bacteria,'.mat',sep='')
    print(ModelName)
    ModelLocation=paste(ModelsFolder,'/',bacteria,'.mat',sep='')
    #Read the model .mat
    BacMetabolicMod <-BacArena::readMATmod(ModelLocation)
    #CConvert into BacArena object
    BacBacAr <-BacArena::Bac(BacMetabolicMod) #here I can add the max weight.. wait! not really
    arena <- addOrg(arena,BacBacAr,amount=1) #Include the actual number, and revise the max biomass
    
  }
  return (arena)
}



Menu <- function(MenuLocation,arena){
  menuTable <- read_excel(MenuLocation)
  NIngredients <-nrow(menuTable)
  for (substance in 1:NIngredients){
    id <- as.character(c(paste('EX_',menuTable[substance,'id'],sep='')))
    smax <- as.numeric(c((menuTable[substance,'Concentration (mM)'])))
    arena <- addSubs(object=arena, smax=smax, mediac=id,addAnyway=TRUE,add=FALSE,unit='mM',difunc='pde')
  }
  return(arena)
}

MetabolitesAbsorbed <- function(MenuLocation){
  menuTable <- read_excel(MenuLocation)
  ID_Absorb_Duodenum <- menuTable['id'][menuTable['Duodenum']==1]
  ID_Absorb_Jejunum <- menuTable['id'][menuTable['Jejunum']==1]
  ID_Absorb_Ilenum <- menuTable['id'][menuTable['Ileum']==1]
  ID_Absorb_LargeIntestine <- menuTable['id'][menuTable['Large intestine']==1]  
  return(list(ID_Absorb_Duodenum,ID_Absorb_Jejunum,ID_Absorb_Ilenum,ID_Absorb_LargeIntestine))
}


Geometry <- function(reactor_id,Parameters){
  GridSize <- Parameters[4*reactor_id,2]
  Width <- Parameters[4*reactor_id + 1,2]
  steps <- Parameters[4*reactor_id + 2,2]
  Length <- Parameters[4*reactor_id + 3,2]
  return( as.numeric(c(GridSize,Width,steps,Length)))
}

NutrientsAbsorption <- function(eval,IDsabsorp,GridSize){
  #check that the metabolites in the diet are consumed by one of the microorganisms in the environment
  #this way is sensible to the number of steps, upgrade with an absorption rate where all nutrients are absorbed in the reactor  
  last_step <- length(eval@medlist)
  for (id in IDsabsorp){
    eval@medlist[[last_step]][[id]][ 1:(GridSize-1)] <- 0 
    eval@medlist[[last_step]][[id]][GridSize*(1:(GridSize-1))+1] <- 0
    eval@medlist[[last_step]][[id]][GridSize*(1:GridSize)] <- 0
    eval@medlist[[last_step]][[id]][2:(GridSize-1)+(GridSize*(GridSize-1))] <- 0
  }
  return(eval)
}

Bioreactor <- function(steps,eval,IDsabsorp,GridSize){
  for (t in 1:steps){
    #   print(t)
    # eval <- NutrientsAbsorption(eval,IDsabsorp,GridSize)
    eval <- BacArena::simEnv(eval, sec_obj='mtf',time=1)
  }  
  return(eval)
}


#Define Biomass maxweight for the Org objects. I can do this with a volume stimation for the cell and the bacteria


home <- "/home/edwin/Downloads/Neonates_microbiome_modelling/Data/Model" #getwd()
ModelsFolder <- paste(home,'/',"MicroModel",sep='')
#Add a function to verify that what we have in the folder is the same as in the excel table
ParametersLocation <- paste(home,'/','ControlPanel.xlsx',sep='')
Parameters <- read_excel(ParametersLocation)
ModelCommunityLocation <- paste(home,'/',Parameters[1,2],sep='')
MenuLocation <- paste(home,'/',Parameters[2,2],sep='')

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
  eval <- BacArena::simEnv(arena, sec_obj='mtf',time=1)
  steps <-steps-1
}
arena <- Menu(MenuLocation,arena)
Absorption <- MetabolitesAbsorbed(MenuLocation)
IDsabsorp <- Absorption[[reactor_id]]
eval <- Bioreactor(steps,eval,IDsabsorb,GridSize)
}










#Cargando las sustancias





#write.csv(menu, file = "my_data.csv")



reactor <- "duodeno"
IDsabsorber=list()



for (id in IDsabsorber){
 print(id)
}



pasos_duodeno <- 1 #Define la longitud del reactor

eval <- BacArena::simEnv(arena, time=10)




re <- getVarSubs(eval)

plotCurves2(eval)


arenita <- arena
arenita <- Inoculation(ModelCommunityLocation,ModelsFolder,arenita) #Add the microorganisms to the arena
eval <- BacArena::simEnv(arenita, time=3)

reactor <- "yeyuno"


IDsabsorber=list()
for (sustancia in 1:NIngredientes){
  id <- as.character(c(paste('EX_',menu[sustancia,'id'],sep='')))
  absorber=as.numeric(c((menu[sustancia,reactor])))
  if (absorber==1){
    IDsabsorber <- append(IDsabsorber,id)
  }
}

pasos_yeyuno <- 10 #Define la longitud del reactor

for (t in 1:(pasos_yeyuno-1)){
  print(t)
  ultimo_paso <- length(eval@medlist)
  
  for (id in IDsabsorber){
    eval@medlist[[ultimo_paso]][[id]][ 10*1:9+1] <- 0 #cambia con el tamano de la red
    eval@medlist[[ultimo_paso]][[id]][ 10*1:9] <- 0
    eval@medlist[[ultimo_paso]][[id]][ 1:9] <- 0
    eval@medlist[[ultimo_paso]][[id]][ 92:100] <- 0
  }
  eval <- BacArena::simEnv(eval, time=1)
}

getVarSubs(eval)

plotCurves2(eval)


reactor <- "ileon"


IDsabsorber=list()
for (sustancia in 1:NIngredientes){
  id <- as.character(c(paste('EX_',menu[sustancia,'id'],sep='')))
  absorber=as.numeric(c((menu[sustancia,reactor])))
  if (absorber==1){
    IDsabsorber <- append(IDsabsorber,id)
  }
}

pasos_ileon <- 10 #Define la longitud del reactor

for (t in 1:(pasos_ileon-1)){
  print(t)
  ultimo_paso <- length(eval@medlist)
  
  for (id in IDsabsorber){
    eval@medlist[[ultimo_paso]][[id]][ 10*1:9+1] <- 0 #cambia con el tamano de la red
    eval@medlist[[ultimo_paso]][[id]][ 10*1:9] <- 0
    eval@medlist[[ultimo_paso]][[id]][ 1:9] <- 0
    eval@medlist[[ultimo_paso]][[id]][ 92:100] <- 0
  }
  eval <- BacArena::simEnv(eval, time=1)
}

getVarSubs(eval)

plotCurves2(eval)



reactor <- "grueso"


IDsabsorber=list()
for (sustancia in 1:NIngredientes){
  id <- as.character(c(paste('EX_',menu[sustancia,'id'],sep='')))
  absorber=as.numeric(c((menu[sustancia,reactor])))
  if (absorber==1){
    IDsabsorber <- append(IDsabsorber,id)
  }
}

pasos_grueso <- 20 #Define la longitud del reactor

for (t in 1:(pasos_grueso-1)){
  print(t)
  ultimo_paso <- length(eval@medlist)
  
  for (id in IDsabsorber){
    eval@medlist[[ultimo_paso]][[id]][ 10*1:9+1] <- 0 #cambia con el tamano de la red
    eval@medlist[[ultimo_paso]][[id]][ 10*1:9] <- 0
    eval@medlist[[ultimo_paso]][[id]][ 1:9] <- 0
    eval@medlist[[ultimo_paso]][[id]][ 92:100] <- 0
  }
  eval <- BacArena::simEnv(eval, time=1)
}

getVarSubs(eval)

plotCurves2(eval)

#excel resultados