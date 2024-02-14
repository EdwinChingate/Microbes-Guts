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




