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
