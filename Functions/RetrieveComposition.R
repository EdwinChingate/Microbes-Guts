RetrieveComposition <- function(eval,CellVolume){
  NIngredients <-length(eval@media)
  IDsVec <- eval@media[[1]]@id
  ConcentrationProfiles <- getSubHist(eval,IDsVec)
  for (substance in 2:NIngredients){
    id <- eval@media[[substance]]@id
    IDsVec <- c(IDsVec,id)
    ConcentrationProfile <- getSubHist(eval,id)*1e3/CellVolume
    ConcentrationProfiles <- cbind(ConcentrationProfiles,ConcentrationProfile)
  }
  Concentrations_DF <- data.frame(ConcentrationProfiles)
  names(Concentrations_DF) <- IDsVec
  Concentrations_DF$TimeSteps <- 1:length(ConcentrationProfile)
  return(Concentrations_DF)
}

# I can walk all the substances inside every new arena with the diff mat arena2@media[["EX_ala_L(e)"]]@diffmat, sum and get the avaraege value
# I also need a vector with the list of IDs arena@mediac[[1]]
# I need to be sure about the number of steps in the simulation before going throw the bioreactor, so  I would have to create a table with all the specific parameters before I run the simulation
