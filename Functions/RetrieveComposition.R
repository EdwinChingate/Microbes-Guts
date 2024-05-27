RetrieveComposition <- function(eval,CellVolume,CellVolumeVec){
  NIngredients <-length(eval@media)
  IDsVec <- eval@media[[1]]@id
  ConcentrationProfiles <- getSubHist(eval,IDsVec)*10/CellVolumeVec
  for (substance in 2:NIngredients){
    id <- eval@media[[substance]]@id
    IDsVec <- c(IDsVec,id)
    ConcentrationProfile <- getSubHist(eval,id)*10/CellVolumeVec
    ConcentrationProfiles <- cbind(ConcentrationProfiles,ConcentrationProfile)
  }
  Concentrations_DF <- data.frame(ConcentrationProfiles)
  names(Concentrations_DF) <- IDsVec
  Concentrations_DF$TimeSteps <- 1:length(ConcentrationProfile)
  return(Concentrations_DF)
}