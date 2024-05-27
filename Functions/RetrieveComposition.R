RetrieveComposition <- function(eval,GridSize,CellVolume,CellVolumeVec){
  Ncells <- GridSize^2
  NIngredients <-length(eval@media)
  IDsVec <- eval@media[[1]]@id
  ConcentrationProfiles <- getSubHist(eval,IDsVec)*1e3/(CellVolumeVec*Ncells)
  for (substance in 2:NIngredients){
    id <- eval@media[[substance]]@id
    IDsVec <- c(IDsVec,id)
    ConcentrationProfile <- getSubHist(eval,id)*1e3/(CellVolumeVec*Ncells)
    ConcentrationProfiles <- cbind(ConcentrationProfiles,ConcentrationProfile)
  }
  Concentrations_DF <- data.frame(ConcentrationProfiles)
  names(Concentrations_DF) <- IDsVec
  Concentrations_DF$TimeSteps <- 1:length(ConcentrationProfile)
  return(Concentrations_DF)
}