RetrieveBiomass <-function(eval,CellVolume,GridSize,MicrobeMass){
  Ncells <- GridSize^2
  Tsteps <- length(eval@simlist)
  NSpecs <- length(eval@specs)
  Biomass_matrix <- matrix(0, nrow=Tsteps,ncol=NSpecs)
  SpecNames <- ''
  for (Mspec in 1:NSpecs){
    SpecNames  <- c(SpecNames,eval@specs[[Mspec]]@type)
  }
  for (moment in 1:Tsteps){
    StepBiomass <- eval@simlist[[moment]]
    for (Mspec in 1:NSpecs){
      biomass <- sum(StepBiomass['biomass'][StepBiomass['type']==Mspec])/(CellVolume*Ncells)*1e12 #Biomass concentration pg/mL
      Biomass_matrix[moment,Mspec] <- biomass/MicrobeMass #Biomass concentration cells/mL
    }
  }
  Biomass_DF <- data.frame(Biomass_matrix)
  names(Biomass_DF) <- SpecNames[2:(NSpecs+1)]
  Biomass_DF$TimeSteps <- 1:Tsteps
  return(Biomass_DF)
}
