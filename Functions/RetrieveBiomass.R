RetrieveBiomass <-function(eval){
  Tsteps <- length(eval@simlist)
  NSpecs <- length(eval@specs)
  Biomass_matrix <- matrix(0, nrow=Tsteps,ncol=NSpecs)
  SpecNames <- eval@specs[[1]]@type
  for (Mspec in 2:NSpecs){
    SpecNames  <- c(SpecNames,eval@specs[[Mspec]]@type)
  }
  for (moment in 1:Tsteps){
    StepBiomass <- eval@simlist[[moment]]
    for (Mspec in 1:NSpecs){
      biomass <- sum(StepBiomass['biomass'][StepBiomass['type']==Mspec])
      Biomass_matrix[moment,Mspec] <- biomass
    }
  }
  Biomass_DF <- data.frame(Biomass_matrix)
  names(Biomass_DF) <- SpecNames
  return(Biomass_DF)
}
