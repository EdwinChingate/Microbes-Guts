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