Menu <- function(MenuLocation,arena,CellVolume){
  menuTable <- read_excel(MenuLocation)
  NIngredients <-nrow(menuTable)
  for (substance in 1:NIngredients){
    id <- as.character(c(paste('EX_',menuTable[substance,'id'],sep='')))
    smax <- as.numeric(c((menuTable[substance,'Concentration (mM)'])))
    if (smax >0){
    arena <- addSubs(object=arena, smax=smax*CellVolume/1e3, mediac=id,addAnyway=TRUE,add=FALSE,unit='fmol/cell',difunc='pde')
    }
  }
  return(arena)
}