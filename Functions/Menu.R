Menu <- function(MenuLocation,arena,CellVolume){
  menuTable <- read_excel(MenuLocation)
  NIngredients <-nrow(menuTable)
  for (substance in 1:NIngredients){
    substanceID=menuTable[substance,'id']
    id <- as.character(c(paste('EX_',substanceID,sep='')))
    smax <- as.numeric(c((menuTable[substance,'Concentration (mM)'])))
    CellSmax <- smax*CellVolume/1e3
    if (smax >0){
    arena <- addSubs(object=arena, smax=CellSmax, mediac=id,addAnyway=TRUE,add=FALSE,unit='fmol/cell',difunc='pde')
    }
  }
  return(arena)
}