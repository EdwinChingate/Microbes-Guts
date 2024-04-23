UpdateMenu <- function(arena,GeometricMatrix,reactor_id){
  Past_cellVolume <- GeometricMatrix[1,reactor_id-1]
  Current_cellVolume <-GeometricMatrix[1,reactor_id] 
  dilution_ratio <- Past_cellVolume/Current_cellVolume
  MetabolitesList <- arena@media
  for (metabolite in MetabolitesList){
    metabolite_id <- metabolite@id
    diffmat <- arena@media[[metabolite_id]]@diffmat
    diffmat <- diffmat*dilution_ratio
    arena@media[[metabolite_id]]@diffmat <- diffmat
  }
  return(arena)
}
