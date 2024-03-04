UpdateMenu <- function(arena,GeometricMatrix,reactor_id){
  Past_cellVolume <- GeometricMatrix[1,reactor_id-1]
  Current_cellVolume <-GeometricMatrix[1,reactor_id] 
  dilution_ratio <- Past_cellVolume/Current_cellVolume
  for (metabolite in arena@media){
    metabolite_id <- metabolite@id
    arena@media[[metabolite_id]]@diffmat <- arena@media[[metabolite_id]]@diffmat*dilution_ratio
  }
  return(arena)
}
