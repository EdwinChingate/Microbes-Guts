source('Functions/Menu.R')
source('Functions/UpdateMenu.R')
AdaptArena <- function(reactor_id,GeometricMatrix,MenuLocation,arena){
  CellVolume <- GeometricMatrix[1,reactor_id] 
  Hydraulic_retention_time <- GeometricMatrix[2,reactor_id] 
  MaxBiomass <- GeometricMatrix[3,reactor_id] 
  steps <- GeometricMatrix[4,reactor_id] 
  dt <- GeometricMatrix[5,reactor_id] 
  arena@tstep <- dt 
  arena@Lx <- Width 
  arena@Ly <- Width 
  if(reactor_id == 1){
    arena <- Menu(MenuLocation,arena,CellVolume)
  }else{
    arena <- UpdateMenu(arena,GeometricMatrix,reactor_id)
  }
  SpecsNumbers <- length(arena@specs)
  for (s in 1:SpecsNumbers){
    arena@specs[[s]]@maxweight <- MaxBiomass #I can replace it
  }
}
#Update the dimensions of the arena
