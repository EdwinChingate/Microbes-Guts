NutrientsAbsorption <- function(arena,Absorption,GeometricMatrix,reactor_id){
  #check that the metabolites in the diet are consumed by one of the microorganisms in the environment
  #this way is sensible to the number of steps, upgrade with an absorption rate where all nutrients are absorbed in the reactor  
 # last_step <- length(eval@medlist)
  #also evaluate the kind of object it is... avoid  logi(0) 
  # mode(eval@medlist[[2]][["EX_26dap_M(e)"]])=="logical"
  Hydraulic_retention_time <- GeometricMatrix[2,reactor_id]
  dt <- GeometricMatrix[5,reactor_id]
  IDsabsorp <- Absorption[reactor_id][[1]]
  for (id in IDsabsorp){
    AbsorptionEfficiency <- 0.9
    Ex_id <- paste('EX_',id,sep='')
    diffmat <- arena@media[[Ex_id]]@diffmat
    diffmat <- diffmat*(1-AbsorptionEfficiency)^(dt/Hydraulic_retention_time)
    arena@media[[Ex_id]]@diffmat <- diffmat
  }
  return(arena)
}
#GridSize is not necessary anymore. Also, this function should be updated when getting rid of eval
# this is how I can extract the concentration profiles inside the arena, as a a matrix, and modifity them arena2@media[["EX_ala_L(e)"]]@diffmat
