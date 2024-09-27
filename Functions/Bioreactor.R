source('Functions/NutrientsAbsorption.R')
Bioreactor <- function(GeometricMatrix,reactor_id,Absorption,arena,SaveResults){
steps <- GeometricMatrix[4,reactor_id]
  for (t in 1:steps){
    arena <- NutrientsAbsorption(arena,Absorption,GeometricMatrix,reactor_id) #I can replace it
    eval <- BacArena::simEnv(arena,time=1) #, sec_obj='mtf' #I can replace it
    arena <- BacArena::getArena(eval)
  }  
  return(arena)
}
#Make IDsabsorb a table with the efficienty too
#Define Biomass maxweight for the Org objects. I can do this with a volume estimation for the cell and the bacteria
