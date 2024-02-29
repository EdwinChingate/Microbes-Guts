source('Functions/NutrientsAbsorption.R')
Bioreactor <- function(steps,eval,IDsabsorp,GridSize,Hydraulic_retention_time,dt,AbsorptionEfficiency=0.9){
  for (t in 1:steps){
    eval <- NutrientsAbsorption(eval,IDsabsorp,GridSize,Hydraulic_retention_time,dt,AbsorptionEfficiency) #I can replace it
    eval <- BacArena::simEnv(eval,time=1) #, sec_obj='mtf' #I can replace it
  }  
  return(eval)
}

#Define Biomass maxweight for the Org objects. I can do this with a volume estimation for the cell and the bacteria