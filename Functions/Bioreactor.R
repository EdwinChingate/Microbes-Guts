source('Functions/NutrientsAbsorption.R')
Bioreactor <- function(steps,eval,IDsabsorp,GridSize){
  for (t in 1:steps){
    eval <- NutrientsAbsorption(eval,IDsabsorp,GridSize)
    eval <- BacArena::simEnv(eval,time=1) #, sec_obj='mtf'
  }  
  return(eval)
}

#Define Biomass maxweight for the Org objects. I can do this with a volume estimation for the cell and the bacteria