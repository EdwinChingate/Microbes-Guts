source('Functions/NutrientsAbsorption.R')
Bioreactor <- function(steps,eval,IDsabsorp,GridSize){
  for (t in 1:steps){
    #   print(t)
    # eval <- NutrientsAbsorption(eval,IDsabsorp,GridSize)
    eval <- BacArena::simEnv(eval, sec_obj='mtf',time=1)
  }  
  return(eval)
}

#Define Biomass maxweight for the Org objects. I can do this with a volume stimation for the cell and the bacteria