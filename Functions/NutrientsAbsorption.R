NutrientsAbsorption <- function(eval,IDsabsorp,GridSize,Hydraulic_retention_time,dt,AbsorptionEfficiency=0.9){
  #check that the metabolites in the diet are consumed by one of the microorganisms in the environment
  #this way is sensible to the number of steps, upgrade with an absorption rate where all nutrients are absorbed in the reactor  
  last_step <- length(eval@medlist)
  #also evaluate the kind of object it is... avoid  logi(0) 
  # mode(eval@medlist[[2]][["EX_26dap_M(e)"]])=="logical"
  for (id in IDsabsorp){
    Ex_id <- paste('EX_',id,sep='')
    eval@medlist[[last_step]][[Ex_id]] <- (eval@medlist[[last_step]][[Ex_id]])*(1-AbsorptionEfficiency)^(dt/Hydraulic_retention_time)
  }
  return(eval)
}