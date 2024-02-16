NutrientsAbsorption <- function(eval,IDsabsorp,GridSize){
  #check that the metabolites in the diet are consumed by one of the microorganisms in the environment
  #this way is sensible to the number of steps, upgrade with an absorption rate where all nutrients are absorbed in the reactor  
  last_step <- length(eval@medlist)
  #also evaluate the kind of object it is... avoid  logi(0) 
  for (id in IDsabsorp){
    Ex_id <- paste('EX_',id,sep='')
    eval@medlist[[last_step]][[Ex_id]][ 1:(GridSize-1)] <- 0 
    eval@medlist[[last_step]][[Ex_id]][GridSize*(1:(GridSize-1))+1] <- 0
    eval@medlist[[last_step]][[Ex_id]][GridSize*(1:GridSize)] <- 0
    eval@medlist[[last_step]][[Ex_id]][2:(GridSize-1)+(GridSize*(GridSize-1))] <- 0
  }
  return(eval)
}