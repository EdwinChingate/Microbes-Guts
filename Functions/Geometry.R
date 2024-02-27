Geometry <- function(reactor_id,Parameters){
  TotalLength <- sum(as.numeric(c(Parameters[7,2],Parameters[10,2],Parameters[13,2],Parameters[16,2])))
  AllSteps <- as.numeric(Parameters[4,2])
  GridSize <- round(as.numeric(Parameters[3*reactor_id+2,2]))
  Width <- as.numeric(Parameters[3*reactor_id + 3,2])
  Length <- as.numeric(Parameters[3*reactor_id + 4,2])
  LenghtFraction <- Length/TotalLength
  steps <- as.integer(LenghtFraction*AllSteps+1)
  return( as.numeric(c(GridSize,Width,steps,Length)))
}
