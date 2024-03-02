Geometry <- function(reactor_id,Parameters){
  TotalLength <- sum(as.numeric(c(Parameters[7,2],Parameters[9,2],Parameters[11,2],Parameters[13,2])))
  AllSteps <- as.numeric(Parameters[4,2])
  Width <- as.numeric(Parameters[2*reactor_id + 4,2])
  Length <- as.numeric(Parameters[2*reactor_id + 5,2])
  LenghtFraction <- Length/TotalLength
  steps <- as.integer(LenghtFraction*AllSteps+1)
  return( as.numeric(c(Width,steps,Length)))
}
