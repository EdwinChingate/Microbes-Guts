Geometry <- function(reactor_id,Parameters){
  GridSize <- round(as.numeric(Parameters[4*reactor_id,2]))
  Width <- as.numeric(Parameters[4*reactor_id + 1,2])
  steps <- round(as.numeric(Parameters[4*reactor_id + 2,2]))
  Length <- as.numeric(Parameters[4*reactor_id + 3,2])
  return( as.numeric(c(GridSize,Width,steps,Length)))
}