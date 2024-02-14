Geometry <- function(reactor_id,Parameters){
  GridSize <- Parameters[4*reactor_id,2]
  Width <- Parameters[4*reactor_id + 1,2]
  steps <- Parameters[4*reactor_id + 2,2]
  Length <- Parameters[4*reactor_id + 3,2]
  return( as.numeric(c(GridSize,Width,steps,Length)))
}
