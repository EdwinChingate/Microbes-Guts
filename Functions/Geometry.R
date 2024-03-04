Geometry <- function(Parameters){
  GeometricMatrix <- matrix(0,nrow=6,ncol=4)
  Flow_rate <- as.numeric(Parameters[3,2])*1000/24 #cm3/h  
  AllSteps <- as.numeric(Parameters[4,2])
  GridSize <- as.numeric(Parameters[5,2])
  TotalLength <- sum(as.numeric(c(Parameters[7,2],Parameters[9,2],Parameters[11,2],Parameters[13,2])))
  MicrobeVolume <- as.numeric(Parameters[14,2]) #um3
  MicrobeMass <- as.numeric(Parameters[15,2])
  for (reactor_id in 1:4){
    Width <- as.numeric(Parameters[2*reactor_id + 4,2])
    Length <- as.numeric(Parameters[2*reactor_id + 5,2])
    LenghtFraction <- Length/TotalLength
    steps <- as.integer(round(LenghtFraction*AllSteps))    
    Flow_area <- Width^2 #cm2
    Speed <- Flow_rate/Flow_area #cm/h
    Hydraulic_retention_time <- Length/Speed #h
    dt <- Hydraulic_retention_time/steps
    CellLengt <- dt*Speed
    width <- Width/GridSize
    CellArea <- width**2
    CellVolume <- CellArea*CellLengt*1e12 #um3 #to define the ammount of metabolite in each cell
    MaxMicrobesNumber <- CellVolume/MicrobeVolume
    MaxBiomass <- MaxMicrobesNumber*MicrobeMass
    GeometricMatrix[1,reactor_id] <- CellVolume
    GeometricMatrix[2,reactor_id] <- Hydraulic_retention_time
    GeometricMatrix[3,reactor_id] <- MaxBiomass
    GeometricMatrix[4,reactor_id] <- steps
    GeometricMatrix[5,reactor_id] <- dt
    GeometricMatrix[6,reactor_id] <- Width
  }
  return(GeometricMatrix)
}