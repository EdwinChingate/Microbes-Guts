closeAllConnections()
rm(list=ls()) #to clean memory, better to always run it

home <- "~/LRZ Sync+Share/GitHubProjects/Codding/Microbes-Guts" #getwd()
setwd(home)
source('Functions/Guts.R')
eval <- Guts(home,Parameters_folder='ParametersRun',Models_folder='MicroModelsRun') #Default values Parameters_folder='Parameters',Models_folder='MicroModel'

#check for the absorption in the duodenum column
#there's still a problem with the IDs...
#check for the zero definition in the internal environment

MenuLocation <- paste(home,'/ParametersRun/InfantFormulaDiet.xlsx',sep='')
menuTable <- read_excel(MenuLocation)

menuTable['id'][(menuTable['Duodenum']==1)&(menuTable['Concentration (mM)']>0)&(menuTable['Exchange']==1)]
asAbsorption <- MetabolitesAbsorbed(MenuLocation)

reactor_id <- 2
IDsabsorp <- Absorption[[reactor_id]]
#veryfy that the substances we're changing are in the environment, and that they can be consumed  
last_step <- length(eval@medlist)
for (id in IDsabsorp){
  Ex_id <- paste('EX_',id,sep='')
  print(Ex_id)
  #print(eval@medlist[[last_step]][[Ex_id]])
  eval@medlist[[last_step]][[Ex_id]][10] <- 0 
  evalF <- BacArena::simEnv(eval, sec_obj='mtf',time=1)
}
steps <-1
GridSize=10
eval <- Bioreactor(steps,eval,IDsabsorp,GridSize)
