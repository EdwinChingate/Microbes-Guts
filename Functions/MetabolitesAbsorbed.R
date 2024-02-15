MetabolitesAbsorbed <- function(MenuLocation){
  menuTable <- read_excel(MenuLocation)
  ID_Absorb_Duodenum <- menuTable['id'][(menuTable['Duodenum']==1)&(menuTable['Concentration (mM)']>0)&(menuTable['Exchange']==1)]
  ID_Absorb_Jejunum <- menuTable['id'][(menuTable['Jejunum']==1)&(menuTable['Concentration (mM)']>0)&(menuTable['Exchange']==1)]
  ID_Absorb_Ilenum <- menuTable['id'][(menuTable['Ileum']==1)&(menuTable['Concentration (mM)']>0)&(menuTable['Exchange']==1)]
  ID_Absorb_LargeIntestine <- menuTable['id'][(menuTable['Large intestine']==1)&(menuTable['Concentration (mM)']>0)&(menuTable['Exchange']==1)]  
  return(list(ID_Absorb_Duodenum,ID_Absorb_Jejunum,ID_Absorb_Ilenum,ID_Absorb_LargeIntestine))
}
