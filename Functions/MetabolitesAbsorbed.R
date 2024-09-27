MetabolitesAbsorbed <- function(MenuLocation){
  menuTable <- read_excel(MenuLocation)
  menuTableConcentrationFilter <-menuTable['Concentration (mM)']>0
  menuTableExchangeFilter <- menuTable['Exchange']==1
  menuTable_DuodenumFilter <- menuTable['Duodenum']==1
  DuodenumFilter <- menuTableConcentrationFilter & menuTableExchangeFilter & menuTable_DuodenumFilter
  ID_Absorb_Duodenum <- list()
  ID_Absorb_Duodenum <- append(ID_Absorb_Duodenum,menuTable['id'][DuodenumFilter])
  menuTable_JejunumFilter <- menuTable['Jejunum']==1
  JejunumFilter <- menuTableConcentrationFilter & menuTableExchangeFilter & menuTable_JejunumFilter
  ID_Absorb_Jejunum <- list()
  ID_Absorb_Jejunum <- append(ID_Absorb_Jejunum,menuTable['id'][JejunumFilter])
  menuTable_IleumFilter <- menuTable['Ileum']==1
  IleumFilter <- menuTableConcentrationFilter & menuTableExchangeFilter & menuTable_IleumFilter
  ID_Absorb_Ilenum <- list()
  ID_Absorb_Ilenum <- append(ID_Absorb_Ilenum,menuTable['id'][IleumFilter])
  menuTable_LargeIntestineFilter <- menuTable['Large intestine']==1
  LargeIntestineFilter <- menuTableConcentrationFilter & menuTableExchangeFilter & menuTable_LargeIntestineFilter
  ID_Absorb_LargeIntestine <- list()
  ID_Absorb_LargeIntestine <- append(ID_Absorb_LargeIntestine,menuTable['id'][LargeIntestineFilter])  
  Absorption <- list(ID_Absorb_Duodenum,ID_Absorb_Jejunum,ID_Absorb_Ilenum,ID_Absorb_LargeIntestine)
  return(Absorption)
}
