
load <- function(){
  if(!file.exists('../Datos/walmart.rds')){

    walmart <- read_csv("../Datos/train.csv") 
    saveRDS(walmart, "../Datos/walmart.rds")
    print('walmart.rds se bajó y guardó\n')
  }
  else{
    warning('walmart.rds ya existe\n')
    walmart <- readRDS("../Datos/walmart.rds")
  }
  
  return(walmart)
}

prepare <- function(){
  data_sumarised <- walmart %>% group_by(TripType,VisitNumber,Weekday,DepartmentDescription) %>% 
    summarise(Count = sum(ScanCount)) %>% 
    spread(key=DepartmentDescription,value=Count)
  return(data_sumarised)
}


clean <- function() { 
  walmartSpread[is.na(walmartSpread)] <- 0
  walmartSpread$MENSWEAR <- walmartSpread$`MENS WEAR`+walmartSpread$MENSWEAR
  walmartSpread <- walmartSpread %>% subset(select = -c(`MENS WEAR`) )
}
