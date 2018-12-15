
load <- function(){
  if(!file.exists('walmart.rds')){

    walmart <- read_csv("Datos/train.csv") 
    saveRDS(walmart, "walmart.rds")
    print('walmart.rds se bajó y guardó\n')
  }
  else{
    warning('walmart.rds ya existe\n')
    walmart <- readRDS("walmart.rds")
  }
  
  return(walmart)
}
