#Preparación de los datos, se suman los productos por visita y se agrupan por departamento,
#posteriormente se coloca cada departemento como una columna

walmart_data <- prepare(walmart_data_Row)

prepare <- function(data){
  data_sumarised <- data %>% group_by(TripType,VisitNumber,Weekday,DepartmentDescription) %>% 
    summarise(Count = sum(ScanCount)) %>% 
    spread(key=DepartmentDescription,value=Count)
  return(data_sumarised)
}