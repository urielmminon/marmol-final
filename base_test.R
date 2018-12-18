

walmart.test <- read_csv("Datos/test.csv") 
#653,646

# visitas <- walmart.test %>% group_by(VisitNumber) %>% 
#   summarise(.,total=n())
# nrow(visitas) 
# 
# deptos <- walmart.test %>% group_by(DepartmentDescription) %>% 
#   summarise(.,total=sum(ScanCount))
# nrow(deptos)
# deptos.ord <- deptos[order(deptos$total),]

walmart.testSpread <- walmart.test %>% group_by(VisitNumber,Weekday,DepartmentDescription) %>% 
  summarise(Count = sum(ScanCount)) %>% 
  spread(key=DepartmentDescription,value=Count)
nrow(data_sumarised)

walmart.testSpread[is.na(walmart.testSpread)] <- 0
walmart.testSpread$MENSWEAR <- walmart.testSpread$`MENS WEAR`+walmart.testSpread$MENSWEAR
walmart.testSpread <- walmart.testSpread %>% subset(select = -c(`MENS WEAR`) )
walmart.testSpread$HEALTH_AND_BEAUTY_AIDS <- 0


path <- "Datos/walmart_test.feather"
write_feather(walmart.testSpread, path)

