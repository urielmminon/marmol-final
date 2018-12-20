
list.of.packages <- c("readr", "dplyr","tidyr","feather","stringr","tibble") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] 
if(length(new.packages)) install.packages(new.packages)

library(readr)
library(dplyr)
library(tidyr)
library(feather)
library(stringr)
library(tibble)


##### train

walmart <- read_csv("Datos/train.csv")

print('train se bajó y guardó\n')

source("utils.R")
source("01-prepare.R")
source("02-clean.R") #cambia nombres de variables

#regresar a nombres originales, solo se corre si en el 02-clean.R se cambiaron los nombres
#de las variables
colnames(walmartSpread) <- names_orig

#total productos comprados
walmartSpread$total_prod <- rowSums(walmartSpread[,-c(1,2,3)])

#indicadora devoluciones

walmart.Dev <- walmart %>% group_by(VisitNumber) %>% 
  summarise(min_p = min(ScanCount)) 

walmart.Dev <- mutate(walmart.Dev, ind_dev = ifelse(min_p < 0, 1, 0))
walmart.Dev$min_p <- NULL

walmartSpread <- merge(walmartSpread,walmart.Dev, by = "VisitNumber")

#el merge convierte la base a solo data frame

# total de departamentos visitados
walmartSpread$departamentos_visitados <- apply(walmartSpread[,4:71], 1, function(x) sum(x!=0))


# cuenta de articulos diferentes basados en upc
prod_distintos <- walmart %>% filter(ScanCount>0) %>% group_by(VisitNumber) %>% summarize(prod_distintos_upc=n())
walmartSpread <- left_join(walmartSpread,prod_distintos,by=c("VisitNumber"="VisitNumber"))



# cuenta de articulos devueltos
prod_devueltos<- walmart %>% filter(ScanCount<0) %>%  arrange(VisitNumber) %>%  group_by(VisitNumber) %>% summarize(prod_devueltos=n())
walmartSpread <- left_join(walmartSpread,prod_devueltos,by=c("VisitNumber"="VisitNumber"))

walmartSpread[is.na(walmartSpread)] <- 0

#test

walmart.test <- read_csv("Datos/test.csv") 
#653,646
print('test se bajó y guardó\n')

walmart.testSpread <- walmart.test %>% group_by(VisitNumber,Weekday,DepartmentDescription) %>% 
  summarise(Count = sum(ScanCount)) %>% 
  spread(key=DepartmentDescription,value=Count)

walmart.testSpread[is.na(walmart.testSpread)] <- 0
walmart.testSpread$MENSWEAR <- walmart.testSpread$`MENS WEAR`+walmart.testSpread$MENSWEAR
walmart.testSpread <- walmart.testSpread %>% subset(select = -c(`MENS WEAR`) )
#walmart.testSpread$HEALTH_AND_BEAUTY_AIDS <- 0


walmart.testSpread <- add_column(walmart.testSpread, rep(0,nrow(walmart.testSpread)), .after = 28)
colnames(walmart.testSpread)[29] <- "HEALTH_AND_BEAUTY_AIDS"


#total productos comprados
walmart.testSpread$total_prod <- rowSums(walmart.testSpread[,-c(1,2)])

#indicadora devoluciones

walmart.testDev <- walmart.test %>% group_by(VisitNumber) %>% 
  summarise(min_p = min(ScanCount)) 

walmart.testDev <- mutate(walmart.testDev, ind_dev = ifelse(min_p < 0, 1, 0))
walmart.testDev$min_p <- NULL

#pegar variable a la base completa
walmart.testSpread <- merge(walmart.testSpread,walmart.testDev, by = "VisitNumber")

# total de departamentos visitados
walmart.testSpread$departamentos_visitados <- apply(walmart.testSpread[,4:71], 1, function(x) sum(x!=0))


# cuenta de articulos diferentes basados en upc
prod_distintos <- walmart.test %>% filter(ScanCount>0) %>% group_by(VisitNumber) %>% summarize(prod_distintos_upc=n())
walmart.testSpread <- left_join(walmart.testSpread,prod_distintos,by=c("VisitNumber"="VisitNumber"))


# cuenta de articulos devueltos
prod_devueltos<- walmart.test %>% filter(ScanCount<0) %>%  arrange(VisitNumber) %>%  group_by(VisitNumber) %>% summarize(prod_devueltos=n())
walmart.testSpread <- left_join(walmart.testSpread,prod_devueltos,by=c("VisitNumber"="VisitNumber"))

walmart.testSpread[is.na(walmart.testSpread)] <- 0

#dia del mes


walmart.todo <- rbind(walmartSpread[,c(1,3)],walmart.testSpread[,c(1,2)])

walmart.ord <- walmart.todo[order(walmart.todo$VisitNumber),c(1,2)]

#detectar cambios de semana
viernes <- data.frame(walmart.ord[walmart.ord$Weekday=="Friday",])
viernes <- mutate(viernes, lag_v = dplyr::lag(VisitNumber, default=0))

viernes$dif <- viernes$VisitNumber - viernes$lag_v

cambio.viernes <- viernes[viernes$dif > 1 & viernes$lag_v != 0,]

##sábado

sabado <- data.frame(walmart.ord[walmart.ord$Weekday=="Saturday",])
sabado <- mutate(sabado, lag_v = dplyr::lag(VisitNumber, default=0))

sabado$dif <- sabado$VisitNumber - sabado$lag_v

cambio.sabado <- sabado[sabado$dif > 1 & sabado$lag_v != 0,]

##domingo

domingo <- data.frame(walmart.ord[walmart.ord$Weekday=="Sunday",])
domingo <- mutate(domingo, lag_v = dplyr::lag(VisitNumber, default=0))

domingo$dif <- domingo$VisitNumber - domingo$lag_v

cambio.domingo <- domingo[domingo$dif > 1 & domingo$lag_v != 0,]

##lunes

lunes <- data.frame(walmart.ord[walmart.ord$Weekday=="Monday",])
lunes <- mutate(lunes, lag_v = dplyr::lag(VisitNumber, default=0))

lunes$dif <- lunes$VisitNumber - lunes$lag_v

cambio.lunes <- lunes[lunes$dif > 1 & lunes$lag_v != 0,]

##martes

martes <- data.frame(walmart.ord[walmart.ord$Weekday=="Tuesday",])
martes <- mutate(martes, lag_v = dplyr::lag(VisitNumber, default=0))

martes$dif <- martes$VisitNumber - martes$lag_v

cambio.martes <- martes[martes$dif > 1 & martes$lag_v != 0,]

##miércoles

miercoles <- data.frame(walmart.ord[walmart.ord$Weekday=="Wednesday",])
miercoles <- mutate(miercoles, lag_v = dplyr::lag(VisitNumber, default=0))

miercoles$dif <- miercoles$VisitNumber - miercoles$lag_v

cambio.miercoles <- miercoles[miercoles$dif > 1 & miercoles$lag_v != 0,]

##jueves

jueves <- data.frame(walmart.ord[walmart.ord$Weekday=="Thursday",])
jueves <- mutate(jueves, lag_v = dplyr::lag(VisitNumber, default=0))

jueves$dif <- jueves$VisitNumber - jueves$lag_v

cambio.jueves <- jueves[jueves$dif > 1 & jueves$lag_v != 0,]

#######

jueves$cambio1 <- cambio.jueves[1,"VisitNumber"]
jueves$cambio2 <- cambio.jueves[2,"VisitNumber"]
jueves$cambio3 <- cambio.jueves[3,"VisitNumber"]

jueves <- mutate(jueves, dia_mes = ifelse(VisitNumber < cambio1, 7, 
                                          ifelse(VisitNumber < cambio2, 14, 
                                                 ifelse(VisitNumber < cambio3, 21, 28)
                                          ) 
))
##


miercoles$cambio1 <- cambio.miercoles[1,"VisitNumber"]
miercoles$cambio2 <- cambio.miercoles[2,"VisitNumber"]
miercoles$cambio3 <- cambio.miercoles[3,"VisitNumber"]

miercoles <- mutate(miercoles, dia_mes = ifelse(VisitNumber < cambio1, 6, 
                                                ifelse(VisitNumber < cambio2, 13, 
                                                       ifelse(VisitNumber < cambio3, 20, 27)
                                                ) 
))

##


martes$cambio1 <- cambio.martes[1,"VisitNumber"]
martes$cambio2 <- cambio.martes[2,"VisitNumber"]
martes$cambio3 <- cambio.martes[3,"VisitNumber"]

martes <- mutate(martes, dia_mes = ifelse(VisitNumber < cambio1, 5, 
                                          ifelse(VisitNumber < cambio2, 12, 
                                                 ifelse(VisitNumber < cambio3, 19, 26)
                                          ) 
))


##


lunes$cambio1 <- cambio.lunes[1,"VisitNumber"]
lunes$cambio2 <- cambio.lunes[2,"VisitNumber"]
lunes$cambio3 <- cambio.lunes[3,"VisitNumber"]

lunes <- mutate(lunes, dia_mes = ifelse(VisitNumber < cambio1, 4, 
                                        ifelse(VisitNumber < cambio2, 11, 
                                               ifelse(VisitNumber < cambio3, 18, 25)
                                        ) 
))


##

domingo$cambio1 <- cambio.domingo[1,"VisitNumber"]
domingo$cambio2 <- cambio.domingo[2,"VisitNumber"]
domingo$cambio3 <- cambio.domingo[3,"VisitNumber"]
domingo$cambio4 <- cambio.domingo[4,"VisitNumber"]

domingo <- mutate(domingo, dia_mes = ifelse(VisitNumber < cambio1, 3, 
                                            ifelse(VisitNumber < cambio2, 10, 
                                                   ifelse(VisitNumber < cambio3, 17, 
                                                          ifelse(VisitNumber < cambio4, 24,31)
                                                   )
                                            ) 
))

##

sabado$cambio1 <- cambio.sabado[1,"VisitNumber"]
sabado$cambio2 <- cambio.sabado[2,"VisitNumber"]
sabado$cambio3 <- cambio.sabado[3,"VisitNumber"]
sabado$cambio4 <- cambio.sabado[4,"VisitNumber"]

sabado <- mutate(sabado, dia_mes = ifelse(VisitNumber < cambio1, 2, 
                                          ifelse(VisitNumber < cambio2, 9, 
                                                 ifelse(VisitNumber < cambio3, 16, 
                                                        ifelse(VisitNumber < cambio4, 23,30)
                                                 )
                                          ) 
))

##

viernes$cambio1 <- cambio.viernes[1,"VisitNumber"]
viernes$cambio2 <- cambio.viernes[2,"VisitNumber"]
viernes$cambio3 <- cambio.viernes[3,"VisitNumber"]
viernes$cambio4 <- cambio.viernes[4,"VisitNumber"]

viernes <- mutate(viernes, dia_mes = ifelse(VisitNumber < cambio1, 1, 
                                            ifelse(VisitNumber < cambio2, 8, 
                                                   ifelse(VisitNumber < cambio3, 15, 
                                                          ifelse(VisitNumber < cambio4, 22,29)
                                                   )
                                            ) 
))

juntar <- rbind(viernes[,c("VisitNumber","dia_mes")],sabado[,c("VisitNumber","dia_mes")],
                domingo[,c("VisitNumber","dia_mes")],lunes[,c("VisitNumber","dia_mes")],
                martes[,c("VisitNumber","dia_mes")],miercoles[,c("VisitNumber","dia_mes")],
                jueves[,c("VisitNumber","dia_mes")])


#agregar a train y test

walmartSpread <- merge(walmartSpread, juntar, by = "VisitNumber")

walmart.testSpread <- merge(walmart.testSpread, juntar, by = "VisitNumber")


path <- "Datos/walmart_test.feather"
write_feather(walmart.testSpread, path)


path <- "Datos/walmart.feather"
write_feather(walmartSpread, path)


