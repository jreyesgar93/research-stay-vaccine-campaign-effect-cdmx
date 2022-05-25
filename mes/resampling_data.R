library(purrr)
library(furrr)
library(lubridate)


path<-"data/210901COVID19MEXICO.csv"
covidmx <- read_csv(path)


#### Generating resample 
set.seed(42)


resample_cases<-function(sim){

  covidmx_positivos <- covidmx %>% 
    filter(CLASIFICACION_FINAL==3,ENTIDAD_RES=="09")%>% 
    dplyr::select(FECHA_SINTOMAS,FECHA_INGRESO,FECHA_DEF,TIPO_PACIENTE,EDAD)
  
  covidmx_positivos$id<-c(1:nrow(covidmx_positivos))
  covidmx_positivos<-covidmx_positivos[sample(covidmx_positivos$id,
                                              replace = TRUE),]
  window<-7
  ### Variable como fecha
  covidmx_positivos$FECHA_SINTOMAS <- as.Date(covidmx_positivos$FECHA_SINTOMAS)
  
  covidmx_positivos<-covidmx_positivos%>%
    group_by(FECHA_SINTOMAS) %>%
    summarise(TOTAL = n(),
              CASOS60 = sum(EDAD >= 60),
              HOSP50 = sum(EDAD >= 50 & EDAD <=59),
              HOSP40 = sum(EDAD >= 40 & EDAD <=49))%>%
    filter(FECHA_SINTOMAS>='2020-06-1',FECHA_SINTOMAS<'2021-06-1')%>%
    mutate(CASOS60= c(rep(NA,window-1),rollmean(CASOS60,k=window)))%>%
    mutate(CASOS50= c(rep(NA,window-1),rollmean(CASOS50,k=window)))%>%
    mutate(CASOS40= c(rep(NA,window-1),rollmean(CASOS40,k=window)))%>%
    drop_na()
  return(covid_casos)
}


resample_hosp<-function(sim){
  covidmx_positivos <- covidmx %>% 
    filter(CLASIFICACION_FINAL==3,ENTIDAD_RES=="09",TIPO_PACIENTE==2)%>%  ### Incluye hospitalizados
    dplyr::select(FECHA_SINTOMAS,FECHA_INGRESO,FECHA_DEF,TIPO_PACIENTE,EDAD)
  
  covidmx_positivos$id<-c(1:nrow(covidmx_positivos))
  covidmx_positivos<-covidmx_positivos[sample(covidmx_positivos$id,
                                              replace = TRUE),]
  
  covidmx_positivos$FECHA_SINTOMAS <- as.Date(covidmx_positivos$FECHA_SINTOMAS)
  window<-7
  
  covidmx_positivos<-covidmx_positivos%>%
    group_by(FECHA_SINTOMAS) %>%
    summarise(HOSPTOTAL = n(),
              HOSP60 = sum(EDAD >= 60),
              HOSP50 = sum(EDAD >= 50 & EDAD <=59),
              HOSP40 = sum(EDAD >= 40 & EDAD <=49))%>%
    filter(FECHA_SINTOMAS>='2020-06-1',FECHA_SINTOMAS<'2021-06-1')%>%
    mutate(HOSP60= c(rep(NA,window-1),rollmean(HOSP60,k=window)))%>%
    mutate(HOSP50= c(rep(NA,window-1),rollmean(HOSP50,k=window)))%>%
    mutate(HOSP40= c(rep(NA,window-1),rollmean(HOSP40,k=window)))%>%
    drop_na()
  ## Renombre de columnas
  return(covidmx_positivos)
}


resample_deaths<-function(sim){
  covidmx_positivos <- covidmx %>% 
    filter(CLASIFICACION_FINAL==3,ENTIDAD_RES=="09",!is.na(FECHA_DEF))%>%  ### Selecciona solo aquellos que tienen regisrada una fecha de defunción 
    dplyr::select(FECHA_SINTOMAS,FECHA_DEF,TIPO_PACIENTE,EDAD)
  
  covidmx_positivos$id<-c(1:nrow(covidmx_positivos))
  covidmx_positivos<-covidmx_positivos[sample(covidmx_positivos$id,
                                              replace = TRUE),]
  
  covidmx_positivos$FECHA_SINTOMAS <- as.Date(covidmx_positivos$FECHA_SINTOMAS)
  window<-7
  covidmx_positivos<-covidmx_positivos%>%
    group_by(FECHA_SINTOMAS) %>%
    summarise(DEFTOTAL = n(),
              DEF60 = sum(EDAD >= 60),
              DEF50 = sum(EDAD >= 50 & EDAD <=59),
              DEF40 = sum(EDAD >= 40 & EDAD <=49))%>%
    filter(FECHA_SINTOMAS>='2020-06-1',FECHA_SINTOMAS<'2021-06-1')%>%
    mutate(DEF60= c(rep(NA,window-1),rollmean(DEF60,k=window)))%>%
    mutate(DEF50= c(rep(NA,window-1),rollmean(DEF50,k=window)))%>%
    mutate(DEF40= c(rep(NA,window-1),rollmean(DEF40,k=window)))%>%
    drop_na()
  ## Renombre de columnas
  return(covidmx_positivos)
}





### Generating resampled data
## Cases

data<-tibble(sim = c(1:300)) %>% 
  mutate(resampled_cases=map(sim,resample_cases))

save(data, file = "resampled_datasets_pt1_2.RData")

###Hosp

data<-data%>%
  mutate(resampled_hosp=map(sim,resample_hosp))

### Deaths 
data<-data%>%
  mutate(resampled_deaths=map(sim,resample_deaths))


save(data, file = "resampled_datasets.RData")











