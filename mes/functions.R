library(lubridate)
library(dplyr)
library(reshape2)
library(ggplot2)
library(SemiPar)
library(ggthemes)
library(tidyr)
library(zoo)
library(CausalImpact)
library(readr)
library(stringr)
library(reshape2)
library(ISOweek)
library(curl)
library(purrr)

download_data<-function(){
  curl_download("http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/historicos/2021/09/datos_abiertos_covid19_01.09.2021.zip", 
                destfile="dataset.zip", 
                mode="wb") 
  unzip ("dataset.zip", exdir = "data/")
}


### Preparación de los datos de Casos de Covid positivos
get_data_cases<-function(path="data/210901COVID19MEXICO.csv",plot=FALSE,window = 7){
  ## Descarga datos
  covidmx <- read_csv(path)
  
  ### Segmenta por grupo de edad
  covidmx_positivos <- covidmx %>% 
    filter(CLASIFICACION_FINAL==3,ENTIDAD_RES=="09") %>% 
    group_by(FECHA_SINTOMAS) %>%
    summarise(TOTAL = n(),
              MAYOR60 = sum(EDAD >= 60),
              E.50.59 = sum(EDAD >= 50 & EDAD <=59),
              E.40.49 = sum(EDAD >= 40 & EDAD <=49),
              E.30.39 = sum(EDAD >= 30 & EDAD <=39),
              E.20.29 = sum(EDAD >= 20 & EDAD <=29),
              MENOR20 = sum(EDAD <= 19),
              MENOR60 = sum(EDAD < 60))
  
  ### Variable como fecha
  covidmx_positivos$FECHA_SINTOMAS <- as.Date(covidmx_positivos$FECHA_SINTOMAS)
  ### Transformación para graficos y calculo de rolling mean  días
  datos_graficar <- covidmx_positivos%>%
    filter(FECHA_SINTOMAS>='2020-06-1',FECHA_SINTOMAS<'2021-06-1')%>%
    dplyr::select(FECHA_SINTOMAS,TOTAL,MAYOR60,E.50.59,E.40.49,E.30.39,E.20.29,MENOR20)%>%
    pivot_longer(!FECHA_SINTOMAS,names_to = "Group",values_to = "Casos")%>%
    mutate(Group = recode(Group,
                          "TOTAL" = "Total",
                          "MAYOR60" = "60+",
                          "E.50.59" = "50-59",
                          "E.40.49" = "40-49",
                          "E.30.39" = "30-39",
                          "E.20.29" = "20-29",
                          "MENOR20" = "19-"),
           Group = factor(Group,
                          levels = c("19-","20-29","30-39","40-49","50-59","60+","Total")))%>%
    group_by(Group)%>%
    mutate(moving.cases= c(rep(NA,window-1),rollmean(Casos,k=window)),
           OR20 = )
  
  ## Codigo para graficos
  if(plot==TRUE){
    
    datos_graficar%>%
      filter(Group!='TOTAL')%>%
      ggplot()+
      geom_line(aes(x=FECHA_SINTOMAS,y = moving.cases,col=Group))+
      xlab("")+
      theme_light()+
      theme(legend.position = "top")+
      ylab("Total Cases")+
      theme(text = element_text(size = 15))+
      scale_fill_colorblind()+
      scale_x_date(breaks = as.Date(c("2020-8-1","2020-9-1","2020-10-1",
                                      "2020-11-1","2020-12-1","2021-1-1",
                                      "2021-2-1","2021-3-1","2021-4-1",
                                      "2021-5-1","2021-6-1")),
                   labels = c("Ago","Sep","Oct","Nov","Dec","Jan","Feb",
                              "Mar","Abr","May","Jun"))
    
  }
  ### Conversion a wide
  covid_casos<-datos_graficar %>%
    pivot_wider(id_cols = FECHA_SINTOMAS,
                names_from = Group,
                values_from = moving.cases)%>%
    drop_na()
  ## Renombre de columnas
  colnames(covid_casos)<-c("FECHA_SINTOMAS","TOTAL","CASOS60","CASOS50", "CASOS40","CASOS30","CASOS20","CASOSMENOR20")
  
  return(covid_casos)
}


get_data_hosp<-function(path="data/210901COVID19MEXICO.csv",plot=FALSE,window = 7){
  ## Descarga datos
  covidmx <- read_csv(path)
  
  ### Segmenta por grupo de edad
  covidmx_positivos <- covidmx %>% 
    filter(CLASIFICACION_FINAL==3,ENTIDAD_RES=="09",TIPO_PACIENTE==2) %>% 
    group_by(FECHA_SINTOMAS) %>%
    summarise(TOTAL = n(),
              MAYOR60 = sum(EDAD >= 60),
              E.50.59 = sum(EDAD >= 50 & EDAD <=59),
              E.40.49 = sum(EDAD >= 40 & EDAD <=49),
              E.30.39 = sum(EDAD >= 30 & EDAD <=39),
              E.20.29 = sum(EDAD >= 20 & EDAD <=29),
              MENOR20 = sum(EDAD <= 19),
              MENOR60 = sum(EDAD < 60))
  
  ### Variable como fecha
  covidmx_positivos$FECHA_SINTOMAS <- as.Date(covidmx_positivos$FECHA_SINTOMAS)
  ### Transformación para graficos y calculo de rolling mean  días
  datos_graficar <- covidmx_positivos%>%
    filter(FECHA_SINTOMAS>='2020-06-1',FECHA_SINTOMAS<'2021-06-1')%>%
    dplyr::select(FECHA_SINTOMAS,TOTAL,MAYOR60,E.50.59,E.40.49,E.30.39,E.20.29,MENOR20)%>%
    pivot_longer(!FECHA_SINTOMAS,names_to = "Group",values_to = "Casos")%>%
    mutate(Group = recode(Group,
                          "TOTAL" = "Total",
                          "MAYOR60" = "60+",
                          "E.50.59" = "50-59",
                          "E.40.49" = "40-49",
                          "E.30.39" = "30-39",
                          "E.20.29" = "20-29",
                          "MENOR20" = "19-"),
           Group = factor(Group,
                          levels = c("19-","20-29","30-39","40-49","50-59","60+","Total")))%>%
    group_by(Group)%>%
    mutate(moving.cases= c(rep(NA,window-1),rollmean(Casos,k=window)),
           OR20 = )
  
  ## Codigo para graficos
  if(plot==TRUE){
    
    datos_graficar%>%
      filter(Group!='TOTAL')%>%
      ggplot()+
      geom_line(aes(x=FECHA_SINTOMAS,y = moving.cases,col=Group))+
      xlab("")+
      theme_light()+
      theme(legend.position = "top")+
      ylab("Total Cases")+
      theme(text = element_text(size = 15))+
      scale_fill_colorblind()+
      scale_x_date(breaks = as.Date(c("2020-8-1","2020-9-1","2020-10-1",
                                      "2020-11-1","2020-12-1","2021-1-1",
                                      "2021-2-1","2021-3-1","2021-4-1",
                                      "2021-5-1","2021-6-1")),
                   labels = c("Ago","Sep","Oct","Nov","Dec","Jan","Feb",
                              "Mar","Abr","May","Jun"))
    
  }
  ### Conversion a wide
  covid_casos<-datos_graficar %>%
    pivot_wider(id_cols = FECHA_SINTOMAS,
                names_from = Group,
                values_from = moving.cases)%>%
    drop_na()
  ## Renombre de columnas
  colnames(covid_casos)<-c("FECHA_SINTOMAS","HOSPTOTAL","HOSP60","HOSP50", "HOSP40","HOSP30","HOSP20","HOSPMENOR20")
  return(covid_casos)
}


get_data_muertes<-function(path="data/210901COVID19MEXICO.csv",plot=FALSE,window = 7){
  ## Descarga datos
  covidmx <- read_csv(path)
  
  ### Segmenta por grupo de edad
  covidmx_positivos <- covidmx %>% 
    filter(CLASIFICACION_FINAL==3,ENTIDAD_RES=="09",!is.na(FECHA_DEF)) %>% 
    group_by(FECHA_SINTOMAS) %>%
    summarise(TOTAL = n(),
              MAYOR60 = sum(EDAD >= 60),
              E.50.59 = sum(EDAD >= 50 & EDAD <=59),
              E.40.49 = sum(EDAD >= 40 & EDAD <=49),
              E.30.39 = sum(EDAD >= 30 & EDAD <=39),
              E.20.29 = sum(EDAD >= 20 & EDAD <=29),
              MENOR20 = sum(EDAD <= 19),
              MENOR60 = sum(EDAD < 60))
  
  ### Variable como fecha
  covidmx_positivos$FECHA_SINTOMAS <- as.Date(covidmx_positivos$FECHA_SINTOMAS)
  ### Transformación para graficos y calculo de rolling mean  días
  datos_graficar <- covidmx_positivos%>%
    filter(FECHA_SINTOMAS>='2020-06-1',FECHA_SINTOMAS<'2021-06-1')%>%
    dplyr::select(FECHA_SINTOMAS,TOTAL,MAYOR60,E.50.59,E.40.49,E.30.39,E.20.29,MENOR20)%>%
    pivot_longer(!FECHA_SINTOMAS,names_to = "Group",values_to = "Casos")%>%
    mutate(Group = recode(Group,
                          "TOTAL" = "Total",
                          "MAYOR60" = "60+",
                          "E.50.59" = "50-59",
                          "E.40.49" = "40-49",
                          "E.30.39" = "30-39",
                          "E.20.29" = "20-29",
                          "MENOR20" = "19-"),
           Group = factor(Group,
                          levels = c("19-","20-29","30-39","40-49","50-59","60+","Total")))%>%
    group_by(Group)%>%
    mutate(moving.cases= c(rep(NA,window-1),rollmean(Casos,k=window)),
           OR20 = )
  
  ## Codigo para graficos
  if(plot==TRUE){
    
    datos_graficar%>%
      filter(Group!='TOTAL')%>%
      ggplot()+
      geom_line(aes(x=FECHA_SINTOMAS,y = moving.cases,col=Group))+
      xlab("")+
      theme_light()+
      theme(legend.position = "top")+
      ylab("Total Cases")+
      theme(text = element_text(size = 15))+
      scale_fill_colorblind()+
      scale_x_date(breaks = as.Date(c("2020-8-1","2020-9-1","2020-10-1",
                                      "2020-11-1","2020-12-1","2021-1-1",
                                      "2021-2-1","2021-3-1","2021-4-1",
                                      "2021-5-1","2021-6-1")),
                   labels = c("Ago","Sep","Oct","Nov","Dec","Jan","Feb",
                              "Mar","Abr","May","Jun"))
    
  }
  ### Conversion a wide
  covid_casos<-datos_graficar %>%
    pivot_wider(id_cols = FECHA_SINTOMAS,
                names_from = Group,
                values_from = moving.cases)%>%
    drop_na()
  ## Renombre de columnas
  colnames(covid_casos)<-c("FECHA_SINTOMAS","DEFTOTAL","DEF60","DEF50", "DEF40","DEF30","DEF20","DEFMENOR20")
  return(covid_casos)
}


cum_data_as_of_date<-function(date='2020-06-1'){
  path="data/210901COVID19MEXICO.csv"
  date <-as.Date(date)
  covidmx <- read_csv(path)
  
  ### Segmenta por grupo de edad
  covidmx_hosp <- covidmx %>% 
    filter(CLASIFICACION_FINAL==3,ENTIDAD_RES=="09",TIPO_PACIENTE==2) %>% 
    group_by(FECHA_SINTOMAS) %>%
    summarise(TOTAL = n(),
              MAYOR60 = sum(EDAD >= 60),
              E.50.59 = sum(EDAD >= 50 & EDAD <=59),
              E.40.49 = sum(EDAD >= 40 & EDAD <=49),
              E.30.39 = sum(EDAD >= 30 & EDAD <=39),
              E.20.29 = sum(EDAD >= 20 & EDAD <=29),
              MENOR20 = sum(EDAD <= 19),
              MENOR60 = sum(EDAD < 60))%>%
    filter(FECHA_SINTOMAS<=date)
  
  covidmx_hosp_cum <- covidmx_hosp%>% mutate(cum_mayores=cumsum(MAYOR60))
  cum_hosp<-covidmx_hosp_cum$cum_mayores[length(covidmx_hosp_cum$cum_mayores)-1]
  
  covidmx_casos <- covidmx %>% 
    filter(CLASIFICACION_FINAL==3,ENTIDAD_RES=="09") %>% 
    group_by(FECHA_SINTOMAS) %>%
    summarise(TOTAL = n(),
              MAYOR60 = sum(EDAD >= 60),
              E.50.59 = sum(EDAD >= 50 & EDAD <=59),
              E.40.49 = sum(EDAD >= 40 & EDAD <=49),
              E.30.39 = sum(EDAD >= 30 & EDAD <=39),
              E.20.29 = sum(EDAD >= 20 & EDAD <=29),
              MENOR20 = sum(EDAD <= 19),
              MENOR60 = sum(EDAD < 60))%>%
    filter(FECHA_SINTOMAS<=date)
  
  covidmx_casos_cum <- covidmx_casos%>% mutate(cum_mayores=cumsum(MAYOR60))
  cum_casos<-covidmx_casos_cum$cum_mayores[length(covidmx_casos_cum$cum_mayores)-1]
  
  covidmx_muertes <- covidmx %>% 
    filter(CLASIFICACION_FINAL==3,ENTIDAD_RES=="09",!is.na(FECHA_DEF)) %>% 
    group_by(FECHA_SINTOMAS) %>%
    summarise(TOTAL = n(),
              MAYOR60 = sum(EDAD >= 60),
              E.50.59 = sum(EDAD >= 50 & EDAD <=59),
              E.40.49 = sum(EDAD >= 40 & EDAD <=49),
              E.30.39 = sum(EDAD >= 30 & EDAD <=39),
              E.20.29 = sum(EDAD >= 20 & EDAD <=29),
              MENOR20 = sum(EDAD <= 19),
              MENOR60 = sum(EDAD < 60))%>%
    filter(FECHA_SINTOMAS<=date)
  
  covidmx_muertes_cum <- covidmx_muertes%>% mutate(cum_mayores=cumsum(MAYOR60))
  cum_muertes<-covidmx_muertes_cum$cum_mayores[length(covidmx_muertes_cum$cum_mayores)-1]
  
  return(c(cum_casos,cum_hosp,cum_muertes))                                           
  
}

