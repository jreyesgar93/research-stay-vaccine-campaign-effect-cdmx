

### Model fits
fit_model <- function(covid_casos,sd.level = 0.01){ 
  fecha_inicio_datos <- covid_casos %>% pull(FECHA_SINTOMAS) %>% min()
  fecha_inicio_60    <- as.Date("2021-02-15")
  fecha_inicio_otros <- as.Date("2021-05-03")
  fecha_fin_datos    <- covid_casos %>% pull(FECHA_SINTOMAS) %>% max()
  
  CausalImpact(data = zoo(covid_casos[,c(3,4,5)],
                          covid_casos$FECHA_SINTOMAS),
               pre.period  = c(fecha_inicio_datos, fecha_inicio_60),
               post.period = c(fecha_inicio_60 + 1, fecha_inicio_otros),
               model.args  = list(niter = 1000,
                                  prior.level.sd = sd.level,
                                  dynamic.regression=FALSE
               ))
}



get_pvalue <- function(model){
  model$summary$p[[1]]
}

get_abs_effect <- function(model){
  model$summary$AbsEffect[[1]]
}

get_abs_effect_cum <- function(model){
  model$summary$AbsEffect[[2]]
}

get_rel_effect <- function(model){
  model$summary$RelEffect[[1]]
}


get_rel_effect_cum <- function(model){
  model$summary$RelEffect[[2]]
}


get_pred <- function(model){
  model$summary$Pred[[1]]
}


get_prediction<-function(model){
  x<-model$series
  x<-x[as.Date(c(as.Date("2021-02-15"):as.Date("2021-05-31")))]
  return(x)
}

## Fitting model to bootstapped data

data_models_nr <- data%>%
  mutate(cases_model=map(resampled_cases, fit_model))%>%
  mutate(cases_pvalue=map(cases_model,get_pvalue))%>%
  mutate(cases_abs_effecte=map(cases_model,get_abs_effect))%>%
  mutate(cases_abs_effecte_cum=map(cases_model,get_abs_effect_cum))%>%
  mutate(cases_pred=map(cases_model,get_pred))%>%
  mutate(cases_prediction=map(cases_model,get_prediction))%>%
  mutate(hosp_model=map(resampled_hosp, fit_model))%>%
  mutate(hosp_pvalue=map(hosp_model, get_pvalue))%>%
  mutate(hosp_abs_effecte=map(hosp_model, get_abs_effect))%>%
  mutate(hosp_abs_effecte_cum=map(hosp_model, get_abs_effect))%>%
  mutate(hosp_pred=map(hosp_model, get_pred))%>%
  mutate(hosp_prediction=map(hosp_model, get_prediction))%>%
  mutate(deaths_model=map(resampled_deaths, fit_model))%>%
  mutate(deaths_pvalue=map(deaths_model, get_pvalue))%>%
  mutate(deaths_abs_effecte=map(deaths_model, get_abs_effect))%>%
  mutate(deaths_abs_effecte_cum=map(deaths_model, get_abs_effect_cum))%>%
  mutate(deaths_pred=map(deaths_model, get_pred))%>%
  mutate(deaths_prediction=map(deaths_model, get_prediction))



data_models_nr2 <- data_models_nr%>%
   mutate(cases_rel_effecte=map(cases_model, get_rel_effect))%>%
   mutate(cases_rel_effecte_cum=map(cases_model, get_rel_effect_cum))%>%
  mutate(hosp_rel_effecte=map(hosp_model, get_rel_effect))%>%
  mutate(hosp_rel_effecte_cum=map(hosp_model, get_rel_effect_cum))%>%
   mutate(deaths_rel_effecte=map(deaths_model, get_rel_effect))%>%
   mutate(deaths_rel_effecte_cum=map(deaths_model, get_rel_effect_cum))


save(data_models_nr, file = "bootstrap_model_results.RData")