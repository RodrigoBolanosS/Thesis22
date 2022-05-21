
####About####

#Hertie School
#Rodrigo Bolaños Suárez
#Master's Thesis
#May, 2022
#Subnational state capacities in Mexico: an approach to its measurement and its determinants


####Libraries####

library(tidyverse)
library(plm)
library(ggplot2)
library(lubridate)
library(corrplot)
library(zoo)
library(lmtest)
library(gplots)
library(standardize)
library(kableExtra)
library(table1)
library(modelsummary)
library(tseries)
library(wesanderson)

rm(list = ls()) #clear elements
cat("\014") #for clearing console

####0. Preparative functions#####

transformar_asf <- function(entidad_df,nombre_entidad) {
  cols.num <- c("yr2000","yr2001","yr2002","yr2003","yr2004","yr2005",
                "yr2006","yr2007","yr2008","yr2009",
                "yr2010","yr2011","yr2012","yr2013","yr2014","yr2015",
                "yr2016","yr2017","yr2018","yr2019","yr2020")
  
  entidad_df<-entidad_df%>%
    tidyr::pivot_longer(
      cols = cols.num, 
      names_to = "year", 
      names_prefix = "yr", 
      values_to = "tipo_observacion" 
    )
  
  entidad_df<-entidad_df%>%
    mutate(tipo_observacion = gsub("-", NA, tipo_observacion))
  
  entidad_df<-entidad_df%>%
    mutate(ENTIDAD=nombre_entidad)
  
  return(entidad_df)
  
}

normalize<-function(x){
  
  mean_x=mean(x)
  sd_x=sd(x)
  y=(x-mean_x)/sd_x
  
  return(y)
}
  
#### I. Preparing the data ####
  
####1. Subnational states IDs ####

claves_df<-read_csv("Inputs/CLAVES.csv")

####2.Tax Database ####

taxes_1989 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_1989.csv")
taxes_1990 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_1990.csv")
taxes_1991 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_1991.csv")
taxes_1992 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_1992.csv")
taxes_1993 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_1993.csv")
taxes_1994 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_1994.csv")
taxes_1995 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_1995.csv")
taxes_1996 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_1996.csv")
taxes_1997 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_1997.csv")
taxes_1998 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_1998.csv")
taxes_1999 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_1999.csv")
taxes_2000 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_2000.csv")
taxes_2001 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_2001.csv")
taxes_2002 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_2002.csv")
taxes_2003 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_2003.csv")
taxes_2004 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_2004.csv")
taxes_2005 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_2005.csv")
taxes_2006 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_2006.csv")
taxes_2007 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_2007.csv")
taxes_2008 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_2008.csv")
taxes_2009 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_2009.csv")
taxes_2010 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_2010.csv")
taxes_2011 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_2011.csv")
taxes_2012 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_2012.csv")
taxes_2013 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_2013.csv")
taxes_2014 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_2014.csv")
taxes_2015 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_2015.csv")
taxes_2016 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_2016.csv")
taxes_2017 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_2017.csv")
taxes_2018 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_2018.csv")
taxes_2019 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_2019.csv")
taxes_2020 <- read_csv("Inputs/efipem_estatal_anual_tr_cifra_2020.csv")

local_taxes<-rbind(taxes_1989,taxes_1990,taxes_1991,taxes_1992,taxes_1993,taxes_1994,taxes_1995,taxes_1996,taxes_1997,
                   taxes_1998,taxes_1999,taxes_2000,taxes_2001,taxes_2002,taxes_2003,taxes_2004,taxes_2005,
                   taxes_2006, taxes_2007,taxes_2008,taxes_2009,taxes_2010, taxes_2011,taxes_2012,taxes_2013,taxes_2014,
                   taxes_2015,taxes_2016,taxes_2017,taxes_2018,taxes_2019,taxes_2020)


local_taxes_a<-local_taxes%>%
  filter(DESCRIPCION_CATEGORIA=="Impuestos" | DESCRIPCION_CATEGORIA=="Total de ingresos")


#####2a. Tax as revenue ####

names(local_taxes_a)[3]<-"year"

impuestos<-local_taxes_a%>%
  filter(DESCRIPCION_CATEGORIA=="Impuestos")

impuestos<- impuestos %>%
  mutate(year_entidad=str_c(year,"_",ID_ENTIDAD))

names(impuestos)[8]<-"total_impuestos_vcorriente"

ingresos<-local_taxes_a%>%
  filter(DESCRIPCION_CATEGORIA=="Total de ingresos")%>%
  mutate(year_entidad=str_c(year,"_",ID_ENTIDAD))

names(ingresos)[8]<-"ingresos_value"

ingresos<-ingresos%>%
  select(c("ingresos_value","year_entidad"))

tax_revenue<-impuestos %>%
  left_join(ingresos, by="year_entidad")

tax_revenue<-tax_revenue%>%
  mutate(tax_rev=total_impuestos_vcorriente/ingresos_value)


####3. Local GDP constant values####


local_gdp_constant<- read_csv("Inputs/Pib_estatal_2.csv")

cols.num <- c("yr2003","yr2004","yr2005","yr2006","yr2007","yr2008","yr2009","yr2010","yr2011",
              "yr2012","yr2013","yr2014","yr2015","yr2016","yr2017","yr2018","yr2019",
              "yr2020")

local_gdp_df_2<-local_gdp_constant%>%
  tidyr::pivot_longer(
    cols = cols.num, 
    names_to = "year", 
    names_prefix = "yr", 
    values_to = "local_gdp_constant" 
  )

local_gdp_df_2<-local_gdp_df_2%>% 
  left_join(claves_df)

local_gdp_df_2 <- local_gdp_df_2 %>%
  mutate(year_entidad=str_c(year,"_",ID_ENTIDAD))

####4. Conflicts ####

conflict_data <- read_csv("Inputs/conflict_data_mex.csv")

conflict_data<-conflict_data %>%
  mutate(adm_1=ifelse(adm_1=="Aguascalientes state","Aguascalientes",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Baja California state","Baja California",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Baja California Sur state","Baja California Sur",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Chiapas state","Chiapas",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Chihuahua state","Chihuahua",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Coahuila state","Coahuila",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Colima state","Colima",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Distrito Federal","Ciudad de Mexico",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Durango state","Durango",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Guanajuato state","Guanajuato",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Guerrero state","Guerrero",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Hidalgo state","Hidalgo",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Jalisco state","Jalisco",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="México state","Estado de Mexico",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Michoacán state","Michoacan",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Morelos state","Morelos",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Nayarit state","Nayarit",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Nuevo León state","Nuevo Leon",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Oaxaca state","Oaxaca",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Puebla state","Puebla",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Querétaro state","Queretaro",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Quintana Roo state","Quintana Roo",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="San Luis Potosí state","San Luis Potosi",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Sinaloa state","Sinaloa",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Sonora state","Sonora",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Tabasco state","Tabasco",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Tamaulipas state","Tamaulipas",adm_1)) %>%
  mutate(adm_1=ifelse(adm_1=="Tlaxcala state","Tlaxcala",adm_1)) %>% 
  mutate(adm_1=ifelse(adm_1=="Veracruz state","Veracruz",adm_1)) %>% 
  mutate(adm_1=ifelse(adm_1=="Yucatán state","Yucatan",adm_1)) %>% 
  mutate(adm_1=ifelse(adm_1=="Zacatecas state","Zacatecas",adm_1))


conflict_data<-conflict_data%>%
  mutate(total_deaths=deaths_a+deaths_b+deaths_civilians+deaths_unknown)

sum_conflict<-conflict_data %>%
  group_by(year,type_of_violence,adm_1) %>%
  summarise(n=n()) %>%
  arrange(year,adm_1) %>%
  ungroup()
 

names(sum_conflict)[3] <- "ENTIDAD" #adjust the names

sum_conflict<-sum_conflict%>% 
  left_join(claves_df)

sum_conflict<-sum_conflict %>%
  mutate(year_entidad=str_c(year,"_",ID_ENTIDAD))

sum_conflict_df<-as.data.frame(sum_conflict)

sum_conflict_df_2<-sum_conflict_df%>%
  filter(type_of_violence==2)

####5.Loading population ####

population_data <- read_csv("Inputs/poblacion_conapo.csv")


population_data<-population_data%>% 
  left_join(claves_df)

population_data<-population_data%>% 
  mutate(year_entidad=str_c(year,"_",ID_ENTIDAD))

###6. Loading homicide rates####

homicides_data <- read_csv("Inputs/Homicidios_ent_fed_csv.csv")

cols.num <- c("ID_01","ID_02","ID_03","ID_04","ID_05","ID_06","ID_07","ID_08",
              "ID_09","ID_10","ID_11","ID_12","ID_13","ID_14","ID_15",
              "ID_16","ID_17","ID_18","ID_19","ID_20","ID_21","ID_22",
              "ID_23","ID_24","ID_25","ID_26","ID_27","ID_28","ID_29",
              "ID_30","ID_31","ID_32")

homicides_data <-homicides_data %>%
  tidyr::pivot_longer(
    cols = cols.num, 
    names_to = "ID_ENTIDAD", 
    names_prefix = "ID_", 
    values_to = "Homicides_absolute" 
  )

homicides_data <-homicides_data %>%
  mutate(year_entidad=str_c(year,"_",ID_ENTIDAD))

homicides_data <-homicides_data %>%
  left_join(claves_df)

####7. Loading area#####


area_df<- read_csv("Inputs/area_csv.csv")

area_df <-area_df%>%
  left_join(claves_df)

####8.Loading ASF and building Corruption Index####


aguascalientes_asf<-readxl::read_xls("Inputs/ASF_new/Aguascalientes.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[1])  

bajacalifornia_asf<-readxl::read_xls("Inputs/ASF_new/BajaCalifornia.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[2])  

bajacaliforniasur_asf<-readxl::read_xls("Inputs/ASF_new/BajaCaliforniaSur.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[3])  

campeche_asf<-readxl::read_xls("Inputs/ASF_new/Campeche.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[4])  

coahuila_asf<-readxl::read_xls("Inputs/ASF_new/Coahuila.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[5])  

colima_asf<-readxl::read_xls("Inputs/ASF_new/Colima.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[6])  

chiapas_asf<-readxl::read_xls("Inputs/ASF_new/Chiapas.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[7])  

chihuahua_asf<-readxl::read_xls("Inputs/ASF_new/Chihuahua.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[8])  

durango_asf<-readxl::read_xls("Inputs/ASF_new/Durango.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[10])  

guanajuato_asf<-readxl::read_xls("Inputs/ASF_new/Guanajuato.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[11])  

guerrero_asf<-readxl::read_xls("Inputs/ASF_new/GuerreroXLS.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[12])  

hidalgo_asf<-readxl::read_xls("Inputs/ASF_new/Hidalgo.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[13])  

jalisco_asf<-readxl::read_xls("Inputs/ASF_new/Jalisco.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[14])  

estadomexico_asf<-readxl::read_xls("Inputs/ASF_new/EstadoMexico.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[15])  

michoacan_asf<-readxl::read_xls("Inputs/ASF_new/Michoacan.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[16])  

morelos_asf<-readxl::read_xls("Inputs/ASF_new/Morelos.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[17])  

nayarit_asf<-readxl::read_xls("Inputs/ASF_new/Nayarit.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[18])  

nuevoleon_asf<-readxl::read_xls("Inputs/ASF_new/NuevoLeon.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[19])  

oaxaca_asf<-readxl::read_xls("Inputs/ASF_new/Oaxaca.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[20])  

puebla_asf<-readxl::read_xls("Inputs/ASF_new/Puebla.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[21])  

queretaro_asf<-readxl::read_xls("Inputs/ASF_new/Queretaro.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[22])  

quintanaroo_asf<-readxl::read_xls("Inputs/ASF_new/QuintanaRoo.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[23])  

sanluispotosi_asf<-readxl::read_xls("Inputs/ASF_new/SanLuisPotosi.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[24])  

sinaloa_asf<-readxl::read_xls("Inputs/ASF_new/Sinaloa.xls") %>% 
  transformar_asf(.,claves_df$ENTIDAD[25])  

sonora_asf<-readxl::read_xls("Inputs/ASF_new/Sonora.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[26])  

tabasco_asf<-readxl::read_xls("Inputs/ASF_new/Tabasco.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[27])  

tamaulipas_asf<-readxl::read_xls("Inputs/ASF_new/Tamaulipas.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[28])  

tlaxcala_asf<-readxl::read_xls("Inputs/ASF_new/Tlaxcala.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[29])  

veracruz_asf<-readxl::read_xls("Inputs/ASF_new/Veracruz.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[30])  

yucatan_asf<-readxl::read_xls("Inputs/ASF_new/Yucatan.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[31])  

zacatecas_asf<-readxl::read_xls("Inputs/ASF_new/Zacatecas.xls")%>% 
  transformar_asf(.,claves_df$ENTIDAD[32])  

observaciones_asf<-rbind(aguascalientes_asf,bajacalifornia_asf,bajacaliforniasur_asf,
                       campeche_asf,coahuila_asf, colima_asf,chiapas_asf,chihuahua_asf,durango_asf,
                         guanajuato_asf,
                         guerrero_asf,hidalgo_asf,jalisco_asf,
                         estadomexico_asf,michoacan_asf,morelos_asf,nayarit_asf,
                         nuevoleon_asf,oaxaca_asf,puebla_asf,queretaro_asf,quintanaroo_asf,
                         sanluispotosi_asf,sinaloa_asf,sonora_asf,tabasco_asf,tamaulipas_asf,
                         tlaxcala_asf,veracruz_asf,yucatan_asf,zacatecas_asf) %>%
                    mutate(tipo_observacion=as.numeric(tipo_observacion))
                         
  observaciones_asf_cuenta<-observaciones_asf%>%
    group_by(ENTIDAD,year) %>%
    summarize(suma=sum(tipo_observacion,na.rm = T))
  
  observaciones_asf_corrupcion<-observaciones_asf%>%
    filter(tipo_accion=="denuncia"|tipo_accion=="promadmsanc"
           |tipo_accion=="fincamrespon"|tipo_accion=="informerespon")%>%
    group_by(ENTIDAD,year) %>%
    summarize(suma_corrup=sum(tipo_observacion,na.rm = T))
  
  observaciones_asf_cuenta<-observaciones_asf_cuenta%>%
    left_join(observaciones_asf_corrupcion,by=c("year","ENTIDAD"))
  
  observaciones_asf_cuenta<-observaciones_asf_cuenta%>%
    mutate(indicador_corrupcion_1=suma_corrup/suma)%>%
    mutate(indicador_corrupcion_1=ifelse(indicador_corrupcion_1=="NaN",0,indicador_corrupcion_1))
  

                      
observaciones_asf_2<-observaciones_asf%>%
  mutate(puntaje=case_when(tipo_accion=="recomend"~0,
                           tipo_accion=="recomenddesemp"~0,
                           tipo_accion=="prominterv"~0,
                           tipo_accion=="promadmsanc"~0,
                           tipo_accion=="promejerc"~2,
                           tipo_accion=="solicitaclar"~1,
                           tipo_accion=="pliego"~1,
                           tipo_accion=="fincamrespon"~3,
                           tipo_accion=="denuncia"~3,
                           tipo_accion=="informerespon"~3
                           ))%>%
mutate(ponderado=tipo_observacion*puntaje)


observaciones_asf_cuenta_2<-observaciones_asf_2%>%
  group_by(ENTIDAD,year) %>%
  summarize(suma_ponderado=sum(ponderado,na.rm = T))


observaciones_asf_cuenta<-observaciones_asf_cuenta%>%
  left_join(observaciones_asf_cuenta_2,by=c("year","ENTIDAD"))

observaciones_asf_cuenta<-observaciones_asf_cuenta%>%
  mutate(indice_ponderado=suma_ponderado/suma) %>%
  mutate(indice_ponderado=ifelse(indice_ponderado=="NaN",0,indice_ponderado))

observaciones_asf_cuenta<-observaciones_asf_cuenta%>%
  left_join(claves_df)

observaciones_asf_cuenta<-observaciones_asf_cuenta%>%
  mutate(year_entidad=str_c(year,"_",ID_ENTIDAD))

####9. Loading subnational party in government and building cohesiveness & transition####

pol_inst_df <- read_csv("Inputs/gobiernos_entidades.csv")

cols.num <- c("yr1989","yr1990","yr1991","yr1992","yr1993","yr1994",
              "yr1995","yr1996","yr1997",
              "yr1998","yr1999",
              "yr2000","yr2001","yr2002","yr2003","yr2004","yr2005",
              "yr2006","yr2007","yr2008","yr2009",
              "yr2010","yr2011","yr2012","yr2013","yr2014","yr2015",
              "yr2016","yr2017","yr2018","yr2019", "yr2020")

pol_inst_df <-pol_inst_df %>%
  tidyr::pivot_longer(
    cols = cols.num, 
    names_to = "year", 
    names_prefix = "yr", 
    values_to = "gov_party" 
  )


pol_inst_df<-pol_inst_df %>%
  mutate(align_president = case_when(year>=1989 & year<=2000 & gov_party!="PRI"~0,
                                     year>=2001 & year<=2012 & gov_party!="PAN"~0,
                                     year>=2013 & year<=2018 & gov_party!="PRI"~0,
                                     year>=2019 & year<=2020 & gov_party!="Morena"~0,
                                     TRUE ~ 1))


lag_pol_inst_df<-pol_inst_df %>%
  select(year, ID_ENTIDAD,gov_party)

lag_pol_inst_df<-lag_pol_inst_df %>% 
  mutate(year=as.numeric(year))%>% 
  mutate(year=year+1)%>% 
  mutate(year=as.character(year))%>% 
  select("year","ID_ENTIDAD","gov_party_lag"="gov_party")


pol_inst_df<-pol_inst_df%>%
  left_join(lag_pol_inst_df, by=c("year","ID_ENTIDAD"))

pol_inst_df<-pol_inst_df%>%
  mutate(transition=ifelse(gov_party==gov_party_lag,0,1))

pol_inst_df<-pol_inst_df%>%
  mutate(year_entidad=str_c(year,"_",ID_ENTIDAD))

pol_inst_df<-pol_inst_df%>%
  mutate(year_entidad=as.character(year_entidad))


####10. Loading Inequality#####

inequality_df<- read_csv("Inputs/inequality2000_2020.csv")


cols.num <- c("yr2000","yr2001","yr2002","yr2003","yr2004","yr2005","yr2006",
              "yr2007","yr2008","yr2009",
              "yr2010","yr2011","yr2012","yr2013","yr2014","yr2015",
              "yr2016","yr2017","yr2018","yr2019", "yr2020")


inequality_df <-inequality_df%>%
  tidyr::pivot_longer(
    cols = cols.num, 
    names_to = "year", 
    names_prefix = "yr", 
    values_to = "gini_index" 
  )

inequality_df_2<-inequality_df%>%
  group_by(ENTIDAD)%>%
  mutate(gini_index_spline = na.spline(gini_index))


inequality_df_2<-inequality_df_2%>%
  group_by(ENTIDAD)%>%
  mutate(gini_index_lineal = na.approx(gini_index))

inequality_df_2<-inequality_df_2%>%
  left_join(claves_df)

inequality_df_2<-inequality_df_2%>%
  mutate(year_entidad=str_c(year,"_",ID_ENTIDAD))

####11. Selecting variables for merging####


tax_revenue<-tax_revenue%>%
  select(c("year","ID_ENTIDAD","total_impuestos_vcorriente","year_entidad","ingresos_value","tax_rev"))

population_data<-population_data%>%
  select(c("year_entidad","poblacion"))

local_gdp_df_2<-local_gdp_df_2 %>%
  select(c("year_entidad","local_gdp_constant"))

homicides_data<-homicides_data%>%
  select(c("year_entidad","Homicides_absolute"))


sum_conflict_df_2<-sum_conflict_df_2%>%
  select(c("year_entidad","n","type_of_violence")) %>%
  drop_na()

observaciones_asf_cuenta<-observaciones_asf_cuenta %>% 
  as.data.frame(.) %>% 
  select("year_entidad","suma","indicador_corrupcion_1","indice_ponderado")

pol_inst_df<-pol_inst_df%>%
  select(-c("ID_ENTIDAD","ENTIDAD","year"))

inequality_df_2<-inequality_df_2%>%
  as.data.frame(.)%>%
  select(c("year_entidad","gini_index","gini_index_spline","gini_index_lineal"))

area_df<-area_df%>%
  select("ID_ENTIDAD","AREA_KM2")

####12. Merging ####


first_df<-tax_revenue%>%
  left_join(population_data, by="year_entidad")


first_df<-first_df%>%
  left_join(local_gdp_df_2, by="year_entidad")

first_df<-first_df%>%
  left_join(sum_conflict_df_2,by="year_entidad")

first_df<-first_df%>%
  left_join(homicides_data,by="year_entidad")

first_df<-first_df%>%
  left_join(observaciones_asf_cuenta,by="year_entidad")

first_df<-first_df%>%
  left_join(pol_inst_df,by="year_entidad")

first_df<-first_df%>%
  left_join(inequality_df_2,by="year_entidad")

first_df<-first_df%>%
  left_join(area_df,by="ID_ENTIDAD")

first_df<-first_df%>%
  left_join(claves_df,by="ID_ENTIDAD")


####13. Calculating and adjusting variables ####

first_df<-first_df%>%
  mutate(n_corrected=ifelse(is.na(n),0,n))

first_df<-first_df%>%
  mutate(gdp_capita_2=local_gdp_constant/poblacion)

first_df<-first_df%>% 
  mutate(gdp_capita_2m=gdp_capita_2*1000000)

first_df<-first_df%>%
  mutate(homicide_rate=Homicides_absolute/poblacion)%>%
  mutate(homicide_rate=homicide_rate*100000)

first_df<-first_df%>%
  mutate(population_density=poblacion/AREA_KM2)

first_df<-first_df%>%
  mutate(density=poblacion/AREA_KM2)

first_df<-first_df %>% 
  mutate(indicador_reversed=3-indice_ponderado)


first_df<-first_df%>% 
  ungroup()


####14. Lag variables#####

first_df<-first_df%>%
  arrange(ID_ENTIDAD,--year)

first_df<-first_df%>%
  group_by(ID_ENTIDAD) %>%  
  mutate(lag_tax_rev = dplyr::lag(tax_rev, n = 1, default = NA))%>% 
  ungroup()

first_df<-first_df%>%
  group_by(ID_ENTIDAD) %>%  
  mutate(lag_gini_index_spline = dplyr::lag(gini_index_spline, n = 1, default = NA))%>% 
  ungroup()


first_df<-first_df%>%
  group_by(ID_ENTIDAD) %>%  
  mutate(lag_homicide_rate= dplyr::lag(homicide_rate, n = 1, default = NA))%>% 
  ungroup()

first_df<-first_df%>%
  group_by(ID_ENTIDAD) %>%  
  mutate(lag_indice_ponderado= dplyr::lag(indice_ponderado, n = 1, default = NA))%>% 
  ungroup()

first_df<-first_df%>%
  group_by(ID_ENTIDAD) %>%  
  mutate(lag_indicador_reversed= dplyr::lag(indicador_reversed, n = 1, default = NA))%>% 
  ungroup()

first_df<-first_df%>%
  group_by(ID_ENTIDAD) %>%  
  mutate(lag_n_conflicts= dplyr::lag(n, n = 1, default = NA))%>% 
  ungroup()


first_df<-first_df%>%
  group_by(ID_ENTIDAD) %>%  
  mutate(lag_n_conflicts_corrected= dplyr::lag(n_corrected, n = 1, default = NA))%>% 
  ungroup()

first_df<-first_df%>%
  group_by(ID_ENTIDAD) %>%  
  mutate(lag_gdp_capita_2= dplyr::lag(gdp_capita_2, n = 1, default = NA))%>% 
  ungroup()

first_df<-first_df%>%
  group_by(ID_ENTIDAD) %>%  
  mutate(lag_gdp_capita_2m= dplyr::lag(gdp_capita_2m, n = 1, default = NA))%>% 
  ungroup()

 
####II. FINAL REGRESSIONS DATASET#####
 
####1. Filter years#### 
final_regressions_df<-first_df%>%
   filter(year>=2004)
 
####2. Lag and normalize variables####
 
final_regressions_df<-final_regressions_df %>%  
  mutate(lag_n_conflicts_std=normalize(lag_n_conflicts_corrected))

final_regressions_df<-final_regressions_df %>%  
  mutate(lag_gini_std=normalize(lag_gini_index_spline))
 
final_regressions_df<-final_regressions_df %>%  
  mutate(lag_gdp_capita_std=normalize(lag_gdp_capita_2))

final_regressions_df<-final_regressions_df %>%  
  mutate(lag_gdp_capita_std_m=normalize(lag_gdp_capita_2m))

final_regressions_df<-final_regressions_df %>%  
  mutate(lag_tax_std=normalize(lag_tax_rev))

final_regressions_df<-final_regressions_df %>%  
  mutate(lag_homicide_std=normalize(lag_homicide_rate))

final_regressions_df<-final_regressions_df %>%  
  mutate(lag_corrupt_std=normalize(lag_indicador_reversed))


####3. Normalize regular variables####

final_regressions_df<-final_regressions_df%>%
  mutate(tax_rev_sd=normalize(tax_rev))

final_regressions_df<-final_regressions_df%>%
  mutate(homicide_rates_sd=normalize(homicide_rate))

final_regressions_df<-final_regressions_df%>%
  mutate(indicador_rev_sd=normalize(indicador_reversed))


final_regressions_df<-final_regressions_df %>%  
  mutate(density_std_a=normalize(density))

####4.Calculate rates of change####

final_regressions_df <- final_regressions_df %>%  
  mutate(tax_rev_change=(tax_rev/lag_tax_rev)-1)

final_regressions_df <- final_regressions_df %>% 
  mutate(homicide_rate_change=(homicide_rate/lag_homicide_rate)-1)

final_regressions_df <- final_regressions_df %>% 
  mutate(indicador_reversed_change=(indicador_reversed/lag_indicador_reversed)-1)

####5. Run final regressions####

regression_std_aa<-plm(tax_rev~lag_n_conflicts_std + density_std_a + transition*lag_gini_std+align_president+lag_gdp_capita_std_m  + lag_tax_std, 
                       data = final_regressions_df, 
                       index = c("ID_ENTIDAD","year"), # unit
                       effect = "individual",
                       model = "within")


regression_std_bb<-plm(homicide_rate~lag_n_conflicts_std + density_std_a + transition*lag_gini_std+align_president+lag_gdp_capita_std_m +lag_homicide_std, 
                       data = final_regressions_df, 
                       index = c("ID_ENTIDAD","year"), # unit
                       effect = "individual",
                       model = "within")



regression_std_cc<-plm(indicador_reversed~lag_n_conflicts_std+ density_std_a  + transition*lag_gini_std+align_president+lag_gdp_capita_std_m + lag_corrupt_std, 
                       data = final_regressions_df, 
                       index = c("ID_ENTIDAD","year"), # unit
                       effect = "individual",
                       model = "within")

#stargazer::stargazer(regression_std_cc, type = "text")


####6. Prepare output tables for final fixed-effects regressions####

cov_labels<-c("Conflicts (lagged,standardized)","Population density (lagged,standardized)",
              "Party Transition","Gini (lagged, standardized)","Party aligned with federal executive",
              "GRP per capita (lagged,standardized)","Tax revenue(lagged,standardized)","Homicide rate (lagged,standardized)","Corruption Index (lagged standardized)","Party Transition*Gini (lagged,standardized)")

stargazer::stargazer(regression_std_aa,regression_std_bb,regression_std_cc, title="Regression results",
       align=TRUE, dep.var.labels=c("Tax Revenue","Homicide Rates","Corruption Index"), covariate.labels =cov_labels ,type="html",out = "tab_1.html" )


####7. Summary of hypothesis effects####

Hypothesis<-c("Cohesiveness (H1)","Stability (H2)","Income inequality (H3)",
              "Income inequality * Stability (H4)","Income per capita (H5)",
              "Internal conflicts (H6)","Lagged state capacity (H7)")

Extractive<-c("","↓ *","","","","","↑")
Coercive<-c("","","","","","↓","↑")
Administrative<-c("↑","","↓","↑*","","","↑")

table_effects<-data.frame(Hypothesis,Extractive,Coercive,Administrative)

table_effects %>% 
  knitr::kable() %>% 
  kableExtra::kable_styling()


####III. Descriptives of variables#####

####1.Table of dependent variables####

table1::label(final_regressions_df$tax_rev)<-"Tax as % of revenue"
table1::label(final_regressions_df$homicide_rate)<-"Homicide rates (per 100,000 persons)"
table1::label(final_regressions_df$indicador_reversed) <- "Corruption Index"

table_2<-table1(~tax_rev+homicide_rate+indicador_reversed,data=final_regressions_df)

table_2

####2. Graphs of dependent variables####

mean_tax_rev<-final_regressions_df %>% 
  group_by(ENTIDAD) %>%
  summarize(mean_tax_rev=mean(tax_rev)) 

final_regressions_df %>% 
  mutate(year=lubridate::ymd(year,truncated = 2L))%>%
  ggplot(.,aes(x = year,   y= tax_rev)) +
  geom_point(size=1) +
  geom_hline(data= mean_tax_rev, aes(yintercept = mean_tax_rev),col="grey",show.legend = FALSE) +
  facet_wrap(~ENTIDAD,ncol = 4,scales = "free")+
  scale_x_date(date_labels = "%Y")+
  theme_minimal()+
  theme(strip.text.x = element_text(size = 7))+
  theme(axis.text = element_text(size = 5.5)) +
  xlab("Year") + ylab("Tax revenue (%)")

mean_homicide_tab<-final_regressions_df %>% 
  group_by(ENTIDAD) %>%
  summarize(mean_homicide=mean(homicide_rate)) 

final_regressions_df %>% 
  mutate(year=lubridate::ymd(year,truncated = 2L))%>%
  ggplot(.,aes(x = year,   y= homicide_rate)) +
  geom_point(size=1) +
  geom_hline(data= mean_homicide_tab, aes(yintercept = mean_homicide),col="grey",show.legend = FALSE) +
  facet_wrap(~ENTIDAD,ncol = 4,scales = "free")+
  scale_x_date(date_labels = "%Y")+
  theme_minimal()+
  theme(strip.text.x = element_text(size = 7))+
  theme(axis.text = element_text(size = 5.5)) +
  xlab("Year") + ylab("Homicide Rates")

mean_indicador_tab<-final_regressions_df %>% 
  group_by(ENTIDAD) %>%
  summarize(mean_indicador=mean(indicador_reversed))

final_regressions_df %>% 
  mutate(year=lubridate::ymd(year,truncated = 2L))%>%
  ggplot(.,aes(x = year,   y= indicador_reversed)) +
  geom_point(size=1) +
  geom_hline(data= mean_indicador_tab, aes(yintercept = mean_indicador),col="grey",show.legend = FALSE) +
  facet_wrap(~ENTIDAD,ncol = 4,scales = "free")+
  scale_x_date(date_labels = "%Y")+
  theme_minimal()+
  theme(strip.text.x = element_text(size = 7))+
  theme(axis.text = element_text(size = 5.5)) +
  xlab("Year") + ylab("Corruption Index")


####3. Growth of dependent variables per subnational entity####

tax_rev_growth<-final_regressions_df %>% 
  group_by(ENTIDAD) %>%  
  summarize(tax_rev_change_m=mean(tax_rev_change))

homicide_rate_growth<-final_regressions_df %>% 
  group_by(ENTIDAD) %>%  
  summarize(homicide_rate_change_m=mean(homicide_rate_change))

indicador_reversed_growth<-final_regressions_df %>% 
  group_by(ENTIDAD) %>%  
  summarize(indicador_reversed_change_m=mean(indicador_reversed_change))


####4. Classifying subnational entities per quantile of dependent variables, average over time####

mean_dependent<-final_regressions_df %>% 
  group_by(ENTIDAD) %>% 
  summarize(tax_rev_m=mean(tax_rev),homicide_rate_m=mean(homicide_rate),indicador_reversed=mean(indicador_reversed))

mean_tax_quant<-quantile(mean_dependent$tax_rev_m)
mean_homicide_quant<-quantile(mean_dependent$homicide_rate_m)
mean_corrupt_quant<-quantile(mean_dependent$indicador_reversed)


mean_dependent_quantiles<-mean_dependent %>% 
  mutate(tax_rev_q=ntile(tax_rev_m,4)) %>% 
  mutate(homicide_rate_q=5-ntile(homicide_rate_m,4))%>% 
  mutate(indicador_reversed_q=ntile(indicador_reversed,4))%>% 
  mutate(total_q=tax_rev_q+homicide_rate_q+indicador_reversed_q)

 
mean_dependent_quantiles_selection<-mean_dependent_quantiles %>% 
  select("ENTIDAD","tax_rev_q","homicide_rate_q","indicador_reversed_q","total_q")

mean_dependent_quantiles_selection %>%
  arrange(desc(total_q)) 

cols.num <- c("tax_rev_q","homicide_rate_q","indicador_reversed_q")

mean_dependent_quantiles_selection<-mean_dependent_quantiles_selection%>%
  tidyr::pivot_longer(
    cols = cols.num, 
    names_to = "capacity_dimension", 
    values_to = "quantile" 
  )

mean_dependent_quantiles_selection<-mean_dependent_quantiles_selection %>%
  arrange(desc(total_q)) 

mean_dependent_quantiles_selection<-mean_dependent_quantiles_selection %>% 
  group_by(ENTIDAD) %>%
  mutate(label_y=cumsum(quantile))

mean_dependent_quantiles_selection %>%
  arrange(desc(total_q)) %>% 
ggplot(., aes(fill=capacity_dimension, y=quantile, x=ENTIDAD)) + 
  geom_col(position="stack", stat="identity") +
  geom_text(aes(label = quantile), vjust = 1.5, colour = "white") + 
  scale_y_continuous(n.breaks=12) +
  scale_fill_manual(values=wes_palette(name="Darjeeling1"), labels = c("Homicide rates","Corruption Index","Tax % revenue")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust=1)) +
  theme(axis.text.y=element_blank(), title=element_blank())


mean_dependent_quantiles_selection %>%
  arrange(desc(total_q)) %>% 
  ggplot(., aes(fill=capacity_dimension, y=quantile, x=ENTIDAD)) + 
  geom_bar(position="stack", stat="identity") +
  scale_y_continuous(n.breaks=12) +
  scale_fill_grey(labels=c("Homicide rates","Corruption Index","Tax % revenue")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust=1)) +
  theme(title=element_blank())


####5. Table of independent variables####


table1::label(final_regressions_df$n_corrected) <- "Number of conflicts"
table1::label(final_regressions_df$gini_index_spline) <- "Gini index"
table1::label(final_regressions_df$gdp_capita_2m) <- "Gross regional product per capita"
table1::label(final_regressions_df$density) <- "Density"


table_3<-table1(~n_corrected +gini_index_spline+gdp_capita_2m+density,data=final_regressions_df)

table_3
  

final_regressions_df %>%  
  group_by(transition)%>%
  summarise(n = n()) %>%
  mutate(freq = round(n / sum(n)*100,2)) %>% 
   knitr::kable(
    col.names = c("Transition", "Number",
                  "Share(%)")) %>%  
    kableExtra::kable_styling()

final_regressions_df %>%  
  group_by(align_president)%>%
  summarise(n = n()) %>%
  mutate(freq = round(n / sum(n)*100,2)) %>% 
  knitr::kable(
    col.names = c("Alignment with President", "Number",
                  "Share(%)")) %>%  
  kableExtra::kable_styling()

transitions_per_state<-final_regressions_df %>% 
  group_by(ENTIDAD) %>% 
  summarize(transitions_total=sum(transition))

align_per_state<-final_regressions_df %>% 
  group_by(ENTIDAD) %>% 
  summarize(aligns_total=sum(align_president))


ggplot(transitions_per_state, aes(x = ENTIDAD, y= transitions_total, fill=ENTIDAD)) +
  geom_bar(stat="identity", alpha=0.4) +
  guides(fill=FALSE)+
  labs(x = "State ",
       y = "Number of transitions")+
  theme(axis.text.x=element_text(angle=55,hjust=1,vjust=1))  +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "grey",
                                        size = 0.5, linetype = "solid"))+
  scale_fill_grey(end = 0) 


ggplot(align_per_state, aes(x = ENTIDAD, y= aligns_total, fill=ENTIDAD)) +
  geom_bar(stat="identity", alpha=0.4) +
  guides(fill=FALSE)+
  labs(x = "State ",
       y = "Years with alignment")+
  theme(axis.text.x=element_text(angle=55,hjust=1,vjust=1))  +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "grey",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "white"))+
  scale_fill_grey(end = 0)  +
  scale_y_continuous(n.breaks=15)


####6. Graphs of independent variables####


mean_conflicts<-final_regressions_df %>% 
  group_by(ENTIDAD) %>%
  summarize(mean_con=mean(n_corrected))

final_regressions_df %>% 
  mutate(year=lubridate::ymd(year,truncated = 2L))%>%
  ggplot(.,aes(x = year,   y= n_corrected)) +
  geom_point(size=.5) +
  geom_hline(data= mean_conflicts, aes(yintercept = mean_con),col="lightgrey",show.legend = FALSE) +
  facet_wrap(~ENTIDAD,ncol = 4,scales = "free")+
  scale_x_date(date_labels = "%Y")+
  theme_minimal()+
  theme(strip.text.x = element_text(size = 7))+
  theme(axis.text = element_text(size = 5.5)) +
  xlab("Year") + ylab("Conflicts")

mean_percapita<-final_regressions_df %>% 
  group_by(ENTIDAD) %>%
  summarize(mean_perc=mean(gdp_capita_2m))

final_regressions_df %>% 
  mutate(year=lubridate::ymd(year,truncated = 2L))%>%
  ggplot(.,aes(x = year,   y= gdp_capita_2m)) +
  geom_point(size=.5) +
  geom_hline(data= mean_percapita, aes(yintercept = mean_perc),col="lightgrey",show.legend = FALSE) +
  facet_wrap(~ENTIDAD,ncol = 4,scales = "free")+
  scale_x_date(date_labels = "%Y")+
  theme_minimal()+
  theme(strip.text.x = element_text(size = 7))+
  theme(axis.text = element_text(size = 5.5)) +
  xlab("Year") + ylab("Product per capita")


mean_gini<-final_regressions_df %>% 
  group_by(ENTIDAD) %>%
  summarize(mean_gin=mean(gini_index_spline))

final_regressions_df %>% 
  mutate(year=lubridate::ymd(year,truncated = 2L))%>%
  ggplot(.,aes(x = year,   y= gini_index_spline)) +
  geom_point(size=.5) +
  geom_hline(data= mean_gini, aes(yintercept = mean_gin),col="lightgrey",show.legend = FALSE) +
  facet_wrap(~ENTIDAD,ncol = 4,scales = "free")+
  scale_x_date(date_labels = "%Y")+
  theme_minimal()+
  theme(strip.text.x = element_text(size = 7))+
  theme(axis.text = element_text(size = 5.5)) +
  xlab("Year") + ylab("Gini Index")

mean_density<-final_regressions_df %>% 
  group_by(ENTIDAD) %>%
  summarize(mean_den=mean(density))

final_regressions_df %>% 
  mutate(year=lubridate::ymd(year,truncated = 2L))%>%
  ggplot(.,aes(x = year,   y= density)) +
  geom_point(size=.5) +
  geom_hline(data= mean_density, aes(yintercept = mean_den),col="lightgrey",show.legend = FALSE) +
  facet_wrap(~ENTIDAD,ncol = 4,scales = "free")+
  scale_x_date(date_labels = "%Y")+
  theme_minimal()+
  theme(strip.text.x = element_text(size = 7))+
  theme(axis.text = element_text(size = 5.5)) +
  xlab("Year") + ylab("Population Density")



#### IV. Robustness and checking regression assumptions####

####1. ANOVA####

anova_1<-aov(tax_rev~ID_ENTIDAD,data=final_regressions_df)
summary(anova_1)

anova_2<-aov(homicide_rate~ID_ENTIDAD,data=final_regressions_df)
summary(anova_2)

anova_3<-aov(indicador_reversed~ID_ENTIDAD, data=final_regressions_df)
summary(anova_3)

a1<-summary(anova_1)[[1]]
a2<-summary(anova_2)[[1]]
a3<-summary(anova_3)[[1]]

anova_results <- data.frame(cbind(c("ID_ENTIDAD", "Residuals", "ID_ENTIDAD", "Residuals","ID_ENTIDAD", "Residuals"), 
                                  rbind(a1, a2,a3))) 
colnames(anova_results) <- c("", "DF", "Sum Sq.","Mean Sq." , "F value", "Pr(>F)")
row.names(anova_results) <- NULL

anova_results %>% kable("html", digits=2) %>% 
  kable_styling(bootstrap_options = "striped", full_width = F) %>% 
  pack_rows(., "Tax % revenue", 1, 2) %>% # groups rows with label
  pack_rows(., "Homicide rate", 3, 4) %>% 
  pack_rows(., "Corruption Index", 5, 6) 


####2.Random vs fixed effects####

regression_std_aa_random<-plm(tax_rev~lag_n_conflicts_std + density_std_a + transition*lag_gini_std+align_president+lag_gdp_capita_std_m  + lag_tax_std, 
                       data = final_regressions_df, 
                       index = c("ID_ENTIDAD","year"), # unit
                       effect = "individual",
                       model = "random")

test_a<-phtest(regression_std_aa,regression_std_aa_random)


regression_std_bb_random<-plm(homicide_rate~lag_n_conflicts_std + density_std_a + transition*lag_gini_std+align_president+lag_gdp_capita_std_m +lag_homicide_std, 
                       data = final_regressions_df, 
                       index = c("ID_ENTIDAD","year"), # unit
                       effect = "individual",
                       model = "random")
 
test_b<-phtest(regression_std_bb,regression_std_bb_random)


regression_std_cc_random<-plm(indicador_reversed~lag_n_conflicts_std+ density_std_a  + transition*lag_gini_std+align_president+lag_gdp_capita_std_m + lag_corrupt_std, 
                       data = final_regressions_df, 
                       index = c("ID_ENTIDAD","year"), # unit
                       effect = "individual",
                       model = "random")

test_c<-phtest(regression_std_cc,regression_std_cc_random)


hausman_tests<-rbind(c("Tax % revenue","Homicide rate","Corruption Index"),c(test_a[2]$p.value,test_b[2]$p.value,test_c[2]$p.value))

hausman_tests %>% 
  knitr::kable("html", digits=5) %>% 
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F)

####3. Random effects results table####
 

cov_labels<-c("Conflicts (lagged,standardized)","Population density (lagged,standardized)",
              "Party Transition","Gini (lagged, standardized)","Party aligned with federal executive",
              "GRP per capita (lagged,standardized)","Tax revenue(lagged,standardized)","Homicide rate (lagged,standardized)","Corruption Index (lagged standardized)","Party Transition*Gini (lagged,standardized)")

stargazer::stargazer(regression_std_aa_random,regression_std_bb_random,regression_std_cc_random, title="Regression results with random effects",
                     align=TRUE, dep.var.labels=c("Tax Revenue","Homicide Rates","Corruption Index"), covariate.labels =cov_labels ,type="html",out = "table_random_effects.html" )


 
####4. LM models and residual plots####

 
lm_std_aa<-lm(tax_rev~lag_n_conflicts_std + density_std_a + transition*lag_gini_std+align_president+lag_gdp_capita_std_m  + lag_tax_std + factor(ENTIDAD)-1,data = final_regressions_df)
summary(lm_std_aa) 

layout(matrix(1:4, ncol = 2))
plot(lm_std_aa)

lm_std_bb<-lm(homicide_rate~lag_n_conflicts_std + density_std_a + transition*lag_gini_std+align_president+lag_gdp_capita_std_m +lag_homicide_std+ factor(ENTIDAD)-1,data = final_regressions_df)
summary(lm_std_bb)
plot(lm_std_bb)       

lm_std_cc<-lm(indicador_reversed~lag_n_conflicts_std+ density_std_a  + transition*lag_gini_std+align_president+lag_gdp_capita_std_m + lag_corrupt_std+ factor(ENTIDAD)-1,data = final_regressions_df) 
summary(lm_std_cc)

plot(lm_std_cc)             

####5. Prediction values vs real values plots####

predict_tax_rev<-predict(lm_std_aa)
predict_homicide_rate<-predict(lm_std_bb)
predict_indicador_reversed<-predict(lm_std_cc)

final_regression_predict<-cbind(final_regressions_df,predict_tax_rev)
final_regression_predict<-cbind(final_regression_predict,predict_homicide_rate)
final_regression_predict<-cbind(final_regression_predict,predict_indicador_reversed)

par(mfrow=c(1,1))
dev.off()

final_regression_predict%>% 
  mutate(year=lubridate::ymd(year,truncated = 2L))%>%
  ggplot(.,aes(x = year)) +
  geom_point(aes(y=tax_rev), color="grey", fill="black") +
  geom_point(aes(y=predict_tax_rev), color="red", size=.3) +
  facet_wrap(~ENTIDAD,ncol = 4,scales = "free")+
  scale_x_date(date_labels = "%Y")+
  theme_minimal()+
  theme(strip.text.x = element_text(size = 7))+
  theme(axis.text = element_text(size = 3.5)) +
  xlab("Year") + ylab("Tax % revenue")

final_regression_predict%>% 
  mutate(year=lubridate::ymd(year,truncated = 2L))%>%
  ggplot(.,aes(x = year)) +
  geom_point(aes(y=homicide_rate),color="grey", fill="black") +
  geom_point(aes(y=predict_homicide_rate), color="red", size=.3) +
  facet_wrap(~ENTIDAD,ncol = 4,scales = "free")+
  scale_x_date(date_labels = "%Y")+
  theme_minimal()+
  theme(strip.text.x = element_text(size = 7))+
  theme(axis.text = element_text(size = 3.5)) +
  xlab("Year") + ylab("Homicide rate")

final_regression_predict%>% 
  mutate(year=lubridate::ymd(year,truncated = 2L))%>%
  ggplot(.,aes(x = year)) +
  geom_point(aes(y=indicador_reversed),color="grey", fill="black") +
  geom_point(aes(y=predict_indicador_reversed), color="red", size=.3) +
  facet_wrap(~ENTIDAD,ncol = 4,scales = "free")+
  scale_x_date(date_labels = "%Y")+
  theme_minimal()+
  theme(strip.text.x = element_text(size = 7))+
  theme(axis.text = element_text(size = 3.5)) +
  xlab("Year") + ylab("Corruption index")


####6. Naive models####

lm_naive_aa<-lm(tax_rev~lag_n_conflicts_std + density_std_a + transition*lag_gini_std+align_president+lag_gdp_capita_std_m  + lag_tax_std,data = final_regressions_df)
lm_naive_bb<-lm(homicide_rate~lag_n_conflicts_std + density_std_a + transition*lag_gini_std+align_president+lag_gdp_capita_std_m +lag_homicide_std,data = final_regressions_df)
lm_naive_cc<-lm(indicador_reversed~lag_n_conflicts_std+ density_std_a  + transition*lag_gini_std+align_president+lag_gdp_capita_std_m + lag_corrupt_std,data = final_regressions_df) 



cov_labels<-c("Conflicts (lagged,standardized)","Population density (lagged,standardized)",
              "Party Transition","Gini (lagged, standardized)","Party aligned with federal executive",
              "GRP per capita (lagged,standardized)","Tax revenue(lagged,standardized)","Homicide rate (lagged,standardized)","Corruption Index (lagged standardized)",
              "Party Transition*Gini (lagged,standardized)", "Constant")

stargazer::stargazer(lm_naive_aa,lm_naive_bb,lm_naive_cc, title="Naive regressions results",
                     align=TRUE, dep.var.labels=c("Tax Revenue","Homicide Rates","Corruption Index"),covariate.labels =cov_labels, type="html",out = "table_naive.html")

####7. Correlation of state capacity dimensions####

x<-final_regressions_df%>%
  select('Tax % revenue'=tax_rev,'Homicide rates'=homicide_rate,'Corruption Index'=indicador_reversed)

M<-cor(x, method = c("pearson"))


M%>%
  knitr::kable() %>% 
  kableExtra::kable_styling()

