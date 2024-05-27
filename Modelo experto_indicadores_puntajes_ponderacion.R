library(readxl)
library(tidyverse)
library(openxlsx)
#####################################  #CALCULO DE INDICADORES  ####################################
#Nota: se necesita aplicar una funcion en excel para agregar la columna Tipo_creditocon el subsegmento de productivo
base <- read_xlsx("C:/Users/PERSONAL/Documents/TRABAJO/JUEVES 23 MAYO/Entidades_unicas_cuentas (homologada2).xlsx",sheet = 1)
# base$Tipo_credito <- ifelse(base$CUENTA_401>5000000,"Productivo corporativo",
#                                 ifelse(base$CUENTA_401>1000000 & base$CUENTA_401<=5000000,"Productivo empresarial",
#                                        ifelse(base$CUENTA_401>100000 & base$CUENTA_401<=1000000,"Productivo pymes","Microcrédito")))
# 
# base <- base %>% #group_by(IDENTIFICACION,NOMBRE,`Actividad Económica`) %>% 
#   mutate(Tipo_credito=case_when(CUENTA_401>5000000 ~ "Productivo corporativo",
#                                 CUENTA_401>1000000 & CUENTA_401<=5000000 ~ "Productivo empresarial",
#                                 CUENTA_401>100000 & CUENTA_401<=1000000 ~ "Productivo pymes"))

unique(base$Tipo_credito)
table(base$Tipo_credito)
view(base %>% select(CUENTA_401,Tipo_credito))
names(base_sub)
#Indicadores

base_sub <- base %>% filter(CUENTA_1!=".") %>% 
  select(IDENTIFICACION,NOMBRE,`Actividad Económica`,`CIIU NIVEL 1`,`CIIU NIVEL 6`,`Descripción CIIU NIVEL 1`,everything()) %>% 
  mutate_at(vars(7:24),as.numeric)

base_total <- base_sub %>% filter(CUENTA_1!=".") %>% 
  group_by(IDENTIFICACION,NOMBRE,`Actividad Económica`,Tipo_credito) %>% 
  summarise(Liquidez_Corriente=sum(CUENTA_101)/sum(CUENTA_201),
            Prueba_Acida=(sum(CUENTA_101)-sum(CUENTA_10103))/sum(CUENTA_201),
            Endeudamiento_de_Activo=sum(CUENTA_2)/sum(CUENTA_1),
            Endeudamiento_patrimonial=sum(CUENTA_2)/sum(CUENTA_3),
            Endeudamiento_corto_plazo=sum(CUENTA_201)/sum(CUENTA_2),
            
            Utilidad_Operacional=(sum(CUENTA_401)-sum(CUENTA_501)-(sum(CUENTA_50202)+sum(50201))),#ingresos activ ordi-costo de ventas y produccion-
            Cobertura_de_Intereses=sum(Utilidad_Operacional)/sum(CUENTA_50203),
            Rentabilidad_Financiera=sum(CUENTA_707)/sum(CUENTA_3),
            Rentabilidad_Operacional_del_Activo=(sum(Utilidad_Operacional)/sum(CUENTA_1))
            
  ) %>% select("IDENTIFICACION","NOMBRE","Actividad Económica","Tipo_credito",
               "Liquidez_Corriente","Prueba_Acida","Endeudamiento_de_Activo" ,
               "Endeudamiento_patrimonial", "Endeudamiento_corto_plazo",
               "Cobertura_de_Intereses" , "Rentabilidad_Financiera",
               Utilidad_Operacional="Rentabilidad_Operacional_del_Activo")

#nota: algunos valores estan en cero por lo que el resultado puede ser 0/0 y R lo interpreta como NaN que al final de cuentas lo exporta como NA (vacio)
#en el caso de divisiones para cero R lo interpreta como infinito
base_total

names(base_total)

#guardar la base de las entidades con sus respectivos indicadores de riesgo calculados
library(writexl)
#write_xlsx(base_total,path = "C:/Users/Franklin.Paucar/Downloads/base_indicadores.xlsx")
write_xlsx(base_total,path = "C:/Users/PERSONAL/Documents/TRABAJO/JUEVES 23 MAYO/base_indicadores.xlsx")

#################################### ASIGNACIÓN DE PUNTAJES  ###########################################
#lectura de la base: puntos de corte de excel
puntos_corte <- read_xlsx("C:/Users/PERSONAL/Documents/TRABAJO/JUEVES 23 MAYO/Productivo_Puntos_Cortes_max_min(version2).xlsx",sheet = 1)
names(puntos_corte) <- c("Tipo_credito",names(puntos_corte)[-1])
pc_sel <- puntos_corte %>% select(Tipo_credito,`Actividad Económica`,Indicadores,max,min,calificacion,`Puntaje(%)`)
#nota: las base final de puntos de corte calculados debe tener el maximo, minimo, la calificacion y el puntaje respectivo

base_total_pivot <- base_total %>% pivot_longer(!c("IDENTIFICACION","NOMBRE","Actividad Económica","Tipo_credito"),
                                                names_to = "Indicadores", values_to = "valor_indicador") %>% ungroup()
#write_xlsx(base_total_pivot,path = "C:/Users/Franklin.Paucar/Downloads/base_indicadores_pivot.xlsx")

#### Asignación de puntajes:se asignan todas las posibilidades de puntajes para cada indicador
#y luego se filtran solo los puntajes que esten dentro del intervalo correcto
result <- base_total_pivot %>% ungroup() %>% 
  inner_join(pc_sel %>% ungroup(), by = c("Tipo_credito" = "Tipo_credito",
                                          "Actividad Económica"="Actividad Económica",
                                          "Indicadores" = "Indicadores")) %>%
  filter(valor_indicador >= min & valor_indicador < max) 
#write_xlsx(result,path = "C:/Users/PERSONAL/Documents/TRABAJO/test_join_gg.xlsx")


#se unen los valores de los indicadores iniciales con los resultados de la asignacion de puntaje
# de modo que se agregará NA donde no hay coincidencia
result2 <- base_total_pivot %>%
  left_join(result,by = c("IDENTIFICACION"="IDENTIFICACION",
                          "NOMBRE"="NOMBRE",
                          "Actividad Económica"="Actividad Económica",
                          "Tipo_credito" = "Tipo_credito",
                          "Indicadores" = "Indicadores",
                          "valor_indicador"="valor_indicador"))


result2
write_xlsx(result2,path = "C:/Users/PERSONAL/Documents/TRABAJO/test_calificacion.xlsx")


############################### AGREGAR LAS PONDERACIONES ############################################
#se crea una tabla con los ponderadores de acuerdo al documento ficha_modelo_experto.xlsx
df_ponderadores <- tibble(Indicadores=c("Liquidez_Corriente","Prueba_Acida","Endeudamiento_de_Activo",
                                        "Endeudamiento_patrimonial","Endeudamiento_corto_plazo","Cobertura_de_Intereses",
                                        "Rentabilidad_Financiera","Utilidad_Operacional"),
                          ponderador=c(0.054,0.0388573119068869,0.0441755675742551,0.0416339274105859,
                                       0.0431176117273298,0.0349,0.0592,0.0592))

#se agregan las ponderaciones correspondientes y se calcula el valor final
resultado_final <- result2 %>% left_join(df_ponderadores) %>% mutate(valor_final=`Puntaje(%)`*ponderador)

write_xlsx(resultado_final,path = "C:/Users/PERSONAL/Documents/TRABAJO/Resultado_final_indicadores_modelo_experto.xlsx")






