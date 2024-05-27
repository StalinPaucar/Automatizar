library(readxl)
library(tidyverse)
library(openxlsx)
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
View(base_total)
names(base_total)
base_total[base_total$Cobertura_de_Intereses==Inf,c(1,2,3,4,10)]
x <- Inf
x
library(writexl)
#write_xlsx(base_total,path = "C:/Users/Franklin.Paucar/Downloads/base_indicadores.xlsx")
write_xlsx(base_total,path = "C:/Users/PERSONAL/Documents/TRABAJO/JUEVES 23 MAYO/base_indicadores.xlsx")

puntos_corte <- read_xlsx("C:/Users/PERSONAL/Documents/TRABAJO/JUEVES 23 MAYO/Productivo_Puntos_Cortes_max_min(version2).xlsx",sheet = 1)
names(puntos_corte) <- c("Tipo_credito",names(puntos_corte)[-1])
pc_sel <- puntos_corte %>% select(Tipo_credito,`Actividad Económica`,Indicadores,max,min,calificacion,`Puntaje(%)`)

base_total_pivot <- base_total %>% pivot_longer(!c("IDENTIFICACION","NOMBRE","Actividad Económica","Tipo_credito"),
                            names_to = "Indicadores", values_to = "valor_indicador") %>% ungroup()
#write_xlsx(base_total_pivot,path = "C:/Users/Franklin.Paucar/Downloads/base_indicadores_pivot.xlsx")


df_ponderadores <- tibble(Indicadores=c("Liquidez_Corriente","Prueba_Acida","Endeudamiento_de_Activo",
                   "Endeudamiento_patrimonial","Endeudamiento_corto_plazo","Cobertura_de_Intereses",
                   "Rentabilidad_Financiera","Utilidad_Operacional"),
       ponderador=c(0.054,0.0388573119068869,0.0441755675742551,0.0416339274105859,
                   0.0431176117273298,0.0349,0.0592,0.0592))

resultado_final <- result2 %>% left_join(df_ponderadores) %>% mutate(valor_final=`Puntaje(%)`*ponderador)

unique(base_total_pivot$Indicadores)
unique(puntos_corte$Indicadores)

bases <- list(base_total_pivot,puntos_corte)
todo <- reduce(bases,merge,all=TRUE)
todo

write_xlsx(todo,path = "C:/Users/Franklin.Paucar/Downloads/base_test_calificacion.xlsx")


df1 <- puntos_corte %>% filter(Tipo_credito==unique(puntos_corte$Tipo_credito)[1],
                        `Actividad Económica`==unique(puntos_corte$`Actividad Económica`)[1],
                        Indicadores==unique(puntos_corte$Indicadores)[1])



df2 <- base_total_pivot %>% filter(Tipo_credito==unique(puntos_corte$Tipo_credito)[1],
                                   `Actividad Económica`==unique(puntos_corte$`Actividad Económica`)[1],
                                   Indicadores==unique(puntos_corte$Indicadores)[1]) 

view(df2)

pc_sel <- puntos_corte %>% select(Tipo_credito,`Actividad Económica`,Indicadores,max,min,calificacion)
bases <- list(base_total_pivot,pc_sel)
todo <- as_tibble(reduce(bases,merge,all=FALSE))
todo

base_total_pivot %>% left_join(pc_sel, by = c("Tipo_credito" = "Tipo_credito",
                                              "Actividad Económica"="Actividad Económica",
                                              "Indicadores" = "Indicadores"))
btp_sel <- base_total_pivot %>% select(Tipo_credito,`Actividad Económica`,Indicadores,)
GG <- base_total_pivot %>% left_join(pc_sel, by = c("Tipo_credito" = "Tipo_credito",
                                              "Actividad Económica"="Actividad Económica",
                                              "Indicadores" = "Indicadores"))
my_function(base_total_pivot, pc_sel, join = c("Tipo_credito", "Actividad Económica","Indicadores"))

write_xlsx(GG,path = "C:/Users/Franklin.Paucar/Downloads/test_calificacion.xlsx")


base_fil <- base_total_pivot %>% ungroup()
distinct(base_fil,`Actividad Económica`,Tipo_credito,Indicadores) #%>% select(`Actividad Económica`,Tipo_credito,Indicadores))

base_total_pivot %>% ungroup() %>% select(`Actividad Económica`,Tipo_credito,Indicadores)
base_total_pivot %>% select(`Actividad Económica`,Tipo_credito,Indicadores)





# Ejemplo de datos
df_intervalos <- data.frame(
  maximos = c(100, 200, 300, 400, 500, 600, 700, 800, 900),
  minimos = c(0, 100, 200, 300, 400, 500, 600, 700, 800),
  percentil=c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9"),
  calificacion=c("E","D","C2","C1","B2","B1","A3","A2","A1")
)

df_datos <- data.frame(
  entidades = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J",
                "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T"),
  monto = c(50, 150, 250, 350, 450, 550, 650, 750, 850, 950)
)
length(df_intervalos$maximos)
df_datos %>% mutate(min=case_when(monto>0 ~ "Productivo corporativo",
                                  CUENTA_401>1000000 & CUENTA_401<=5000000 ~ "Productivo empresarial",
                                  CUENTA_401>100000 & CUENTA_401<=1000000 ~ "Productivo pymes"))
maximos <- df_intervalos$maximos
minimos <- df_intervalos$minimos
percentil <- df_intervalos$percentil
calificacion <- df_intervalos$calificacion
monto <- df_datos$monto
entidad <- df_datos$entidades
for (i in 1:length(maximos)) {
  if (monto >= minimos[i] && monto < maximos[i]) {
    respuesta <- data.frame(entidad=entidad[i],monto=monto[i],min=minimos[i],max=maximos[i],percentil=percentil[i],calificacion=calificacion[i])
    #return(respuesta)
  }
}
respuesta
df_intervalos$minimos
# Crear una función para etiquetar los intervalos
etiquetar_intervalo <- function(monto, maximos, minimos,percentil,calificacion) {
  for (i in 1:length(maximos)) {
    if (monto >= minimos[i] && monto < maximos[i]) {
      # min <- minimos[i]
      # max <- maximos[i]
      # perc <- percentil[i]
      # cal <- calificacion[i]
      respuesta <- data.frame(min=minimos[i],max=maximos[i],percentil=percentil[i],calificacion=calificacion[i])
      return(respuesta)
    }
  }
  return(NA)  # Si no se encuentra en ningún intervalo
}
vars <- c()
# Aplicar la función a la columna "monto" y crear una nueva columna en df_datos
gg <- etiquetar_intervalo(df_datos$monto,df_intervalos$maximos, df_intervalos$minimos,df_intervalos$percentil,df_intervalos$calificacion)
class(gg)
df_datos$monto <- (mapply(etiquetar_intervalo, df_datos$monto, df_intervalos$maximos, df_intervalos$minimos,df_intervalos$percentil,df_intervalos$calificacion))
total <- c(vars,)
# Ver el resultado
print(df_datos)


# VENTAS=40101 VENTA DE BIENES
#50201
# Endeudamiento_Activo_Fijo=sum(3)/sum(),#
# Apalancamiento=sum(1)/sum(3),
# Apalancamiento_Financiero=,#
# Rotacion_Cartera=sum(4101)/sum(113),
# Rotacion_Activo_Fijo=sum(4101)/sum(actvos fijos),
# Rotación_Ventas=sum(4101)/sum(1),
# Periodo_Medio_Cobranza=sum(113)*365/sum(4101)