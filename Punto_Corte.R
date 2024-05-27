library(readxl)
library(tidyverse)
library(openxlsx)
corporativo <- read_xlsx("C:/Users/PERSONAL/Documents/TRABAJO/2024-05-22/punto_de_corte_a.xlsx",sheet = "Productivo corporativo")
pymes <- read_xlsx("C:/Users/PERSONAL/Documents/TRABAJO/2024-05-22/punto_de_corte_a.xlsx",sheet = "Productivo empresarial")
empresarial <- read_xlsx("C:/Users/PERSONAL/Documents/TRABAJO/2024-05-22/punto_de_corte_a.xlsx",sheet = "Productivo pymes")
#pymes <- pymes %>% select(1:12)#eliminar columna adicional
#corporativo <- corporativo %>% filter(!is.na(`Tipo de Crédito`))#eliminar filas vacias

#convertir columnas de texto a columnas numericas
# corporativo <- corporativo %>% mutate_at(vars(4:12),as.numeric)
# pymes <- pymes %>% mutate_at(vars(4:12),as.numeric)
# empresarial <- empresarial %>% mutate_at(vars(4:12),as.numeric)

#unificar todos los subsegmentos
PC <- bind_rows(corporativo,pymes,empresarial)
PC

names(PC)

#reordenar y reducir las columnas de los percentiles
PC_R <- PC %>% pivot_longer(!c("Subsegemento de Crédito",  "Actividad Económica", "Indicadores"),
                    names_to = "percentil", values_to = "valor")

unique(PC_R$`Actividad Económica`)
unique(PC_R$`Subsegemento de Crédito`)
unique(PC_R$Indicadores)

# #agregar etiquetas de calificación (obsoleto)
# names_cal <- tibble(percentil=c("Q9","Q8","Q7","Q6","Q5","Q4","Q3","Q2","Q1"),
#                     calificacion=c("A1","A2","A3","B1","B2","C1","C2","D","E"),
#                     cod_cal=c(1:9))
# 
# bases <- list(PC_R,names_cal)
# todo <- reduce(bases,merge,all=TRUE)
# todo <- todo %>% arrange(`Actividad Económica`,`Subsegemento de Crédito`,
#                          Indicadores,percentil,cod_cal,calificacion) %>% select("Actividad Económica","Subsegemento de Crédito",
#                         "Indicadores","percentil","cod_cal","calificacion","valor") %>% as_tibble()
# todo

#casos de endeudamiento
names_endeudamiento <- tibble(percentil=c("Q9","Q8","Q7","Q6","Q5","Q4","Q3","Q2","Q1"),
                      calificacion=c("E","D","C2","C1","B2","B1","A3","A2","A1"),
                      puntaje=c(31,41,51,61,71,78,85,92,100))
df_endeudamiento <- PC_R %>% filter(Indicadores%in%c("Endeudamiento_corto_plazo","Endeudamiento_de_Activo","Endeudamiento_patrimonial"))
df_endeudamiento <- df_endeudamiento %>% left_join(names_endeudamiento,by = "percentil") %>% as_tibble()


#casos de no endeudamiento
names_restante <- tibble(percentil=c("Q9","Q8","Q7","Q6","Q5","Q4","Q3","Q2","Q1"),
                    calificacion=c("A1","A2","A3","B1","B2","C1","C2","D","E"),
                    puntaje=c(100,92,85,78,71,61,51,41,31))
df_restante <- PC_R %>% filter(!Indicadores%in%c("Endeudamiento_corto_plazo","Endeudamiento_de_Activo","Endeudamiento_patrimonial"))
df_restante <- df_restante %>% left_join(names_restante,by = "percentil") %>% as_tibble()

#unir los dataframes
todo <- bind_rows(df_endeudamiento,df_restante) %>% arrange(percentil) %>% 
  select("Actividad Económica","Subsegemento de Crédito","Indicadores","percentil","calificacion","puntaje","valor")

#Nota importante: Debe estar ordenado la columna percentiles de menor a mayor ya que el codigo no funciona con percentiles desordenados


#ifelse(todo$Indicadores%in%c("Endeudamiento_corto_plazo","Endeudamiento_de_Activo","Endeudamiento_patrimonial"),switch(todo$calificacion))


#liquidez corriente
fil <- todo %>% filter(Indicadores=="Liquidez_Corriente")
summary(fil)#min mayor a 0.0001
res <- fil %>% group_by(`Actividad Económica`,`Subsegemento de Crédito`,Indicadores) %>%
  mutate(max=ifelse(percentil=="Q9",9999.9999,valor),min=ifelse(percentil=="Q1",0.0001,lag(max)+0.0001))
lc <- res

#prueba acida
fil <- todo %>% filter(Indicadores=="Prueba_Acida")
summary(fil)#min mayor a 0.0001
res <- fil %>% group_by(`Actividad Económica`,`Subsegemento de Crédito`,Indicadores) %>%
  mutate(max=ifelse(percentil=="Q9",9999.9999,valor),min=ifelse(percentil=="Q1",0.0001,lag(max)+0.0001))
pa <- res

#rentabilidad financiera
fil <- todo %>% filter(Indicadores=="Rentabilidad_Financiera")
summary(fil)#min mayor a 0.0001
res <- fil %>% group_by(`Actividad Económica`,`Subsegemento de Crédito`,Indicadores) %>%
  mutate(max=ifelse(percentil=="Q9",9999.9999,valor),min=ifelse(percentil=="Q1",-9999.9999,lag(max)+0.0001))
rf <- res

#utilidad operacional
fil <- todo %>% filter(Indicadores=="Utilidad_Operacional")
summary(fil)#min mayor a 0.0001
res <- fil %>% group_by(`Actividad Económica`,`Subsegemento de Crédito`,Indicadores) %>%
  mutate(max=ifelse(percentil=="Q9",9999.9999,valor),min=ifelse(percentil=="Q1",-9999.9999,lag(max)+0.0001))
#res %>% filter(`Actividad Económica`=="Otras Actividades De Servicios.",`Subsegemento de Crédito`=="Productivo corporativo")
uo <- res

#cobertura de intereses
fil <- todo %>% filter(Indicadores=="Cobertura_de_Intereses")
summary(fil)#min mayor a 0.0001
res <- fil %>% group_by(`Actividad Económica`,`Subsegemento de Crédito`,Indicadores) %>%
  mutate(max=ifelse(percentil=="Q9",9999.9999,valor),min=ifelse(percentil=="Q1",-9999.9999,lag(max)+0.0001))
#res %>% filter(`Actividad Económica`=="Otras Actividades De Servicios.",`Subsegemento de Crédito`=="Productivo corporativo")
ci <- res

#endeudamiento a corto plazo
fil <- todo %>% filter(Indicadores=="Endeudamiento_corto_plazo")
summary(fil)#min mayor a 0.0001
res <- fil %>% group_by(`Actividad Económica`,`Subsegemento de Crédito`,Indicadores) %>%
  mutate(max=ifelse(percentil=="Q9",1.0000,valor),min=ifelse(percentil=="Q1",0.0000,lag(max)+0.0001))
#res %>% filter(`Actividad Económica`=="Otras Actividades De Servicios.",`Subsegemento de Crédito`=="Productivo corporativo")
ecp <- res

#endeudamiento de activo
fil <- todo %>% filter(Indicadores=="Endeudamiento_de_Activo")
summary(fil)#min mayor a 0.0001
res <- fil %>% group_by(`Actividad Económica`,`Subsegemento de Crédito`,Indicadores) %>%
  mutate(max=ifelse(percentil=="Q9",9999.9999,valor),min=ifelse(percentil=="Q1",0.0000,lag(max)+0.0001))
#res %>% filter(`Actividad Económica`=="Otras Actividades De Servicios.",`Subsegemento de Crédito`=="Productivo corporativo")
ea <- res

#endeudamiento Endeudamiento patrimonial
fil <- todo %>% filter(Indicadores=="Endeudamiento_patrimonial")
summary(fil)#min mayor a 0.0001
res <- fil %>% group_by(`Actividad Económica`,`Subsegemento de Crédito`,Indicadores) %>%
  mutate(max=ifelse(percentil=="Q9",9999.9999,valor),min=ifelse(percentil=="Q1",0.0000,lag(max)+0.0001))
#res %>% filter(`Actividad Económica`=="Otras Actividades De Servicios.",`Subsegemento de Crédito`=="Productivo corporativo")
ep <- res

#
resultados <- bind_rows(lc,pa,rf,uo,ci,ecp,ea,ep)
resultados


library(writexl)
write_xlsx(resultados,path = "C:/Users/Franklin.Paucar/Downloads/Productivo_Puntos_Cortes_max_min_new.xlsx")

# write_xlsx(todo,path = "C:/Users/Franklin.Paucar/Downloads/todo.xlsx")
# write_xlsx(todo2,path = "C:/Users/Franklin.Paucar/Downloads/todo2.xlsx")



#endeudamiento Endeudamiento patrimonial VERSION 2
fil <- todo %>% filter(Indicadores=="Endeudamiento_patrimonial")
summary(fil)#min mayor a 0.0001
res <- fil %>% group_by(`Actividad Económica`,`Subsegemento de Crédito`,Indicadores) %>%
  mutate(max=ifelse(percentil=="Q8",9999.9999,valor),min=ifelse(percentil=="Q1",0.0000,lag(max)+0.0001))
res <- res %>% group_by(`Actividad Económica`,`Subsegemento de Crédito`,Indicadores) %>%
  mutate(max=ifelse(percentil=="Q9",-0.0001,max),min=ifelse(percentil=="Q9",-9999.9999,min))
res %>% filter(`Actividad Económica`=="Otras Actividades De Servicios.",`Subsegemento de Crédito`=="Productivo corporativo")
ep2 <- res

resultados2 <- bind_rows(lc,pa,rf,uo,ci,ecp,ea,ep2)
resultados2

write_xlsx(resultados2,path = "C:/Users/Franklin.Paucar/Downloads/Productivo_Puntos_Cortes_max_min_ver2.xlsx")


#1.0000

# #rentabilidad financiera (obsoleto)
# fil <- todo %>% filter(Indicadores=="Rentabilidad_Financiera")
# summary(fil)#min mayor a 0.0001
# res <- fil %>% group_by(`Actividad Económica`,`Subsegemento de Crédito`,Indicadores) %>%
#   mutate(max=ifelse(percentil=="Q9",9999.9999,lag(valor)),min=ifelse(percentil=="Q1",-9999.9999,ifelse(percentil=="Q2",0.000,ifelse(percentil=="Q3",0.0001,lag(max,n=2)+0.0001))))
# res %>% filter(`Actividad Económica`=="Otras Actividades De Servicios.",`Subsegemento de Crédito`=="Productivo corporativo")


resultados
