
# Ejemplo de datos
df_intervalos <- data.frame(
  maximos = c(100, 200, 300, 400, 500, 600, 700, 800, 900),
  minimos = c(0, 100, 200, 300, 400, 500, 600, 700, 800)
)

df_datos <- data.frame(
  entidades = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J",
                "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T"),
  monto = c(50, 150, 250, 350, 450, 550, 650, 750, 850, 950)
)

# Crear una función para etiquetar los intervalos
etiquetar_intervalo <- function(monto, maximos, minimos) {
  for (i in 1:length(maximos)) {
    if (monto >= minimos[i] && monto < maximos[i]) {
      min <- minimo[i]
      max <- maximo[i]
      perc <- percentil[i]
      
      return(paste(minimos[i], "-", maximos[i]))
    }
  }
  return(NA)  # Si no se encuentra en ningún intervalo
}
vars <- c()
# Aplicar la función a la columna "monto" y crear una nueva columna en df_datos

df_datos$intervalo <- mapply(etiquetar_intervalo, df_datos$monto, df_intervalos$maximos, df_intervalos$minimos)
total <- c(vars,)
# Ver el resultado
print(df_datos)





    etiquetar_intervalo <- function(monto, maximos, minimos,percentil,calificacion) {
      for (i in 1:length(maximos)) {
        if (monto >= minimos[i] && monto < maximos[i]) {
          respuesta <- data.frame(min=minimos[i],max=maximos[i],percentil=percentil[i],calificacion=calificacion[i])
          return(respuesta)
        }
      }
      return(NA)  # Si no se encuentra en ningún intervalo
    }
    
   etiquetar_intervalo(df_datos$monto, df_intervalos$maximos, df_intervalos$minimos,df_intervalos$percentil,df_intervalos$calificacion) 
  
   df_datos 

   
   
   # dataframe1
   dataframe1 <- data.frame(
     entidad = c("A", "B", "C", "D"),
     monto = c(150, 300, 450, 600)
   )
   
   # dataframe2
   dataframe2 <- data.frame(
     minimos = c(0, 200, 400),
     maximos = c(199, 399, 599)
   )
   
   # Necesitamos realizar la operación fila por fila en dataframe1
   result <- dataframe1 %>%
     rowwise() %>%
     mutate(
       intervalo = list(dataframe2 %>%
                          filter( maximos> monto & minimos <= monto) %>%
                          slice(1)),
       minimos = intervalo$minimos,
       maximos = intervalo$maximos
     ) %>%
     ungroup() %>%
     select(-intervalo)  # Eliminamos la columna temporal "intervalo"
   

   # Ejemplo de dataframes
   dataframe1 <- data.frame(entidad = c('A', 'B', 'C', 'D'),
                            monto = c(150, 250, 350, 450))
   
   dataframe2 <- data.frame(maximos = c(100, 200, 300, 400),
                            minimos = c(0, 100, 200, 300))
   
   # Función para encontrar el intervalo
   find_interval <- function(monto, dataframe2) {
     row_index <- which(monto >= dataframe2$minimos & monto < dataframe2$maximos)
     if (length(row_index) == 0) {
       return(c(NA, NA)) # Si no se encuentra un intervalo, devolver NA
     } else {
       return(c(dataframe2$maximos[row_index], dataframe2$minimos[row_index]))
     }
   }
   
   # Aplicar la función y añadir columnas al dataframe1
   intervals <- t(apply(dataframe1, 1, function(row) find_interval(row['monto'], dataframe2)))
   
   dataframe1$maximos <- intervals[, 1]
   dataframe1$minimos <- intervals[, 2]
   
   # Ver el resultado
   print(dataframe1)

   
   
   
   
   library(dplyr)
   
   # Ejemplo de dataframes
   dataframe1 <- data.frame(entidad = c('A', 'B', 'C', 'D'),
                            monto = c(150, 250, 350, 450))
   
   dataframe2 <- data.frame(maximos = c(100, 200, 300, 400),
                            minimos = c(0, 100, 200, 300))
   
   
   # Inicializa las nuevas columnas
   base_total_pivot$min <- NA
   base_total_pivot$max <- NA
   
   # Itera sobre cada fila de dataframe1
   for (i in 1:nrow(base_total_pivot)) {
     # Itera sobre cada fila de dataframe2
     for (j in 1:nrow(puntos_corte)) {
       # Verifica si el monto está dentro del intervalo
       if (base_total_pivot$valor_indicador[i] >= puntos_corte$min[j] && base_total_pivot$valor_indicador[i] <= puntos_corte$max[j]) {
         # Asigna los valores de minimos y maximos correspondientes
         base_total_pivot$min[i] <- puntos_corte$min[j]
         base_total_pivot$max[i] <- puntos_corte$max[j]
         base_total_pivot$percentil[i] <- puntos_corte$percentil[j]
         base_total_pivot$calificacion[i] <- puntos_corte$calificacion[j]
         break  # Sal de la iteración de dataframe2 una vez encontrado el intervalo
       }
     }
   }
   0,27198>=
   class(puntos_corte$min)
   class(puntos_corte$min)
   # Muestra el resultado
   print(dataframe1)
   
   gg <- base_total_pivot %>% ungroup() %>% 
     inner_join(pc_sel %>% ungroup(), by = c("Tipo_credito" = "Tipo_credito",
                                             "Actividad Económica"="Actividad Económica",
                                             "Indicadores" = "Indicadores"))
   # Usar dplyr para encontrar los intervalos correctos y unir los dataframes
   base_total_pivot %>% filter(#Tipo_credito =="Productivo corporativo",
                               Indicadores == "Endeudamiento_Corto_Plazo"
                               Indicadores == "Endeudamiento_corto_plazo",
                               #`Actividad Económica`=="Actividades De Alojamiento Y De Servicio De Comidas."
                               )
   view(as)
   unique(base_total_pivot$Tipo_credito)
   base_total_pivot <- base_total_pivot %>% filter("Tipo_credito" =="Productivo corporativo",
                                                   #Indicadores=="Endeudamiento_corto_plazo",
                                                   "Actividad Económica"=="Actividades De Alojamiento Y De Servicio De Comidas.")
 
   
   ########aplicar puntajes intervalos
    result <- base_total_pivot %>% ungroup() %>% 
     inner_join(pc_sel %>% ungroup(), by = c("Tipo_credito" = "Tipo_credito",
                                     "Actividad Económica"="Actividad Económica",
                                     "Indicadores" = "Indicadores")) %>%
     filter(valor_indicador >= min & valor_indicador < max) 
   write_xlsx(result,path = "C:/Users/PERSONAL/Documents/TRABAJO/test_join_gg.xlsx")
   
   
   # Si quieres mantener todas las filas originales de dataframe1 y añadir NA donde no hay coincidencia
   result2 <- base_total_pivot %>%
     left_join(result,by = c("IDENTIFICACION"="IDENTIFICACION",
                             "NOMBRE"="NOMBRE",
                             "Actividad Económica"="Actividad Económica",
                             "Tipo_credito" = "Tipo_credito",
                             "Indicadores" = "Indicadores",
                             "valor_indicador"="valor_indicador"))
   
   # Ver el resultado
   print(result2)
   write_xlsx(result2,path = "C:/Users/PERSONAL/Documents/TRABAJO/test_calificacion.xlsx")
   
   write_xlsx(gg,path = "C:/Users/PERSONAL/Documents/TRABAJO/test_join.xlsx")
   