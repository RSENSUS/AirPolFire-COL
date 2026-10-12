##### CODIGO PARA INTERSECAR RETROTRAYECTORIAS MODELADAS A 1000 M CON LOS DATOS
##### DE PUNTOS CALIENTES PARA CADA SEGMENTO DIARIO DE LOS 5 DIAS DE LA RETROTRAYECTORIA

#Cargar paquetes
{
  library(sf)
  library(lubridate)
  library(ggplot2)
  library(dplyr)
}

     
############################ Cargar datos ########################################
#Ruta
wd <- '/data/home/TU/tur75984/CODI_Intersecciones'

path_<-'/data/home/TU/tur75984/CODI_Intersecciones/Resultados'

#Retrotrayectorias
pathbackward<-'/data/home/TU/tur75984/CODI_Intersecciones/Traj1000m'

#Cargar a una lista los nombres de los archivos
filenames <- list.files(path=pathbackward,pattern="*.txt", full.names=TRUE)

#Cargar todos los archivos usando read.csv. Backward trajectory text files
ldf <- lapply(filenames,read.table,header=T) 

#Cambiar nombre de cada objeto de la lista (cada archivo)
#str_count(filenames[1]) #Ver numero de caracteres en filenames
names(ldf)<-substr(filenames, 110,113)

#Seleccion de puntos calientes de hasta 5 dias antes desde el momento de analisis

#Checkpoint 
print("Cargando incendios ...")

#2018-2020
p.calientes<-read.csv(paste0(wd,'/fire_archive_SV-C2_292666.csv'),header=T)

#2021-2024
p.calientes<-read.csv(paste0(wd,'/fire_archive_SV-C2_587791_20212024.csv'),header=T)

#Formato fechas
#p.calientes$acq_date<-as.Date(p.calientes$acq_date, format="%Y-%m-%d")

###FILTRAR INCENDIOS DE CONFIDENCE NOMINAL Y HIGH

#Descartar incendios con confidence low
p.calientes$confidence<-as.factor(p.calientes$confidence)
#levels(p.calientes$confidence)

# Subset p.calientes to keep only rows where confidence is "n" or "h"
p.calientes <- p.calientes[p.calientes$confidence %in% c("n", "h"), ]
# Drop the unused levels from the confidence column
p.calientes$confidence <- droplevels(p.calientes$confidence)
#levels(p.calientes$confidence)

####################Georreferenciacion y preproesamiento##########################

#Base de datos vacia
BD_final <- data.frame()

#Corrida interrumpida
#ldf<-ldf[39:length(ldf)]

#Seleccionar primer archivo de analisis con el que empezara a correr el ciclo
#Cambiar para cada year
year <- 7 # 1:7
i<-ldf[[year]]

print(paste0("Starting year:",year))

#Convertir a factor el codigo para cada dia de analisis
i$Flag<-as.factor(i$Flag)
#levels(i$Flag)#Ver cuantos codigos hay (deben haber 365 por cada dia del año - 366 para 2020/2024)

#Formato fechas
#2018 - 2020 (1:3)
#i$Fecha<-as.Date(i$Fecha, format="%d/%m/%Y")

#2021-2024 (4:7)
i$Fecha<-as.Date(i$Fecha, format="%Y-%m-%d")

#w <- levels(i$Flag)[1]

for(w in levels(i$Flag)){#w representa cada retrotrayectoria diaria
  
  #Seleccionar una retrotrayectoria
  datai<-subset(i,i$Flag==w)
  
  #Usar solo la fecha sin hora
  datai$Fecha <- as.Date(datai$Fecha) 
  
  #Checkpoint
  print(paste("Vamos en la fecha----",as.Date(datai[1,1], format="%Y-%m-%d"),"----- YUJU!!!!!!"))
  
  #Crear vector con los dias de la retrotrayectoria
  dias <- unique(datai$Fecha)
  
  # BD para agregar incendios intersecados por trayectoria
  BD_inc_total <- data.frame()  
  
  #d <- dias[1]
  
  #Loop para hacer la interseccion para cada dia de la retrayectoria (para cada segmento)
  for(d in dias){
    
    d <- as.Date(d)  # DEfinir d como fecha
    
    #Filtrar retrotrayectoria para dia d
    datad <- subset(datai, Fecha == d)
    
    # Saltar si hay menos de dos puntos ese día
    if(nrow(datad) < 2) next  
    
    #crear objeto espacial
    puntos_geo <- st_as_sf(datad, coords = c("Longitud", "Latitud"), crs = 4326)
    #Reproyectar puntos de WGS84 a UTM 18N
    puntos_geo <- st_transform(puntos_geo, 32618)
    
    #Agregar geometria de punto al objeto
    puntos_geom <- st_geometry(puntos_geo)
    
    # Numero total de lineas a crear entre punto y punto
    n <- length(puntos_geom)-1
    
    # Crear líneas entre puntos consecutivos
    linestrings <- lapply(1:n, function(x) {
      st_cast(st_combine(c(puntos_geom[x], puntos_geom[x + 1])), "LINESTRING")
    })
    
    #Unir todas las lineas a una sola polilinea    
    tray_dia <- st_multilinestring(do.call("rbind", linestrings))
    
    #Convertir a geometria polilinea
    tray_dia <- st_cast(st_geometry(tray_dia), "LINESTRING")
    
    #Agregar sistema de coordenadas proyectadas
    st_crs(tray_dia) <- 32618
    
    #Generacion de buffer de 150 km alrededor de la trayectoria (la funcion esta en metros)    
    tray_buff <- st_buffer(st_union(tray_dia), 150000)
    
    # Filtrar incendios del mismo dia
    fires_d <- subset(p.calientes, as.Date(p.calientes$acq_date) == d)
    
    ################################################################################
    
    #Comprobar presencia de incendios dentro en la misma fecha del segmento de la retrotrayectoria
    if(nrow(fires_d) == 0){
      message(paste("No hubo incendios para la fecha:", d))
      BD_final <- rbind(BD_final, c(as.character(datai[1,1]),as.character(d),0, 0))
      next
    }
    
    #Convertir base de datos de puntos calientes a puntos (UTM)
    fires_sf <- st_as_sf(fires_d, coords = c("longitude", "latitude"), crs = 4326)
    fires_sf <- st_transform(fires_sf, 32618) #Proyectar puntos WGS84 a UTM
    fires_geom <- st_geometry(fires_sf) #Agregar geometria de punto al objeto
    
    #Interseccion de puntos calientes seleccionados con buffers
    inc_tray <- st_intersection(fires_geom, tray_buff)
    
    #Extraer informacion de puntos calientes intersecados (n y FRP: Fire Radiative Power MW)
    BD_inc <- c()
    
    tryCatch({
      for (f in 1:length(inc_tray)) {
        a <- inc_tray[[f]]
        b <- st_within(a, fires_sf)#Buscar punto en comun
        c <- fires_sf[b[[1]],]#Fila con ubicacion del punto en comun
        BD_inc <- rbind(BD_inc, c)#Funcion de interseccion 
        
        # Acumula cada incendio individual en la base total por trayectoria
        BD_inc_total <- rbind(BD_inc_total, c)  
      }
    }, error = function(e){#Cuando haya errores mostrar el siguiente mensaje
      message(paste("No Incendios - Error en intersección para la fecha", d, "de la retrotrayectoria para", datai[1,1]))
    })
  
    #Para agregar a la BD final
    
    #Si la interseccion es nula, agregar datos vacios a la base de datos
    if (!is.null(BD_inc) && nrow(BD_inc) > 0) {
      #Si hay interseccion de incendios
      BD_final <- rbind(BD_final, c(as.character(datai[1,1]), as.character(d), nrow(BD_inc), sum(BD_inc$frp)))
    } else { #Si no hay interseccion de incendios
      BD_final <- rbind(BD_final, c(as.character(datai[1,1]), as.character(d), 0, 0))
    }
  }
  
  ###################GRafico de control #################################

  # Crear la linea de la retrotrayectoria completa
  pointsgeo2<- st_as_sf(datai, coords = c("Longitud", "Latitud"), crs = 4326)

  #Agregar geometria de punto al objeto
  puntos2<- st_cast(st_geometry(pointsgeo2), "POINT")

  #Proyectar puntos WGS84 a UTM
  puntos2<-st_transform(puntos2, 32618)

  # Numero total de lineas a crear entre punto y punto
  n_total <- length(puntos2) - 1

  # Funcion para construir lineas entre punto y punto
  linestrings <- lapply(X = 1:n_total, FUN = function(x) {

    pair <- st_combine(c(puntos2[x], puntos2[x + 1]))
    line <- st_cast(pair, "LINESTRING")
    return(line)

  })

  #Unir todas las lineas a una sola polilinea
  tray3 <- st_multilinestring(do.call("rbind", linestrings))

  #Convertir a geometria polilinea
  tray3<- st_cast(st_geometry(tray3), "LINESTRING")
  #Agregar sistema de coordenadas proyectadas
  st_crs(tray3)<-32618

  #Generacion de buffer de 150 km alrededor de la trayectoria (la funcion esta en metros)
  tray_complete<-st_union(tray3)
  tray_buff_complete<-st_buffer(tray_complete, 150000)

  #Incedios para todo el rango de 5 dias de la retrotrayectoria
  fires_total <- subset(p.calientes, as.Date(acq_date) >= as.Date(i[1,1],format="%Y-%m-%d")-days(5)
                        & as.Date(acq_date) <= as.Date(i[1,1],format="%Y-%m-%d"))
  #Convertir base de datos de puntos calientes a puntos (UTM)
  fires_total<- st_as_sf(fires_total, coords = c("longitude", "latitude"), crs = 4326)
  fires_total<-st_transform(fires_total, 32618)#Proyectar puntos WGS84 a UTM
  fires_total<- st_cast(st_geometry(fires_total), "POINT")#Agregar geometria de punto al objeto


  # Crear imagen con retrotrayectoria completa e incendios intersectados
  jpeg(file = paste0(path_, "/traj", as.Date(datai[1,1],format="%Y-%m-%d"), ".jpeg"),
       height = 40, width = 80, units = "cm", res = 150, pointsize = 35)

  plot(tray_buff_complete, col = "white", lwd = 2, border = 'seagreen', lty = 3,
       main = paste(datai[1,1]), axes = TRUE)#Buffer
  plot(puntos2, pch = 20, cex = 0.8, col = "darkolivegreen4", add = TRUE) #Puntos Retrotrayectoria
  plot(tray_complete, col = "darkolivegreen4", lwd = 2, add = TRUE)#Linea retrotrayectoria
  plot(fires_total,col="lightsalmon",add = T) #Todos los incendios para el rango de 5 dias

  # Asegurarse de que BD_inc_total exista y tenga geometría
  if (exists("BD_inc_total") && nrow(BD_inc_total) > 0) {
    fires_intersec <- st_as_sf(BD_inc_total, coords = c("longitude", "latitude"), crs = 4326)
    fires_intersec <- st_transform(fires_intersec, 32618)
    plot(fires_intersec, col = "brown1", pch = 19, add = TRUE)#Incendios intersecados
    
    #Exportar los daatos de incendios intersecados
    write.csv(BD_inc_total,paste0(path_,"/", as.Date(datai[1,1],format="%Y-%m-%d"),".csv"))
  }

  dev.off()
}

names(BD_final)<-c("Fecha","Dia_Retrotraj","Incendios","FRP_T")

#Transformar a numero
BD_final$Incendios<-as.numeric(BD_final$Incendios)
BD_final$FRP_T<-as.numeric(BD_final$FRP_T)
BD_final$Fecha <- as.Date(BD_final$Fecha,format = "%Y-%m-%d")

#Agregar informacion de incendios para cada dia y obtener serie de incendios y FRP diaria
BD_final_sum <- BD_final %>%
  group_by(Fecha) %>%
  summarise(
    Incendios = sum(as.numeric(Incendios), na.rm = TRUE),
    FRP_T = sum(as.numeric(FRP_T), na.rm = TRUE)
  )
#Calcular FRP promedio
BD_final_sum$FRP<-BD_final_sum$FRP_T/BD_final_sum$Incendios


#Exportar
write.csv(BD_final,paste0(wd,"/Series/SegmentoRetrotraj_",year,"_v3.csv"))
write.csv(BD_final_sum,paste0(wd,"/Series/ParametrosIncendios_",year,"_v3.csv"))

print(paste0('Intersecciones listas para ',year))
