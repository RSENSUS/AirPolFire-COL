##### CODE FOR PROCESSING SPATIAL INFORMATION FROM BACK-TRAJECTORIES, GENERATING BUFFERS,
##### AND INTERSECTING THEM WITH HOTSPOT DATA FROM THE PREVIOUS 5 DAYS
#https://bookdown.org/robinlovelace/geocompr/reproj-geo-data.html

#Load packages
{
  library(sf)
  library(lubridate)
  library(ggplot2)
  library(xlsx)
}


############################ Cargar datos ########################################
#Paths
wd <- 'path_to_working_directory'
path_<-"path_to_results"
pathbackward<-'path_to_back-trajectories'

#Load the file names into a list
filenames <- list.files(path=pathbackward,pattern="*.txt", full.names=TRUE)

#Load all files using read.csv. Backward trajectory text files
ldf <- lapply(filenames,read.csv) 

#Re-name objects in the list: Extract only the date portion and clean suffixes
names(ldf) <- gsub(".*\\/|(_|-| )?(600|6AM)?\\.txt$", "", filenames) # Remove everything before the last `/` and suffixes
names(ldf) <- gsub("(\\d{2})(\\d{2})(\\d{4})", "\\1-\\2-\\3", names(ldf)) # Format compact ddmmyyyy into dd-mm-yyyy

#Seleccion de puntos calientes de hasta 5 dias antes desde el momento de analisis
p.calientes<-read.csv("Puntos_Calientes_VIIRS_09-02-2020_15-05-2020.csv",header=T)

#Formato fechas
p.calientes$Fecha<-as.POSIXct(p.calientes$Fecha, format="%Y-%m-%d %H:%M")
p.calientes$Fecha<-as.Date(p.calientes$Fecha, format="%Y-%m-%d")

###FILTRAR INCENDIOS DE CONFIDENCE NOMINAL Y HIGH
#Descartar incendios con confidence low
p.calientes$confidence<-as.factor(p.calientes$confidence)
levels(p.calientes$confidence)

# Subset p.calientes to keep only rows where confidence is "n" or "h"
p.calientes <- p.calientes[p.calientes$confidence %in% c("n", "h"), ]
# Drop the unused levels from the confidence column
p.calientes$confidence <- droplevels(p.calientes$confidence)
levels(p.calientes$confidence)

####################Georreferenciacion y preproesamiento##########################
#Filtro por altura
altura<-2000

#Base de datos vacia
BD_final<-c()
BD_final<-as.data.frame(BD_final)

#Corrida interrumpida
#ldf<-ldf[39:length(ldf)]

contador<-0 #Para saber la base de datos que esta ejecutando

# for(i in ldf){
#   #i<-ldf[[1]] #Check
#   contador<-contador+1
#   datai<-subset(i,i$Tray==altura)
#   print(c(contador,datai[1,1]))
# 
# }

for(i in ldf){
  #i<-ldf[[40]] #Check
  contador<-contador+1
  print(c(contador,i[1,1]))
  
  #Crear objeto espacial a partir de dataframe con lat/long values
  datai<-subset(i,i$Tray==altura)
  pointsgeo<- st_as_sf(datai, coords = c("Longitud", "Latitud"), crs = 4326) 
  #Verificar sistema de coordenadas
  #st_is_longlat(pointsgeo)
  #st_crs(pointsgeo)
  
  #Agregar geometria de punto al objeto
  puntos<- st_cast(st_geometry(pointsgeo), "POINT")
  
  #Proyectar puntos WGS84 a UTM
  puntos<-st_transform(puntos, 32618)
  #st_crs(puntos)#Verificar sistema de coordenadas
  #st_is_longlat(puntos)
  
  # Numero total de lineas a crear entre punto y punto
  n <- length(puntos) - 1
  
  # Funcion para construir lineas entre punto y punto
  linestrings <- lapply(X = 1:n, FUN = function(x) {
    
    pair <- st_combine(c(puntos[x], puntos[x + 1]))
    line <- st_cast(pair, "LINESTRING")
    return(line)
    
  })
  
  #Unir todas las lineas a una sola polilinea
  tray <- st_multilinestring(do.call("rbind", linestrings))
  
  #Convertir a geometria polilinea
  tray<- st_cast(st_geometry(tray), "LINESTRING")
  #Agregar sistema de coordenadas proyectadas
  st_crs(tray)<-32618
  
  #Generacion de buffer de 150 km alrededor de la trayectoria (la funcion esta en metros)
  tray2<-st_union(tray)
  tray_buff<-st_buffer(tray2, 150000)
  
  ###################################
   
  #Fecha inicial para extraer datos de puntos calientes (5 dias previos a la retrotrayectoria)
  final<-as.Date(datai[1,1],format="%Y-%m-%d") #Fecha retrotrayectoria
  
  # Filtrar datos de incednio con rango de fechas de retrotrayectoria
  fires_indices <- which(p.calientes$Fecha >= final - days(5) & p.calientes$Fecha <= final)
  
  #Comprobar presencia de incendios dentro del area de influencia de la retrotrayectoria
  if (length(fires_indices) == 0) {
    message(paste("No hubo incendios intersecados con la retrotrayectoria estimada para la fecha:",
                  final))
    BD_final <- rbind(BD_final, c(as.character(final), 0, 0))
    next
  }
  
  #Puntos calientes para el periodo asociado a la retrotrayectoria
  incendios_periodo <- p.calientes[fires_indices, ]
  
  #Convertir base de datos de puntos calientes a puntos (UTM)
  fires<- st_as_sf(incendios_periodo, coords = c("longitude", "latitude"), crs = 4326) 
  fires<-st_transform(fires, 32618)#Proyectar puntos WGS84 a UTM
  fires2<- st_cast(st_geometry(fires), "POINT")#Agregar geometria de punto al objeto
  #st_crs(fires2)#Verificar sistema de coordenadas
  
  #Interseccion de puntos calientes seleccionados con buffers
  inc_tray<-st_intersection(fires2, tray_buff)
  
  #Extraer informacion de puntos calientes intersecados (n y FRP: Fire Radiative Power MW)
  BD_inc<-c()
  
  tryCatch (
    for (f in 1:length(inc_tray)) {
      a<-inc_tray[[f]]
      b<-st_within(a,fires) #Buscar punto en comun
      c<-fires[b[[1]],]#Fila con ubicacion del punto en comun
      BD_inc<-rbind(BD_inc,c)}, #Funcion de interseccion 
    error = function(e){ #Cuando haya errores mostrar el siguiente mensaje
      message(paste("No hubo incendios intersecados con la retrotrayectoria
                          estimada para la fecha", final,":\n"))})
  write.csv2(BD_inc,paste(path_,"/",final,"_",altura,'.csv', sep = ''))
  
  #Para agregar a la BD final
  BD_final<-rbind(BD_final,c(as.character(final),nrow(BD_inc),sum(BD_inc$frp)))
  
  #PLOT
  jpeg(file = paste(path_,"/",final,"_",altura,'.jpeg', sep = ''),height=40,width=80,
       units = "cm",res=150,pointsize=35)
  plot(tray_buff, col = "white", lwd = 2,border = 'seagreen',lty=3,
       main=paste(final,altura),axes=T)
  plot(puntos, pch = 20, cex = 0.8,col="darkolivegreen4",add=T)
  plot(tray2, col = "darkolivegreen4", lwd =2, add = TRUE)
  plot(fires2,add = T,col="lightsalmon")
  plot(inc_tray,col="brown1",add=T,pch=19)
  dev.off()
}


names(BD_final)<-c("Fecha","Incendios","FRP_T")

#Transformar a numero
BD_final$Incendios<-as.numeric(BD_final$Incendios)
BD_final$FRP_T<-as.numeric(BD_final$FRP_T)

#Calcular FRP promedio
BD_final$FRP<-BD_final$FRP_T/BD_final$Incendios


write.csv(BD_final,"PuntosFRP_2000_v1.csv")

#Poner informacion en la base de datos de calibracion

#PRevious code
# for(i in ldf){
#   #i<-ldf[[1]] #Check
#   contador<-contador+1
#   print(c(contador,i[1,1]))
#   
#   #Crear objeto espacial a partir de dataframe con lat/long values
#   datai<-subset(i,i$Tray==altura)
#   pointsgeo<- st_as_sf(datai, coords = c("Longitud", "Latitud"), crs = 4326) 
#   #Verificar sistema de coordenadas
#   #st_is_longlat(pointsgeo)
#   #st_crs(pointsgeo)
#   
#   #Agregar geometria de punto al objeto
#   puntos<- st_cast(st_geometry(pointsgeo), "POINT")
#   
#   #Proyectar puntos WGS84 a UTM
#   puntos<-st_transform(puntos, 32618)
#   #st_crs(puntos)#Verificar sistema de coordenadas
#   #st_is_longlat(puntos)
#   
#   # Numero total de lineas a crear entre punto y punto
#   n <- length(puntos) - 1
#   
#   # Funcion para construir lineas entre punto y punto
#   linestrings <- lapply(X = 1:n, FUN = function(x) {
#     
#     pair <- st_combine(c(puntos[x], puntos[x + 1]))
#     line <- st_cast(pair, "LINESTRING")
#     return(line)
#     
#   })
#   
#   #Unir todas las lineas a una sola polilinea
#   tray <- st_multilinestring(do.call("rbind", linestrings))
#   
#   #Convertir a geometria polilinea
#   tray<- st_cast(st_geometry(tray), "LINESTRING")
#   #Agregar sistema de coordenadas proyectadas
#   st_crs(tray)<-32618
#   
#   #Generacion de buffer de 150 km alrededor de la trayectoria (la funcion esta en metros)
#   tray2<-st_union(tray)
#   tray_buff<-st_buffer(tray2, 150000)
#   
#   ###################################
#   
#   #Fecha inicial para extraer datos de puntos calientes (5 dias previos a la retrotrayectoria)
#   final<-as.Date(datai[1,1],format="%Y-%m-%d") #Fecha retrotrayectoria
#   
#   #Puntos calientes para el periodo asociado a la retrotrayectoria
#   incendios_periodo<-p.calientes[which(p.calientes$Fecha==final-days(5),arr.ind = T):which(p.calientes$Fecha==final,arr.ind = T),]
#   
#   #Convertir base de datos de puntos calientes a puntos (UTM)
#   fires<- st_as_sf(incendios_periodo, coords = c("longitude", "latitude"), crs = 4326) 
#   fires<-st_transform(fires, 32618)#Proyectar puntos WGS84 a UTM
#   fires2<- st_cast(st_geometry(fires), "POINT")#Agregar geometria de punto al objeto
#   #st_crs(fires2)#Verificar sistema de coordenadas
#   
#   #Interseccion de puntos calientes seleccionados con buffers
#   inc_tray<-st_intersection(fires2, tray_buff)
#   
#   #Extraer informacion de puntos calientes intersecados (n y FRP: Fire Radiative Power MW)
#   BD_inc<-c()
#   
#   tryCatch (
#     for (f in 1:length(inc_tray)) {
#       a<-inc_tray[[f]]
#       b<-st_within(a,fires) #Buscar punto en comun
#       c<-fires[b[[1]],]#Fila con ubicacion del punto en comun
#       BD_inc<-rbind(BD_inc,c)}, #Funcion de interseccion 
#     error = function(e){ #Cuando haya errores mostrar el siguiente mensaje
#       message(paste("No hubo incendios intersecados con la retrotrayectoria
#                           estimada para la fecha", final,":\n"))})
#   write.csv2(BD_inc,paste(path_,"/",final,"_",altura,'.csv', sep = ''))
#   
#   #Para agregar a la BD final
#   BD_final<-rbind(BD_final,c(as.character(final),nrow(BD_inc),sum(BD_inc$frp)))
#   
#   #PLOT
#   jpeg(file = paste(path_,"/",final,"_",altura,'.jpeg', sep = ''),height=40,width=80,
#        units = "cm",res=150,pointsize=35)
#   plot(tray_buff, col = "white", lwd = 2,border = 'seagreen',lty=3,
#        main=paste(final,altura),axes=T)
#   plot(puntos, pch = 20, cex = 0.8,col="darkolivegreen4",add=T)
#   plot(tray2, col = "darkolivegreen4", lwd =2, add = TRUE)
#   plot(fires2,add = T,col="lightsalmon")
#   plot(inc_tray,col="brown1",add=T,pch=19)
#   dev.off()
# }