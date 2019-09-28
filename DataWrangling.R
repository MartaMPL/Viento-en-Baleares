#**********************************************************************************
#AUTHOR				  :	Marta Padin
#CREATION DATE	: Feb 2019 - Sep 2019
#**********************************************************************************
#DESCRIPTION:   This file is a draft where you can find how to make data wrangling. 
#               
#
#**********************************************************************************
#To use this file we need an excel containing data from the stations of 
#Palma (Mallorca), Es Codolar (Ibiza) and Mahón (Menorca).
#6 observation to each day.
#In addition, 8 daily observations are needed for the Mallorca and Menorca stations.
#**********************************************************************************
#Data source: Iowa Environmental Mesonet.
#Observations collected at airports.
#ASOS is designed to support weather forecasting activities and aviation operations 
#and, at the same time, support the needs of meteorological, hydrological and 
#climatological research communities.


#Necessary libraries:
library(tidyverse)
library(lubridate)
library(tseries)
library(readr)
library(xlsx)
library(readxl)
library(ggplot2)
library(RColorBrewer)
library(fmsb)
library(gridExtra)
library(GGally)

#Data reading
WindData="WindDataInitial.xlsx"
Data <- read_xlsx(WindData)
#We change the names to the variables
Data %>%
  rename(date=`valid`, temperature =`tmpc`, direction = `drct`, 
         sknots = `sknt`, smiles = `sped`, psealevel = `mslp`, hour =`time`) -> Data1

#Create variables
Data1$Date   <- as.Date(Data$fecha)
Data1$anho   <- as.numeric(format(Data1$fecha, "%Y"))
Data1$mes    <- months(Data1$fecha)
Data1$dia    <- as.numeric(format(Data1$fecha, "%d"))
Data1$tiempo <- format(as.POSIXct(strptime(Data1$hora,"%Y-%m-%d %H:%M",tz="")), 
                       format = "%H:%M")

#To facilitate future work we separate the observations by weather station. In addition, 
#we only want the observations of the month of March and August.
Data1 %>% filter(estacion == "LEPA") %>% 
  filter(mes == "marzo" | mes ==  "agosto") %>% 
  filter(anho>2016,anho<2019)-> auxLEPA
Data1 %>% filter(estacion == "LEMH") %>% 
  filter(mes == "marzo" | mes == "agosto") %>% 
  filter(anho>2016,anho<2019)-> auxLEMH
Data1 %>% filter(estacion == "LEIB") %>% 
  filter(mes == "marzo" | mes ==  "agosto") %>%  
  filter(anho>2016,anho<2019)-> auxLEIB

#Because the observations are collected every $ 30 $ minutes and, on occasion, in random 
#moments, we are left with an observation every hour, that is, $ 24 per day. This will 
#be possible for the Mallorca and Menorca stations, however, due to the quality of the 
#data during the early morning, at the Ibiza station we will be obliged to perform a different
#data processing.

obsdejadas =c("00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00",
              "10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00",
              "20:00","21:00","22:00","23:00")
auxLEPA24obs <- filter(auxLEPA, tiempo %in% obsdejadas)
auxLEMH24obs <- filter(auxLEMH, tiempo %in% obsdejadas)

#Complete Mallorca, there are missing observations.
addrowsPA <- data.frame(fecha=as.POSIXct(c("2017-03-10","2017-03-13","2017-03-19",
                                           "2017-03-24","2017-03-26","2017-08-15",
                                           "2017-08-17","2018-03-9", "2018-03-17",
                                           "2018-03-25","2018-03-26","2018-03-26",
                                           "2018-03-29","2018-08-7")),
                        Date=as.Date(c("2017-03-10","2017-03-13","2017-03-19",
                                       "2017-03-24", "2017-03-26","2017-08-15",
                                       "2017-08-17","2018-03-9","2018-03-17",
                                       "2018-03-25","2018-03-26","2018-03-26",
                                       "2018-03-29","2018-08-7")),
                        v_nudos=c(1,9,1,12,3,4,9,1,17,8,8,8,1,3), 
                        tiempo = c("04:00","22:00","03:00","19:00","02:00",
                                   "22:00","09:00", "03:00","11:00","02:00",
                                   "08:00","09:00","01:00","22:00"),
                        dia=c(10,13,19,24,26,15,17,9,17,25,26,26,29,07),
                        anho=c(2017,2017,2017,2017,2017,2017,2017,2018,2018,2018,
                               2018,2018,2018,2018),
                        mes=c("marzo","marzo","marzo","marzo","marzo","agosto",
                              "agosto","marzo","marzo", "marzo", "marzo","marzo",
                              "marzo","agosto"),
                        direccion=c(0,60,0,300,50,70,50,0,250,340,110,120,0,90),
                        temperatura=c(8,13,4,9,7,26,31,7,15,11,11,15,8,27))

addrowsPA %>% mutate(v_nudos=as.character(v_nudos), direccion=as.character(direccion),
                     temperatura=as.character(temperatura))->addrowsPA

#2976 observaciones.
addrowsPA  <- full_join(addrowsPA,auxLEPA24obs)
addrowsPA  <-addrowsPA %>%  mutate(temperatura=as.numeric(temperatura), 
                                   direccion=as.numeric(direccion), v_nudos=as.numeric(v_nudos))
addrowsPA2 <-addrowsPA %>% filter(direccion>=0 , direccion <=360)

#In Menorca
addrowsMH <- data.frame(fecha=as.POSIXct(c("2017-03-7","2017-03-26",
                                           "2017-08-31", "2018-03-24",
                                           "2018-03-25","2018-03-26",
                                           "2018-03-26","2018-08-2",
                                           "2018-08-3")),
                        Date=as.Date(c("2017-03-7","2017-03-26","2017-08-31",
                                       "2018-03-24","2018-03-25","2018-03-26",
                                       "2018-03-26","2018-08-2","2018-08-3")),
                        v_nudos=c(7,7,12,10,7,6,12,7,2), 
                        tiempo = c("04:00","02:00","13:00","09:00","02:00",
                                   "08:00","09:00","06:00","00:00"),
                        dia=c(7,26,31,24,25,26,26,2,3),
                        temperatura=c(10,11,29,12,8,12,14,26,25),
                        direccion=c(360,180,70,220,90,300,290,20,350),
                        anho=c(2017,2017,2017,2018,2018,2018,2018,2018,2018),
                        mes=c("marzo","marzo","agosto","marzo","marzo","marzo",
                              "marzo","agosto","agosto"))

addrowsMH %>% mutate(v_nudos=as.character(v_nudos), direccion=as.character(direccion),
                     temperatura=as.character(temperatura))->addrowsMH


addrowsMH <- full_join(addrowsMH,auxLEMH24obs)
addrowsMH <- addrowsMH %>% mutate(temperatura=as.numeric(temperatura), 
                                  direccion=as.numeric(direccion), v_nudos=as.numeric(v_nudos))
addrowsMH2 <-addrowsMH %>% filter(direccion>=0 , direccion <=360)

#In ibiza
obsLEIB =c("00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00",
           "08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00",
           "16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00",
           "00:30","01:30","02:30","03:30","04:30","05:30","06:30","07:30",
           "08:30","09:30","10:30","11:30","12:30","13:30","14:30","15:30",
           "16:30","17:30","18:30","19:30","20:30","21:30","22:30","23:30")
grupohorario<-rep(0,5474)
#we create the groups according to a group every 6h.
grupo1 =c("00:00","00:30","01:00", "01:30","02:00","02:30","03:00","03:30",
          "04:00","04:30", "05:00","05:30")
grupo2 =c("06:00","06:30","07:00","07:30", "08:00","08:30","09:00","09:30", 
          "10:00","10:30", "11:00","11:30")
grupo3 =c("12:00","12:30","13:00","13:30", "14:00","14:30","15:00","15:30",
          "16:00","16:30", "17:00","17:30")
grupo4 =c("18:00","18:30","19:00","19:30", "20:00","20:30","21:00","21:30", 
          "22:00","22:30", "23:00","23:30")
auxLEIBnew <-cbind(auxLEIB,grupohorario)
auxLEIBobs <- filter(auxLEIBnew, tiempo %in% obsLEIB)

auxLEIBobschang <- auxLEIBobs %>% 
  mutate(grupohorario = ifelse(tiempo %in% grupo1, 1, grupohorario)) %>% 
  mutate(grupohorario = ifelse(tiempo %in% grupo2, 2, grupohorario)) %>% 
  mutate(grupohorario = ifelse(tiempo %in% grupo3, 3, grupohorario)) %>% 
  mutate(grupohorario = ifelse(tiempo %in% grupo4, 4, grupohorario))

addrowsIB <- auxLEIBobschang %>% 
  select(fecha, grupohorario, temperatura, direccion, v_nudos, dia , mes, anho, Date) %>%
  mutate(temperatura=as.numeric(temperatura), direccion=as.numeric(direccion), 
         v_nudos=as.numeric(v_nudos)) %>% arrange(fecha, anho, mes, dia, grupohorario)

auxLEIBobschang %>% mutate(temperatura=as.numeric(temperatura), 
                           direccion=as.numeric(direccion), 
                           v_nudos=as.numeric(v_nudos)) -> auxLEIBobschang

auxLEIBobschang <- auxLEIBobschang %>% mutate(grupohorario=as.numeric(grupohorario))
auxLEIBobschang %>% group_by(fecha, dia, mes, anho, grupohorario, Date) 
                %>% summarise(v_nudos=median(v_nudos,na.rm=TRUE), 
                              direccion =median(direccion,na.rm=TRUE), 
                              temperatura=median(temperatura,na.rm=TRUE)) -> LEIB_6h

LEIB_6h %>% filter(grupohorario==1, mes=="marzo", dia ==9, anho==2018) -> G1

marzo9<- data.frame(fecha=as.POSIXct("2018-03-09"),
                    v_nudos=median(c(2,7,5,3,5,3)), 
                    dia = 9,
                    Date=as.Date("2018-03-09"),
                    temperatura=median(c(10,11,12,7,7,6)),
                    direccion=median(c(250,240,340,330,310)),
                    anho=2018, grupohorario =1,
                    mes="marzo")
addrowsIB <- full_join(LEIB_6h,marzo9)
addrowsIB %>% select(fecha, temperatura, direccion, v_nudos, anho, mes, 
                     dia, grupohorario, Date) %>% 
  arrange(anho, dia, grupohorario, mes) -> LEIB_6h

#Last Step
LEIB_6h <- LEIB_6h %>% mutate(EsteOeste=(-v_nudos)*sin((direccion)*pi/180)) %>% 
  mutate(NorteSur = (-v_nudos)*cos((direccion)*pi/180)) %>%
  mutate(EsteOeste=round(EsteOeste, digits = 1)) %>% 
  mutate(NorteSur=round(NorteSur, digits = 1))
LEIB_6h %>% arrange(anho, mes, dia, grupohorario) -> LEIB_6h

horario <- rep(0,2708)
grupo_1 = c("00:00", "01:00", "02:00")
grupo_2 = c("03:00", "04:00", "05:00")
grupo_3 = c("06:00", "07:00", "08:00")
grupo_4 = c("09:00", "10:00", "11:00")
grupo_5 = c("12:00", "13:00", "14:00")
grupo_6 = c("15:00", "16:00", "17:00")
grupo_7 = c("18:00", "19:00", "20:00")
grupo_8 = c("21:00", "22:00", "23:00")
addrowsPAnew <-cbind(addrowsPA2, horario)
LEPA_3h <- addrowsPAnew %>% 
  mutate(horario = ifelse(tiempo %in% grupo_1, 1, horario)) %>% 
  mutate(horario = ifelse(tiempo %in% grupo_2, 2, horario)) %>% 
  mutate(horario = ifelse(tiempo %in% grupo_3, 3, horario)) %>% 
  mutate(horario = ifelse(tiempo %in% grupo_4, 4, horario)) %>% 
  mutate(horario = ifelse(tiempo %in% grupo_5, 5, horario)) %>% 
  mutate(horario = ifelse(tiempo %in% grupo_6, 6, horario)) %>% 
  mutate(horario = ifelse(tiempo %in% grupo_7, 7, horario)) %>% 
  mutate(horario = ifelse(tiempo %in% grupo_8, 8, horario)) %>% 
  select(fecha, Date, v_nudos, tiempo, dia, mes, anho, temperatura, 
         direccion, horario)

LEPA_3h <- LEPA_3h %>% mutate(horario=as.numeric(horario))
LEPA_3h <- LEPA_3h %>% arrange(fecha, anho, mes, dia, horario)
LEPA_3h <- LEPA_3h %>% group_by(fecha, dia, mes, anho, horario) %>%
  summarise(v_nudos=median(v_nudos,na.rm=TRUE),
            direccion = median(direccion,na.rm=TRUE),
            temperatura=median(temperatura,na.rm=TRUE)) 

LEPA_3h %>% filter(horario=="1") 
LEPA_3h %>% filter(horario=="2") 
LEPA_3h %>% filter(horario=="3") 
LEPA_3h %>% filter(horario=="4")
LEPA_3h %>% filter(horario=="5")
LEPA_3h %>% filter(horario=="6") 
LEPA_3h %>% filter(horario=="7")
LEPA_3h %>% filter(horario=="8")

addrowsPA %>% select(fecha, temperatura, direccion, v_nudos, anho, mes, 
                     dia, Date, tiempo) %>% 
  arrange(mes, anho, dia,tiempo)-> LEPA


LEPA_3h <- LEPA_3h %>% mutate(EsteOeste=(-v_nudos)*sin((direccion)*pi/180)) %>% 
  mutate(NorteSur = (-v_nudos)*cos((direccion)*pi/180)) %>%
  mutate(EsteOeste=round(EsteOeste, digits = 1)) %>% 
  mutate(NorteSur=round(NorteSur, digits = 1))

horario <- rep(0,2798)
grupo_1 = c("00:00", "01:00", "02:00")
grupo_2 = c("03:00", "04:00", "05:00")
grupo_3 = c("06:00", "07:00", "08:00")
grupo_4 = c("09:00", "10:00", "11:00")
grupo_5 = c("12:00", "13:00", "14:00")
grupo_6 = c("15:00", "16:00", "17:00")
grupo_7 = c("18:00", "19:00", "20:00")
grupo_8 = c("21:00", "22:00", "23:00")
addrowsMHnew <-cbind(addrowsMH2, horario)
LEMH_3h <- addrowsMHnew %>% 
  mutate(horario = ifelse(tiempo %in% grupo_1, 1, horario)) %>% 
  mutate(horario = ifelse(tiempo %in% grupo_2, 2, horario)) %>% 
  mutate(horario = ifelse(tiempo %in% grupo_3, 3, horario)) %>% 
  mutate(horario = ifelse(tiempo %in% grupo_4, 4, horario)) %>% 
  mutate(horario = ifelse(tiempo %in% grupo_5, 5, horario)) %>% 
  mutate(horario = ifelse(tiempo %in% grupo_6, 6, horario)) %>% 
  mutate(horario = ifelse(tiempo %in% grupo_7, 7, horario)) %>% 
  mutate(horario = ifelse(tiempo %in% grupo_8, 8, horario)) %>% 
  select(fecha, Date, v_nudos, tiempo, dia, mes, anho, 
         temperatura, direccion, horario)

LEMH_3h <- LEMH_3h %>% mutate(horario=as.numeric(horario))
LEMH_3h <- LEMH_3h %>% arrange(fecha, anho, mes, dia, horario)
LEMH_3h <- LEMH_3h %>% group_by(fecha, dia, mes, anho, horario) %>% 
  summarise(v_nudos=median(v_nudos,na.rm = TRUE), 
            direccion =median(direccion,na.rm =TRUE), 
            temperatura=median(temperatura,na.rm =TRUE))

LEMH_3h %>% filter(horario=="1") 
LEMH_3h %>% filter(horario=="2")
LEMH_3h %>% filter(horario=="3") 
LEMH_3h %>% filter(horario=="4")
LEMH_3h %>% filter(horario=="5")
LEMH_3h %>% filter(horario=="6") 
LEMH_3h %>% filter(horario=="7")
LEMH_3h %>% filter(horario=="8")

addrowsMH %>% select(fecha, temperatura, direccion, v_nudos, anho, 
                     mes, dia, Date, tiempo) %>% 
  arrange(mes, anho, dia, tiempo)-> LEMH
#To create Este-Oeste and Norte-Sur components
LEMH_3h <- LEMH_3h %>% mutate(EsteOeste=(-v_nudos)*sin((direccion)*pi/180)) %>% 
  mutate(NorteSur = (-v_nudos)*cos((direccion)*pi/180))%>%
  mutate(EsteOeste=round(EsteOeste, digits = 1)) %>% 
  mutate(NorteSur=round(NorteSur, digits = 1))

#Time series of wind speed.
#Mallorca: (Similary we can create for Menorca and Ibiza)
LEPAmarzo <- LEPA %>% filter(mes=="marzo") %>% arrange(anho, dia, tiempo)
LEPAagosto <- LEPA %>% filter(mes=="agosto")%>% arrange(anho, dia, tiempo)
LEPAmarzo.ts<-ts(LEPAmarzo$v_nudos,freq=24)
LEPAagosto.ts<-ts(LEPAagosto$v_nudos,freq =24)

PA_ts_a<-autoplot(LEPAmarzo.ts, ts.colour = "blue", 
                  ylim=c(0,35), ylab = "Velocidad del viento", 
                  xlab = "", 
                  main = "Aeropuerto de Mallorca - Marzo") +
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5),
        plot.title = element_text(color="black", size=14, face="bold"), 
        axis.title.y = element_text(color="black", size=12, face="bold"))+
  scale_x_continuous(breaks=c(1,6,11,16,21,26,31,36,41,46,51,56,61),
                     labels = c("1, marzo, 17","6, marzo, 17","11, marzo, 17",
                                "16, marzo, 17","21, marzo, 17","26, marzo, 17",
                                "31, marzo, 17","5, marzo, 18", "10, marzo, 18",
                                "15, marzo, 18", "20, marzo, 18","25, marzo, 18", 
                                "30, marzo, 18"))
PA_ts_m<-autoplot(LEPAagosto.ts, ts.colour = "blue", 
                  ylim=c(0,35), ylab = "Velocidad del viento", 
                  xlab = "", 
                  main = "Aeropuerto de Mallorca - Agosto") +
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5),
        plot.title = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold"))+
  scale_x_continuous(breaks=c(1,6,11,16,21,26,31,36,41,46,51,56,61),
                     labels=c("1, agosto, 17","6, agosto, 17","11, agosto, 17",
                              "16, agosto, 17","21, agosto, 17","26, agosto, 17",
                              "31, agosto, 17","5, agosto, 18", "10, agosto, 18",
                              "15, agosto, 18", "20, agosto, 18","25, agosto, 18", 
                              "30, agosto, 18"))

ggAcf(LEPAmarzo.ts, plot = T, lag.max = NULL )+ 
  xlab("Retardos") +
  ylab("Autocorrelaciones") +
  ggtitle("Estación de Mallorca en el mes de marzo") -> MMe
ggPacf(LEPAmarzo.ts, plot = T, lag.max = NULL)+
  xlab("Retardos") +
  ylab("Autocorrelaciones Parciales") +
  ggtitle("Estación de Mallorca en el mes de marzo") -> AMe
grid.arrange(MMe, AMe, ncol = 1)

#Wind Rose
plot.windrose2 <- function(data,
                           spd,
                           dir,
                           spdres = 2,
                           #spdres = 0,
                           dirres = 22.5, #Así hacemos 16 divisiones
                           spdmin = 0,
                           spdmax = 35,
                           spdseq = NULL,
                           palette = "YlGnBu",
                           countmax = NA, #Frecuencia
                           debug = 0){
  # Tenemos ya el tipo de dato numérico para pasar a la función:
  if (is.numeric(spd) & is.numeric(dir)){
    data <- data.frame(spd = spd,
                       dir = dir)
    spd = "spd"
    dir = "dir"
  } else if (exists("data")){
    
  }  
  
  
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA
  
  # contenedores para la velocidad del viento:
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  # info sobre el número de contenedores:
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1
  
  # tantos colores como divisiones haga:
  spd.colors <- colorRampPalette(brewer.pal(min(max(3,n.colors.in.range),
                                                min(9,n.colors.in.range)),palette))(n.colors.in.range)
  
  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq,max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),'-',c(spdseq[2:n.spd.seq])),
                    paste(spdmax, "-", max(data[[spd]],na.rm = TRUE)))
    spd.colors <- c(spd.colors, "gray0")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]), '-', c(spdseq[2:n.spd.seq]))    
  }
  data$spd.binned <- cut(x = data[[spd]], breaks = spd.breaks, labels = spd.labels, 
                         ordered_result = TRUE)
  
  
  data. <- na.omit(data)
  
  # Defino los 16 contenedores y sus etiquetas:
  # Secuencia de 22.5 grados por contenedor:
  dir.breaks <- c(-dirres/2, seq(dirres/2, 360-dirres/2, by = dirres), 360+dirres/2)
  
  # Nombra a los contenedores con las etiquetas para los puntos carinales.
  dir.labels <- c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW",
                  "NW","NNW","N") 
  
  
  # Asigno a cada dirección del viento un contenedor:
  dir.binned <- cut(data[[dir]], breaks = dir.breaks, ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  
  # Run debug if required ----
  if (debug>0){    
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")       
  }  
  
  
  if(packageVersion("ggplot2") > "2.2"){    
    cat("Hadley broke my code\n")
    data$spd.binned = with(data, factor(spd.binned, levels = rev(levels(spd.binned))))
    spd.colors = rev(spd.colors)
  }
  
  # Crea usando ggplot la rosa gráfica:
  p.windrose2 <- ggplot(data = data, aes(x = dir.binned, fill = spd.binned)) +
    #Acumulo en las barras
    geom_bar() + 
    # 
    scale_x_discrete(drop = FALSE, labels = waiver()) +
    # Gráfico circular con barras apiladas en coord polares:
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Velocidad (kn)", values = spd.colors, drop = FALSE) +
    # Eje x sin nombre:
    theme(axis.title.x = element_blank())+
    # Etiqueta el título:
    labs(title = "Agosto")+
    ylab("Frecuencia")+xlab("")
  
  # Ajusta los ejes:
  if (!is.na(countmax)){
    p.windrose2 <- p.windrose2 + ylim(c(0,countmax))
  }
  
  
  print(p.windrose2)  
  
  return(p.windrose2)
}
#For each month
LEPA_m <- LEPA_3h %>% filter(mes == "marzo", direccion>=0)   
LEPA_m <- plot.windrose2(spd = LEPA_m$v_nudos,
                        dir = LEPA_m$direccion, 
                        spdseq = c(0,6,9,12,15,20,25,35)) 


LEPA_a <- LEPA_3h %>% filter(mes == "agosto", direccion>=0)  
LEPA_a <- plot.windrose2(spd = LEPA_a$v_nudos,
                         dir = LEPA_a$direccion, 
                         spdseq = c(0,6,9,12,15,20,25,35))

#Wind Speed - Wind direction series .
#With the variables created through the change of coordinates we build the time series 
#of the wind components.

#March:
LEPAmarzoVD <- LEPA_3h %>% filter(mes=="marzo") %>% arrange(anho, dia, horario)
LEPAmarzoVD_EO.ts <- ts(LEPAmarzoVD$EsteOeste, freq = 8)
LEPAmarzoVD_NS.ts <- ts(LEPAmarzoVD$NorteSur, freq = 8)
#August:
LEPAagostoVD <- LEPA_3h %>% filter(mes=="agosto")%>% arrange(anho, dia, horario)
LEPAagostoVD_EO.ts <- ts(LEPAagostoVD$EsteOeste, freq = 8)
LEPAagostoVD_NS.ts <- ts(LEPAagostoVD$NorteSur, freq = 8)
#March graph (Similary, August)
PA_Y_t1_m <- autoplot(LEPAmarzoVD_EO.ts, ts.colour = "red", 
                      ylab = "Componente Este - Oeste", 
                      xlab = "", 
                      main = "Aeropuerto de Mallorca  - Marzo")+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5),
        plot.title = element_text(color="black", size=14, face="bold"), 
        axis.title.y = element_text(color="black", size=12, face="bold"))+
  scale_x_continuous(breaks=c(1,6,11,16,21,26,31,36,41,46,51,56,61),
                     labels = c("1, marzo, 17","6, marzo, 17","11, marzo, 17",
                                "16, marzo, 17","21, marzo, 17","26, marzo, 17",
                                "31, marzo, 17","5, marzo, 18", "10, marzo, 18",
                                "15, marzo, 18", "20, marzo, 18","25, marzo, 18", 
                                "30, marzo, 18"))

PA_Y_t2_m <- autoplot(LEPAmarzoVD_NS.ts, ts.colour = "blue", 
                      ylab = "Componente Norte - Sur", 
                      xlab = "", 
                      main = "Aeropuerto de Mallorca - Marzo")+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5),
        plot.title = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold"))+
  scale_x_continuous(breaks=c(1,6,11,16,21,26,31,36,41,46,51,56,61),
                     labels = c("1, marzo, 17","6, marzo, 17","11, marzo, 17",
                                "16, marzo, 17","21, marzo, 17","26, marzo, 17",
                                "31, marzo, 17","5, marzo, 18", "10, marzo, 18",
                                "15, marzo, 18", "20, marzo, 18","25, marzo, 18", 
                                "30, marzo, 18"))
#FAS and FAP:
ggAcf(LEPAmarzoVD_EO.ts, plot = T, lag.max = NULL )+
  xlab("Retardos") +
  ylab("Autocorrelaciones") +
  ggtitle("Estación de Mallorca en el mes de marzo") -> FasEOMallorca
ggPacf(LEPAmarzoVD_EO.ts, plot = T, lag.max = NULL)+
  xlab("Retardos") +
  ylab("Autocorrelaciones Parciales") +
  ggtitle("Estación de Mallorca en el mes de marzo") -> FapEOMallorca
grid.arrange(FasEOMallorca,FapEOMallorca , ncol = 1)

#24-hour mean speed
MeanMallMar <- LEPA %>% filter(mes == "marzo") %>% group_by(tiempo) %>%
  summarise(viento=mean(v_nudos,na.rm = TRUE))
MeanMallMar.ts <- ts(MeanMallMar$viento)
MeanMallAgos <- LEPA %>% filter(mes == "agosto") %>% group_by(tiempo) %>%
  summarise(viento=mean(v_nudos,na.rm = TRUE))
MeanMallAgos.ts <- ts(MeanMallAgos$viento)


Mallorca_marzo_horas<- autoplot(MeanMallMar.ts, ts.colour = "blue", 
                                ylab = "Velocidad media del viento", 
                                xlab = "", ylim=c(0,14),
                                main = "Aeropuerto de Mallorca - Marzo")+
  theme(axis.text.x=element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold"))+
  scale_x_continuous(breaks= seq(1,24,1),
                     labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8",
                              "9", "10", "11", "12", "13", "14", "15", "16",
                              "17", "18", "19", "20", "21", "22", "23"))
Mallorca_horas_agosto<- autoplot(MeanMallAgos.ts, ts.colour = "blue", 
                                 ylab = "Velocidad media del viento", 
                                 xlab = "",  ylim=c(0,14),
                                 main = "Aeropuerto de Mallorca - Agosto")+
  theme(axis.text.x=element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold"))+
  scale_x_continuous(breaks= seq(1,24,1),
                     labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8",
                              "9", "10", "11", "12", "13", "14", "15", "16",
                              "17", "18", "19", "20", "21", "22", "23"))