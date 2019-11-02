#**********************************************************************************
#AUTHOR				  :	Marta Padin
#CREATION DATE	: Feb 2019 - Sep 2019
#**********************************************************************************
#DESCRIPTION:   This file is a draft where you can find how to make data wrangling. 
#               
#
#**********************************************************************************
#To use this file we need a txt containing data from the stations of 
#Palma (Mallorca), Es Codolar (Ibiza) and Mahón (Menorca). This files are: PA.txt,
#MH.txt and IB.txt.
#The structure of the files must be as follows:
#fecha dia mes anho horario v_nudos direccion temperatura EsteOeste NorteSur mes_num
#**********************************************************************************
#Data source: Iowa Environmental Mesonet.
#Observations collected at airports.
#ASOS is designed to support weather forecasting activities and aviation operations 
#and, at the same time, support the needs of meteorological, hydrological and 
#climatological research communities.



#Necessary libraries:
library(tidyverse)
library(readxl)
library(ggplot2)
library(gridExtra)
library(GGally)


#We read the files and separate in months.
Mallorca <- "PA.txt"
Mallorca <- read.table(Mallorca, sep = " ", header = T)
PAmarzo <- Mallorca %>% filter(mes=="marzo")
PAagosto <- Mallorca %>% filter(mes=="agosto")

Menorca <- "MH.txt"
Menorca <- read.table(Menorca, sep = " ", header = T)
MHmarzo <- Menorca %>% filter(mes=="marzo")
MHagosto <- Menorca %>% filter(mes=="agosto")

Ibiza <- "IB.txt"
Ibiza <- read.table(Ibiza, sep = " ", header =T)
IBmarzo <- Ibiza %>% filter(mes=="marzo")
IBagosto <- Ibiza %>% filter(mes=="agosto")


#We create the components.
Mallorca$u<-(Mallorca$v_nudos*(-1))*sin(pi/180*Mallorca$direccion)
Mallorca$Diass <- seq(1,992,1)

Menorca$u<-(Menorca$v_nudos*(-1))*sin(pi/180*Menorca$direccion)
Menorca$Diass <- seq(1,992,1)

Ibiza$u<-(Ibiza$v_nudos*(-1))*sin(pi/180*Ibiza$direccion)
Ibiza$Diass <-seq(1,496,1)
#Componente v: NorteSur
Mallorca$v<-(Mallorca$v_nudos*(-1))*cos(pi/180*Mallorca$direccion)
Menorca$v<-(Menorca$v_nudos*(-1))*cos(pi/180*Menorca$direccion)
Ibiza$v<-(Ibiza$v_nudos*(-1))*cos(pi/180*Ibiza$direccion)


#To create the stations data.
dia <- seq(1,8,1)
xreal = c(-0.7, -0.2, -0.5, -0.9,  6.1 , 5.8, -3.0, -2.0)
yreal = c(-1.9 ,-1.0, -0.9,  0.5,  5.1,  6.9,  0.5  ,0.3)
#Componente Este: 
eci_0975=c(0.00000,1.922958,9.525541,9.824769,14.148665,13.434480,5.882098,11.821178)
eci_0025=c(-17.31054,-15.315561,-9.348316,-7.625613,-4.412817,-6.702222,-8.208697,-4.949227)
eyt_median=c(-8.1166071,-5.9872674,-1.17,3.0789263,5.4809911,2.0541203,-0.5905586 ,4.8007014)
#Componente Norte: 
nci_0975=c( 2.089211,3.106504,2.943172,4.291394,10.4659217,8.3253727,7.397228,6.093640)
nci_0025=c(-11.273212,-7.950982,-7.089317,-6.074364,-0.8554297,-0.6995072,-3.561402,-4.705724)
nyt_median=c(-4.8765268,-2.7549013,-1.6074911,-0.8803244,3.5173867 ,3.6951045,1.5140262,0)
data <- data.frame(nci_0025,nci_0975,nyt_median, xreal, yreal,dia,eci_0025, eci_0975,eyt_median)

#The process is repeated for the stations of Menorca and Ibiza.
#We build the graph for the east west component and the process for the north south component
#and the other stations is repeated.
ggplot()+
  geom_line(aes(x=data$dia[1:8],y=data$xreal[1:8]), color = "red")+
  geom_line(aes(x=data$dia[1:8],y=data$eci_0025), color = "black", linetype = 4)+
  geom_line(aes(x=data$dia[1:8],y=data$eyt_median), color = "blue")+
  geom_line(aes(x=data$dia[1:8],y=data$eci_0975), color = "black", linetype = 4)+ 
  ggtitle ("Aeropuerto de Mallorca")+ 
  labs(x = " ",y = "Este - Oeste")+
  theme(axis.title.y = element_text(face="bold", vjust=1.5, colour="black",
                                    size=rel(0.6)))+
  theme(axis.text.x=element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(color="black", size=12), 
        axis.title.y = element_text(color="black", size=9))+
  scale_x_continuous(breaks=seq(1,8,1),
                     labels = c("G1","G2","G3","G4","G5","G6","G7","G8"))->eo_pa
ggplot()+
  geom_line(aes(x=data$dia[1:8],y=data$yreal[1:8]), color = "red")+
  geom_line(aes(x=data$dia[1:8],y=data$nci_0025), color = "black", linetype = 4)+
  geom_line(aes(x=data$dia[1:8],y=data$nyt_median), color = "blue")+
  geom_line(aes(x=data$dia[1:8],y=data$nci_0975), color = "black", linetype = 4)+ 
  ggtitle (" ")+ 
  labs(x = " ",y = "Norte - Sur ")+
  theme(axis.title.y = element_text(face="bold", vjust=1.5, colour="black",
                                    size=rel(0.6)))+
  theme(axis.text.x=element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(color="black", size=12), 
        axis.title.y = element_text(color="black", size=9))+
  scale_x_continuous(breaks=seq(1,8,1),
                     labels = c("G1","G2","G3","G4","G5","G6","G7","G8"))->ns_pa

#With the arrangeGrob/grid.arrange() pair of functions, gridExtra builds upon gtable 
#to arrange multiple grobs on a page.
grid.arrange(eo_pa, ns_pa, eo_mh, ns_mh, eo_ib, ns_ib, ncol = 2, nrow = 3)
#It defaults to saving the last plot that you displayed, using the size of the current 
#graphics device.
ggsave <- ggplot2::ggsave; body(ggsave) <- body(ggplot2::ggsave)[-2]
g <- arrangeGrob(eo_pa, ns_pa, eo_mh, ns_mh, eo_ib, ns_ib, ncol = 2, nrow = 3) #generates g
ggsave(file="24_24_trunc.png", g) #saves g
