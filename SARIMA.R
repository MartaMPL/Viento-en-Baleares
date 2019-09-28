#**********************************************************************************
#AUTHOR				  :	Marta Padin
#CREATION DATE	: Feb 2019 - Sep 2019
#**********************************************************************************
#DESCRIPTION:   This file is a draft where you can find how to make SARIMA 
#               forecasts. We can see Mallorca.
#
#**********************************************************************************
#To use this file we need a output of DataWrangling.R
#In addition, 8 daily observations are needed for the Mallorca and Menorca stations.


#Time series Menorca

LEMH19marzo <- LEMH19
LEMH19marzo.ts<-ts(LEMH19marzo$v_nudos,freq=24)

LEMH19marzoVD <- LEMH19_3h %>% arrange(dia, horarioLEMH)
LEMH19marzoVD_EO.ts <- ts(LEMH19marzoVD$EsteOeste, freq = 8)
LEMH19marzoVD_NS.ts <- ts(LEMH19marzoVD$NorteSur, freq = 8)

#Forecast: station of Menorca (Mahón)

#Time series representation: March
autoplot(LEMHmarzoVD_NS.ts, ts.colour = "blue", 
         ylab = "Componente Norte - Sur", 
         xlab = "", 
         main = "Aeropuerto de Menorca - Marzo")+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5),
        plot.title = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold"))+
  scale_x_continuous(breaks=c(1,6,11,16,21,26,31,36,41,46,51,56,61),
                     labels = c("1, marzo, 17","6, marzo, 17","11, marzo, 17",
                                "16, marzo, 17","21, marzo, 17","26, marzo, 17",
                                "31, marzo, 17","5, marzo, 18", "10, marzo, 18",
                                "15, marzo, 18", "20, marzo, 18","25, marzo, 18", 

#Dickey-Fuller Aumentado (Augmented Dickey-Fuller Test (ADF))

adf.test(LEMHmarzoVD_NS.ts, alternative = c("stationary"), 
         k = trunc((length(LEMHmarzoVD_NS.ts)-1)^(1/3)))

#p-valor de 0.01, indicates that we reject the null hypothesis of non-stationarity.

#Phillips-Perron test: 

pp.test(LEMHmarzoVD_NS.ts, alternative="stationary")

#p-valor de 0.01, we reject the null hypothesis of non-stationarity.

#Find the best SARIMA

autoplot(acf(LEMHmarzoVD_NS.ts, lag=70, plot = FALSE))+
  xlab("Retardos") + ylab("Autocorrelaciones")+
  ggtitle("Componente Norte-Sur en Mahón durante los meses de marzo")-> FAS_03_menorca_NS
autoplot(pacf(LEMHmarzoVD_NS.ts, lag=70,  plot = FALSE)) +
  xlab("Retardos") + ylab("Autocorrelaciones Parciales")+
  ggtitle(" ")-> FAP_03_menorca_NS
grid.arrange(FAS_03_menorca_NS,FAP_03_menorca_NS, ncol =1)

#We observe in the simple autocorrelation function that takes values close to 1 in the 
#first delays and slowly decays as the delay increases. On the other hand, in the 
#function of partial autocorrelations it is observed that all delays are within the 
#confidence bands, except for the first, which suggests that an AR (1) could be a good model. For this reason we will study this model for the series. In addition, a certain seasonal structure is appreciated, so a multiplicative scheme with a seasonal part could be a good adjustment.
#We propose, then, the following models: 

MH_NS_m1<- Arima(LEMHmarzoVD_NS.ts, order=c(1,0,0), include.mean = FALSE)
MH_NS_m2<- Arima(LEMHmarzoVD_NS.ts, order=c(1,0,0), include.mean = TRUE)
MH_NS_m3<- Arima(LEMHmarzoVD_NS.ts, order=c(1,0,1), include.mean = FALSE)
MH_NS_m4<- Arima(LEMHmarzoVD_NS.ts, order=c(1,0,0), seasonal=list(order=c(1,0,0),period=8),
                 include.mean = FALSE)
MH_NS_m5<- Arima(LEMHmarzoVD_NS.ts, order=c(1,0,0), seasonal=list(order=c(1,0,1),period=8),
                 include.mean = TRUE)
MH_NS_m6<- Arima(LEMHmarzoVD_NS.ts, order=c(1,0,0), seasonal=list(order=c(1,0,0),period=8),
                 include.mean = TRUE)
MH_NS_m7<- Arima(LEMHmarzoVD_NS.ts, order=c(2,0,3))



#We calculate the value of the Akaike information criterion (AIC):
AIC(MH_NS_m1,MH_NS_m2,MH_NS_m3,MH_NS_m4,MH_NS_m5,MH_NS_m6,MH_NS_m7)


#BIC
BIC(MH_NS_m1,MH_NS_m2,MH_NS_m3,MH_NS_m4,MH_NS_m5,MH_NS_m6,MH_NS_m7)

#We check the residual normality assumptions


(fit5 <- MH_NS_m1)

checkresiduals(fit5)



ForMH_NS_m1 <-forecast(MH_NS_m1, h=8) #Forecast for March 1
autoplot(ForMH_NS_m1)


y1 <- LEMH_3h %>% filter(mes=="marzo")
y1<-y1[9:10]
y2<-LEMH19_3h[7:8]
y3<-rbind(y1,y2)
y3$Dia <- seq(1,744,1)
ForMH_NS_m1$Dia <-seq(1,8,1)

ggplot()+
  geom_line(aes(x=y3$Dia[400:503],y=y3$NorteSur[400:503]))+
  geom_point(aes(x=y3$Dia[496:503],y=ForMH_NS_m1$mean), color = "red")+
  ggtitle ("Componente Norte - Sur Menorca (Modelo 1)")+ 
  labs(x = "",y = "Velocidad-dirección del viento")+ 
  theme (plot.title = element_text(size=rel(1.2), vjust=2, face="bold", color="red", 
                                   lineheight=1.5)) + 
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, colour="blue", size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, colour="blue", size=rel(1)))


#August NS
autoplot(LEMHagostoVD_NS.ts)

#Tests:
adf.test(LEMHagostoVD_NS.ts, alternative = c("stationary"), 
         k = trunc((length(LEMHagostoVD_NS.ts)-1)^(1/3)))
pp.test(LEMHagostoVD_NS.ts, alternative="stationary")

autoplot(acf(LEMHagostoVD_NS.ts, lag=120, plot = FALSE))+
  xlab("Retardos") + ylab("Autocorrelaciones")+
  ggtitle("Componente Norte - Sur en Mahón durante los meses de agosto")-> FAS_08_menorca_NS
autoplot(pacf(LEMHagostoVD_NS.ts, lag=120,  plot = FALSE)) +
  xlab("Retardos") + ylab("Autocorrelaciones Parciales")+
  ggtitle(" ")-> FAP_08_menorca_NS
grid.arrange(FAS_08_menorca_NS,FAP_08_menorca_NS, ncol =1)


MH_NS_a1<- Arima(LEMHagostoVD_NS.ts, order=c(2,0,0), seasonal=list(order=c(1,0,0),period=8),
                 include.mean = TRUE)
MH_NS_a2<- Arima(LEMHagostoVD_NS.ts, order=c(3,0,0), seasonal=list(order=c(1,0,0),period=8),
                 include.mean = TRUE)
MH_NS_a3<- Arima(LEMHagostoVD_NS.ts, order=c(3,0,1), seasonal=list(order=c(1,0,0),period=8),
                 include.mean = TRUE)
MH_NS_a4<- Arima(LEMHagostoVD_NS.ts, order=c(2,0,1), seasonal=list(order=c(1,0,0),period=8),
                 include.mean = FALSE)
MH_NS_a5<- Arima(LEMHagostoVD_NS.ts, order=c(1,0,0), include.mean = TRUE)
MH_NS_a6<- Arima(LEMHagostoVD_NS.ts, order=c(2,0,0), seasonal=list(order=c(2,0,0),period=8),
                 include.mean = TRUE)

MH_NS_a7<- Arima(LEMHagostoVD_NS.ts, order=c(2,0,3))

#AIC & BIC
AIC(MH_NS_a1,MH_NS_a2,MH_NS_a3,MH_NS_a4,MH_NS_a5,MH_NS_a6,MH_NS_a7)
BIC(MH_NS_a1,MH_NS_a2,MH_NS_a3,MH_NS_a4,MH_NS_a5,MH_NS_a6,MH_NS_a7)


#Residuals:
(fit6 <- MH_NS_a1)
checkresiduals(MH_NS_a1)

ForMH_NS_a1 <-forecast(MH_NS_a1, h=8) #Forecast for August 1
autoplot(ForMH_NS_a1)


#Componente EO

autoplot(LEMHmarzoVD_EO.ts, ts.colour = "blue", 
         ylab = "Componente Este - Oeste", 
         xlab = "", 
         main = "Aeropuerto de Menorca  - Marzo")+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5),
        plot.title = element_text(color="black", size=14, face="bold"), 
        axis.title.y = element_text(color="black", size=12, face="bold"))+
  scale_x_continuous(breaks=c(1,6,11,16,21,26,31,36,41,46,51,56,61),
                     labels = c("1, marzo, 17","6, marzo, 17","11, marzo, 17",
                                "16, marzo, 17","21, marzo, 17","26, marzo, 17",
                                "31, marzo, 17","5, marzo, 18", "10, marzo, 18",
                                "15, marzo, 18", "20, marzo, 18","25, marzo, 18", 
                                "30, marzo, 18"))

#Tests

adf.test(LEMHmarzoVD_EO.ts, 

pp.test(LEMHmarzoVD_EO.ts)

kpss.test(LEMHmarzoVD_EO.ts, null = )

LEMHmarzoVD_EO.ts %>% diff() %>%  ggtsdisplay(main="")


diff_01Menorca<-diff(LEMHmarzoVD_EO.ts)
autoplot(acf(diff_01Menorca, lag=50, plot = FALSE))+
  xlab("Retardos") + ylab("Autocorrelaciones")+
  ggtitle("Componente Este - Oeste en Mahón durante los meses de marzo")-> FAS_03_menorca_EO
autoplot(pacf(diff_01Menorca, lag=50,  plot = FALSE)) +
  xlab("Retardos") + ylab("Autocorrelaciones Parciales")+
  ggtitle(" ")-> FAP_03_menorca_EO
grid.arrange(FAS_03_menorca_EO,FAP_03_menorca_EO, ncol =1)


MH_EO_m1<- Arima(diff_01Menorca, order=c(1,0,1), seasonal=list(order=c(0,0,1),
                                                               period=8),
                 include.mean = FALSE)
MH_EO_m2<- Arima(diff_01Menorca, order=c(2,0,1), seasonal=list(order=c(0,0,1),
                                                               period=8),
                 include.mean = FALSE)
MH_EO_m3<- Arima(diff_01Menorca, order=c(1,0,1), include.mean = FALSE)
MH_EO_m4<- Arima(diff_01Menorca, order=c(1,0,1), seasonal=list(order=c(1,0,1),
                                                               period=8),
                 include.mean = FALSE)
MH_EO_m5<- Arima(diff_01Menorca, order=c(1,0,1), seasonal=list(order=c(0,0,1),
                                                               period=8),
                 include.mean = TRUE)
MH_EO_m6<- Arima(diff_01Menorca, order=c(2,0,2), seasonal=list(order=c(0,0,1),
                                                               period=8),
                 include.mean = TRUE)
MH_EO_m7<- Arima(diff_01Menorca, order=c(2,0,3))

#AIC & BIC
AIC(MH_EO_m1,MH_EO_m2,MH_EO_m3,MH_EO_m4,MH_EO_m5,MH_EO_m6,MH_EO_m7)
BIC(MH_EO_m1,MH_EO_m2,MH_EO_m3,MH_EO_m4,MH_EO_m5,MH_EO_m6,MH_EO_m7)
#Residuals
(fit8 <- MH_EO_m1)
checkresiduals(fit8)
ForMH_EO_m1 <-forecast(MH_EO_m1, h=8) 
autoplot(ForMH_EO_m1)



y1 <- LEMH_3h %>% filter(mes=="marzo")
y1<-y1[9:10]
y2<-LEMH19_3h[7:8]
y3<-rbind(y1,y2)
y3$Dia <- seq(1,744,1)
ForMH_EO_m1$Dia <-seq(1,8,1)


ggplot()+
  geom_line(aes(x=y3$Dia[400:503],y=y3$EsteOeste[400:503]))+
  geom_point(aes(x=y3$Dia[496:503],y=ForMH_EO_m1$mean), color = "red")+
  ggtitle ("Componente Este - Oeste Menorca (Modelo 1)")+ 
  labs(x = "",y = "Velocidad-dirección del viento")+ 
  theme (plot.title = element_text(size=rel(1.2), vjust=2, face="bold", color="red",
                                   lineheight=1.5)) + 
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, colour="blue", 
                                    size=rel(1))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, colour="blue", 
                                    size=rel(1)))


#August
autoplot(LEMHagostoVD_EO.ts)



adf.test(LEMHagostoVD_EO.ts, alternative = c("stationary"), 
         k = trunc((length(LEMHagostoVD_EO.ts)-1)^(1/3)))

pp.test(LEMHagostoVD_EO.ts, alternative="stationary")

kpss.test(LEMHmarzoVD_EO.ts, null = "Trend")

LEMHagostoVD_EO.ts %>% diff() %>% ggtsdisplay(main="")

diff_01_agostoMen <- diff(LEMHagostoVD_EO.ts)

autoplot(acf(diff_01_agostoMen, lag=120, plot = FALSE))+
  xlab("Retardos") + ylab("Autocorrelaciones")+
  ggtitle("Componente Este - Oeste en Mahón durante los meses de agosto")-> FAS_08_menorca_EO
autoplot(pacf(diff_01_agostoMen, lag=120,  plot = FALSE)) +
  xlab("Retardos") + ylab("Autocorrelaciones Parciales")+
  ggtitle(" ")-> FAP_08_menorca_EO
grid.arrange(FAS_08_menorca_EO,FAP_08_menorca_EO, ncol =1)


MH_EO_a1<- Arima(diff(LEMHagostoVD_EO.ts), order=c(2,0,4), seasonal=list(order=c(2,0,1),
                                                                         period=8),
                 include.mean = FALSE)
MH_EO_a2<- Arima(diff(LEMHagostoVD_EO.ts), order=c(2,0,3), seasonal=list(order=c(2,0,1),
                                                                         period=8),
                 include.mean = FALSE)
MH_EO_a3<- Arima(diff(LEMHagostoVD_EO.ts), order=c(2,0,3), seasonal=list(order=c(1,0,1),
                                                                         period=8),
                 include.mean = FALSE)
MH_EO_a4<- Arima(diff(LEMHagostoVD_EO.ts), order=c(2,0,3), seasonal=list(order=c(2,0,0),
                                                                         period=8),
                 include.mean = FALSE)
MH_EO_a7<- Arima(diff(LEMHagostoVD_EO.ts), order=c(2,0,3))


#AIC & BIC
AIC(MH_EO_a1,MH_EO_a2,MH_EO_a3,MH_EO_a4,MH_EO_a7)
BIC(MH_EO_a1,MH_EO_a2,MH_EO_a3,MH_EO_a4,MH_EO_a7)

#Residuals
(fit7 <- MH_EO_a3)
checkresiduals(MH_EO_a3)


ForMH_EO_a3 <-forecast(MH_EO_a3, h=8) #Forecast for August 1
autoplot(ForMH_EO_a3)
