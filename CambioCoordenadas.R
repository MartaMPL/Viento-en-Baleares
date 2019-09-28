#**********************************************************************************
#AUTHOR				  :	Marta Padin
#CREATION DATE	: Feb 2019 - Sep 2019
#**********************************************************************************
#DESCRIPTION:   This file is a draft to perform the coordinate change. 
#               
#
#**********************************************************************************
#input: a=(-1)*Componente Este - Oeste 
#       b=(-1)*Componente Norte - Sur
#outputs: wind direction and wind speed, in degrees and knots, respectively


CambioCoordenadas = function(a,b){
  thetai=0
  if (a>=0){
    if(a==0){
      if(b>0){
        thetai=0
      }else{
        thetai=180
      }
    }else{
      if(b>0){
        thetai=90-atan(b/a)*(180/pi)
      }else{
        if(b<0){
          thetai=90-atan(b/a)*(180/pi)
        }else{
          thetai=90
        }
      }
    }
  }else{
    if(b>0){
      thetai=360+atan(b/a)*(180/pi)
    }else{
      if(b<0){
        thetai=270-atan(b/a)*(180/pi)
        
      }else{
        thetai=270
      }
    } 
  }
  return(thetai)
}

#x=c(2,-2,2,-2,0,0,2,-2)
#y=c(1,1,-1,-1,1,-1,0,0)

n=length(x)
theta=rep(0,n)
for (i in 1:n){
  theta[i]=CambioCoordenadas(x[i],y[i])
}
te=theta*pi/180

#Calculamos la magnitud de la velocidad del viento:
magnitud <-function(x,y){
  v=sqrt(x^2+y^2)
}

for (j in 1:m){
  v[j]=magnitud(NS[j],EO[j])
}
v=round(v,1)