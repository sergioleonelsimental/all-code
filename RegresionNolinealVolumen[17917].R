#Ajuste de funciones de volumen 
#Jaime Padilla y J. Javier Corral-Rivas
#18 de mayo de 2017

library(lattice)  #libreria para realizar graficos

setwd("../data") #Seleccionar carpeta de trabajo
dat <-read.table("F:/2019/Tesis/regno lineal/fos.txt",header=T,blank.lines.skip=F)

xyplot(VT~DN | Especie, dat) #Grafico de relacion volumen~diametro por especie
dat <- dat[dat$Especie=="Pinus cooperi", ] #Seleccionar especies de la base da datos

#estimacion de volumen
#volumen rollo total arbol 'Variable combinada' (VC)
#Obtencion de los parametros de inicio VC
start <- coef(lm(log(H_1)~I(log(X)), data = dat))
start
start[1] <- exp(start[1])
names(start) <- c("b0","b1")
start
#Ajuste no lineal mediante minimos cuadrados ordinarios VC
Vrta_VC <- nls(H_1~b0 + b1*X^2, data = dat, start = start, weight = 1/dat$X^4)
summary(Vrta_VC)

#volumen rollo total arbol 'SCHUMACHER Y HALL(SH)
#Obtencion de los parametros de inicio SH
start <- coef(lm(log(H_1)~I(log(X)),data=dat))
start[1] <- exp(start[1])
names(start) <- c("a0","a1")
start
#Ajuste no linear mediante minimos cuadrados ordinarios SH
Vrta_SH <- nls(H_1~a0*X^a1,data = dat, start=start, weight = 1/dat$X^4)
summary(Vrta_SH)

#volumen rollo total arbol 'Spurr' (SP) 
start <- coef(lm(log(H_1)~-I(log(DN*AT)),data=dat))
start <- exp(start)
names(start) <- c("b0")
start
#Ajuste no linear mediante minimos cuadrados ordinarios Sp
Vrta_Sp <- nls(VT~b0*DN^2*AT,data = dat, start=start, weight = 1/dat$DN^4)
summary(Vrta_Sp)


#Residuals plot VC
plot(dat$VT, residuals(Vrta_VC, type = "pearson")/4, xlab = "Dn", ylab = "Residuales")
abline(h=0, col=2)
#Residuals plot SH
plot(dat$VT, residuals(Vrta_SH, type = "pearson")/4, xlab = "Dn", ylab = "Residuales")
abline(h=0, col=2)
#Residuals plot Sp
plot(dat$VT, residuals(Vrta_Sp, type = "pearson")/4, xlab = "Dn", ylab = "Residuales")
abline(h=0, col=2)

#Gr?fico de predicci?n de los modelos ajustados
with(dat,plot(DN,VT,xlab="Di?metro (cm)",ylab="Volumen (m3)"))
D <- sort(dat$DN)
H <- sort(dat$AT) 
lines(D,predict(Vrta_SH,newdata = data.frame(DN=D, AT=H)), col=2)
lines(D,predict(Vrta_VC,newdata = data.frame(DN=D, AT=H)), col=3)
lines(D,predict(Vrta_Sp,newdata = data.frame(DN=D, AT=H)), col=4)

#Funcion para estimar estadiscticos
Estad.ajus <- function(Vrta){
  res.Vrta <- cbind(dat, res = resid(Vrta), fit = fitted(Vrta))
  SSE.Vrta <- sum(res.Vrta$res^2)
  SSC.Vrta<- sum((res.Vrta$VT))-mean(res.Vrta$VT)^2
  n.Vrta <- nrow(res.Vrta)
  p.Vrta <- nrow(data.frame(coefficients(Vrta)))
  SD.Vrta <- sd(res.Vrta$res)
  R2.Vrta <- 1-((n.Vrta-1)*(SSE.Vrta)/((n.Vrta-p.Vrta)*SSC.Vrta))
  RMSE.Vrta <- sqrt(SSE.Vrta/(nrow(res.Vrta)-p.Vrta))
  CV.Vrta <- (SD.Vrta/mean(res.Vrta$VT))*100
  S.Vrta <- sum(res.Vrta$res)/n.Vrta
  params <- c(coefficients(Vrta))
  R2 <- c(R2.Vrta) 
  RMSE <- c(RMSE.Vrta)
  CV <- c(CV.Vrta)
  S <- c(S.Vrta)
  params <- data.frame(rbind(params))
  Estadisticos <- rbind(R2, RMSE, CV, S)
return(Estadisticos)
}

#Estadisticos por modelo
Estad.ajus(Vrta_SH)
data.frame(Vrta_VC= Estad.ajus(Vrta_VC), Vrta_SH= Estad.ajus(Vrta_SH), Vrta_Sp= Estad.ajus(Vrta_Sp))

