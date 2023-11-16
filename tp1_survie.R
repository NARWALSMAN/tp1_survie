# Kaplan-Meier
library("survival")

#1 Entrez les donnees de Freireich
time<- c(6,6,6,6,7,9,10,10,11,13,16,17,19,20,22,23,25,32,32,34,35)
status<-c(1,1,1,0,1,0,1,0,0,1,1,0,0,0,1,1,0,0,0,0,0)
time0<-c(1, 1, 2, 2, 3, 4, 4, 5, 5, 8, 8, 8, 8, 11, 11, 12, 12, 15, 17, 22, 23)
status0<-rep(1,21)
duree<-c(time, time0)
censure<-c(status,status0)
groupe<-c(rep(1,length(time)),rep(0,length(time0)))

#2 Tracez l’estimateur de Kaplan-Meier
fit <- survfit(Surv(duree,censure) ~ 1,conf.type="plain")
plot(fit)

#3 Tracez l’estimateur de Kaplan-Meier
summary(fit)

#4 Refaites les mˆemes ´etapes par traitement
fit.groupe <- survfit(Surv(duree,censure) ~ groupe,conf.type="plain")
plot(fit.groupe)
summary(fit.groupe)