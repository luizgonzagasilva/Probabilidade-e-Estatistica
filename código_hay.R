library(readr)
require(survival)

dados =  read_delim("Bases/PANCAN_clinicalMatrix (1)", "\t", escape_double = FALSE, trim_ws = TRUE)

names(dados)

table(is.na(dados$`_TIME_TO_EVENT`))
table(is.na(dados$`_EVENT`))

dados2 = data.frame(primary_disease = dados$`_primary_disease`,time_to_event = dados$`_TIME_TO_EVENT`, event = dados$`_EVENT`)

dados2$time_to_event = dados2$time_to_event/365

fit = survfit(Surv(time_to_event, event)~ primary_disease,data=dados2)


dados_n1 = subset(dados2, dados2$primary_disease=="cholangiocarcinoma"|
                    dados2$primary_disease=="diffuse large B-cell lymphoma"|
                    dados2$primary_disease=="uterine carcinosarcoma"|
                    dados2$primary_disease=="uveal melanoma"|
                    dados2$primary_disease=="mesothelioma"|
                    dados2$primary_disease=="kidney chromophobe"|
                    dados2$primary_disease=="adrenocortical cancer"|
                    dados2$primary_disease=="thymoma"|
                    dados2$primary_disease=="testicular germ cell tumor")
labelsn1 =sort(c("cholangiocarcinoma",
                 "diffuse large B-cell lymphoma",
                 "uterine carcinosarcoma",
                 "uveal melanoma",
                 "mesothelioma",
                 "kidney chromophobe",
                 "adrenocortical cancer",
                 "thymoma",
                 "testicular germ cell tumor")    )  

dados_n2 = subset(dados2,dados2$primary_disease=="rectum adenocarcinoma"|
                    dados2$primary_disease=="acute myeloid leukemia"|
                    dados2$primary_disease=="pheochromocytoma & paraganglioma"|
                    dados2$primary_disease=="pancreatic adenocarcinoma"|
                    dados2$primary_disease=="esophageal carcinoma"|
                    dados2$primary_disease=="sarcoma"|
                    dados2$primary_disease=="cervical & endocervical cancer"|
                    dados2$primary_disease=="kidney papillary cell carcinoma")
labelsn2 = sort(c("rectum adenocarcinoma",
               "acute myeloid leukemia",
               "pheochromocytoma & paraganglioma",
               "pancreatic adenocarcinoma",
               "esophageal carcinoma",
               "sarcoma",
               "cervical & endocervical cancer",
               "kidney papillary cell carcinoma"))

dados_n3 = subset(dados2, dados2$primary_disease=="bladder urothelial carcinoma"|
                    dados2$primary_disease=="liver hepatocellular carcinoma"|
                    dados2$primary_disease=="skin cutaneous melanoma"|
                    dados2$primary_disease=="stomach adenocarcinoma"|
                    dados2$primary_disease=="colon adenocarcinoma"|
                    dados2$primary_disease=="brain lower grade glioma"|
                    dados2$primary_disease=="prostate adenocarcinoma"|
                    dados2$primary_disease=="thyroid carcinoma")
                    
labelsn3 = sort(c("bladder urothelial carcinoma",
               "liver hepatocellular carcinoma",
               "skin cutaneous melanoma",
               "stomach adenocarcinoma",
               "colon adenocarcinoma",
               "brain lower grade glioma",
               "prostate adenocarcinoma",
               "thyroid carcinoma"))

dados_n4 = subset(dados2, dados2$primary_disease=="uterine corpus endometrioid carcinoma"|
                    dados2$primary_disease=="glioblastoma multiforme"|
                    dados2$primary_disease=="ovarian serous cystadenocarcinoma"|
                    dados2$primary_disease=="head & neck squamous cell carcinoma"|
                    dados2$primary_disease=="lung squamous cell carcinoma"|
                    dados2$primary_disease=="lung adenocarcinoma"|
                    dados2$primary_disease=="kidney clear cell carcinoma"|
                    dados2$primary_disease=="breast invasive carcinoma")
labelsn4 = sort(c("uterine corpus endometrioid carcinoma",
               "glioblastoma multiforme",
               "ovarian serous cystadenocarcinoma",
               "head & neck squamous cell carcinoma",
               "lung squamous cell carcinoma",
               "lung adenocarcinoma",
               "kidney clear cell carcinoma",
               "breast invasive carcinoma")        ) 
                    



fit1 = survfit(Surv(time_to_event, event)~ primary_disease,data=dados_n1)
fit2 = survfit(Surv(time_to_event, event)~ primary_disease,data=dados_n2)
fit3 = survfit(Surv(time_to_event, event)~ primary_disease,data=dados_n3)
fit4 = survfit(Surv(time_to_event, event)~ primary_disease,data=dados_n4)


setwd("C:/Users/gleici/Google Drive/Genetica-dados-sobrevida")

png("Survival_n1.png",width = 800,height = 400)
par(xpd = T, mar = c(4,4,2,15),lty=1)
plot(fit1,xlab="Time(years)",ylab="S(t)",conf.int = F,col = rainbow(10,s = 0.5),lwd=2)
legend(21,1, legend=labelsn1, lty=1,col = rainbow(10,s = 0.5),lwd=2, bty="n" )
dev.off()

png("Survival_n2.png",width = 800,height = 400)
par(xpd = T, mar = c(4,4,2,15),lty=1)
plot(fit2,xlab="Time(years)",ylab="S(t)",conf.int = F,col = rainbow(10,s = 0.5),lwd=2)
legend(27,1, legend=labelsn2, lty=1,col = rainbow(10,s = 0.5),lwd=2, bty="n" )
dev.off()

png("Survival_n3.png",width = 800,height = 400)
par(xpd = T, mar = c(4,4,2,15),lty=1)
plot(fit3,xlab="Time(years)",ylab="S(t)",conf.int = F,col = rainbow(10,s = 0.5),lwd=2)
legend(32,1, legend=labelsn3, lty=1,col = rainbow(10,s = 0.5),lwd=2, bty="n" )
dev.off()

png("Survival_n4.png",width = 800,height = 400)
par(xpd = T, mar = c(4,4,2,15),lty=1)
plot(fit4,xlab="Time(years)",ylab="S(t)",conf.int = F,col = rainbow(10,s = 0.5),lwd=2)
legend(24.5,1, legend=labelsn4, lty=1,col = rainbow(10,s = 0.5),lwd=2, bty="n" )
dev.off()

dadosteste = subset( dados2,  dados2$primary_disease=="kidney clear cell carcinoma")



#--------------------------------------------------------------#
#                 EXEMPLO CV
#--------------------------------------------------------------#



dados_ex = subset(dados2, dados2$primary_disease=="glioblastoma multiforme"|
                    dados2$primary_disease=="breast invasive carcinoma"|
                  dados2$primary_disease=="ovarian serous cystadenocarcinoma")

labels_ex = sort(c("glioblastoma multiforme",
                  "breast invasive carcinoma",
                  "ovarian serous cystadenocarcinoma") )

fit_ex = survfit(Surv(time_to_event, event) ~ primary_disease,data=dados_ex)
plot(fit_ex)

setwd("C:\\Users\\User\\Google Drive (hayalaccs@usp.br)\\Aula curso de verão - Gen\\2019\\sobrevida")

png("Exemplo_R.png",width = 800,height = 400)
par(xpd = T, mar = c(4,4,2,15),lty=1)
plot(fit_ex,xlab="Time(years)",ylab="S(t)",conf.int = F,col = rainbow(10,s = 0.5),lwd=2)
legend(24.5,1, legend=labels_ex, lty=1,col = rainbow(10,s = 0.5),lwd=2, bty="n" )
dev.off()


# Modelo de COX

dados_ex$primary_disease = factor(dados_ex$primary_disease)

fit_cox = coxph(Surv(time_to_event, event) ~ primary_disease,data=dados_ex)

summary(fit_cox)


