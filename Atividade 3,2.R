library(infer)
library(EnvStats)
library(tidyverse)

dados <- amostra_190090090

# AMOSTRAS ----
# n = 30
amostras30 <- dados %>%
  rep_sample_n(size = 30, reps = 1, replace = TRUE)
# n = 100
amostras100 <- dados %>%
  rep_sample_n(size = 100, reps = 1, replace = TRUE)


# QUESTÃO 1 ----
# Nota LP
hist(amostras100$NOTA_LP,ylab="Proporção",xlab="Nota LP",
     main="Nota Lingua Portuguesa", freq = F,col="#054F77",xlim=c(100,350))
lines(density(amostras100$NOTA_LP))

d <- amostras100$NOTA_LP
brk <-  seq(100,340,20) 
f_obs <- data.frame(table(cut(d,breaks=brk,right=FALSE)))
f_esp = c(1.61,2.89,6.08,10.52,14.96,17.50,16.83,13.30,8.65,4.62,2.03,1.01)  

teste_quiquad<-function(f_obs,f_esp,alpha,gl){
  q_obs=sum((f_obs-f_esp)^2/f_esp)
  q_c=qchisq(1-alpha,df=gl)
  p_val=pchisq(q_obs,df=gl,lower.tail = FALSE)
  writeLines("                  *** Teste de Qui-quadrado ***  ")
  writeLines(paste(" Estatística observada: ",round(q_obs,2),"  ",sep=""))
  writeLines(paste("    Graus de liberdade: ", gl,"  ",sep=""))
  writeLines(paste("         Valor crítico: ",round(q_c,2),"  ",sep=""))
  writeLines(paste("               Valor-p: ",round(p_val,4),"  ",sep=""))
  writeLines(paste("Nível de significância: ",alpha,"  ",sep=""))
  if(q_obs>q_c){
    writeLines("        **Rejeito H0**")
  } else writeLines("         **Aceito H0**")
}

teste_quiquad(f_obs,f_esp,0.05,9)


# QUESTÃO 2 ----
# Lingua Portuguesa ----
### Shapiro-Wilk
gofTest(amostras30$NOTA_LP, test = "sw")

### Anderson Darling 
gofTest(amostras30$NOTA_LP, test = "ad")

### Lillie Test 
gofTest(amostras30$NOTA_LP, test = "lillie", distribuition = "norm") 


# Matematica ----
### Shapiro-Wilk
gofTest(amostras30$NOTA_MT, test = "sw")

### Anderson Darling 
gofTest(amostras30$NOTA_MT, test = "ad")

### Lillie Test 
gofTest(amostras30$NOTA_MT, test = "lillie", distribuition = "norm") 



