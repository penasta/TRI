# CAT ----
source("rdocs/source/packages.R")
p_load(catR)

n.itens <- 200 
theta <- -1
aa <- rlnorm(n.itens,0,0.5) 
bb <- rnorm(n.itens,0,1) 
cc <- rbeta(n.itens,80,320) 
dd <- rep(1,n.itens) 
mat.par <- cbind(aa,bb,cc,dd)
resultado <- randomCAT(theta,
                       mat.par,
                       test = list(method = "EAP", priorDist = "norm",priorPar = c(0, 1)),
                       stop = list(rule = "precision", thr = 0.3, alpha = 0.05) )

# plot(resultado$thetaProv,
#      type = "l",
#      xlab = "Número de itens",
#      ylab = "Estimativa da proficiência",
#      main = "Estimativa da proficiência em função do número de itens respondidos",
#      ylim = c(theta-2, theta+2),
#      xlim = c(2, length(resultado$thetaProv)))
# abline(h = theta,
#        col = "red")
# abline(h = resultado$thFinal,
#        col = "blue")
# legend("topleft",
#        legend = c("Proficiência verdadeira", "Proficiência estimada"),
#        col = c("red", "blue"),
#        lty = 1,
#        cex = 0.8)

n.itens2 <- 500 
theta2 <- -1
aa2 <- rlnorm(n.itens2,0,0.5) 
bb2 <- rnorm(n.itens2,0,1) 
cc2 <- rbeta(n.itens2,80,320) 
dd2 <- rep(1,n.itens2) 
mat.par2 <- cbind(aa2,bb2,cc2,dd2)
resultado2 <- randomCAT(theta2,
                       mat.par2,
                       test = list(method = "EAP", priorDist = "norm",priorPar = c(0, 1)),
                       stop = list(rule = "precision", thr = 0.3, alpha = 0.05))

# plot(resultado2$thetaProv,
#      type = "l",
#      xlab = "Número de itens",
#      ylab = "Estimativa da proficiência",
#      main = "Estimativa da proficiência em função do número de itens respondidos",
#      ylim = c(theta2-2, theta2+2),
#      xlim = c(2, length(resultado2$thetaProv)))
# abline(h = theta2,
#        col = "red")
# abline(h = resultado2$thFinal,
#        col = "blue")
# legend("topleft",
#        legend = c("Proficiência verdadeira", "Proficiência estimada"),
#        col = c("red", "blue"),
#        lty = 1,
#        cex = 0.8)

n.itens3 <- 1000 
theta3 <- -1
aa3 <- rlnorm(n.itens3,0,0.5) 
bb3 <- rnorm(n.itens3,0,1) 
cc3 <- rbeta(n.itens3,80,320) 
dd3 <- rep(1,n.itens3) 
mat.par3 <- cbind(aa3,bb3,cc3,dd3)
resultado3 <- randomCAT(theta3,
                       mat.par3,
                       test = list(method = "EAP", priorDist = "norm",priorPar = c(0, 1)),
                       stop = list(rule = "precision", thr = 0.3, alpha = 0.05))

# plot(resultado3$thetaProv,
#      type = "l",
#      xlab = "Número de itens",
#      ylab = "Estimativa da proficiência",
#      main = "Estimativa da proficiência em função do número de itens respondidos",
#      ylim = c(theta3-2, theta3+2),
#      xlim = c(2, length(resultado3$thetaProv)))
# abline(h = theta3,
#        col = "red")
# abline(h = resultado3$thFinal,
#        col = "blue")
# legend("topleft",
#        legend = c("Proficiência verdadeira", "Proficiência estimada"),
#        col = c("red", "blue"),
#        lty = 1,
#        cex = 0.8)

max_length <- max(length(resultado$thetaProv), length(resultado2$thetaProv), length(resultado3$thetaProv))
df <- data.frame(matrix(NA, nrow = max_length, ncol = 3))
df[1:length(resultado$thetaProv),1] <- resultado$thetaProv
df[1:length(resultado2$thetaProv),2] <- resultado2$thetaProv
df[1:length(resultado3$thetaProv),3] <- resultado3$thetaProv

plot(df$X1, type = "l", col = "blue",
     xlab = "Número de itens",
     ylab = "Estimativa da proficiência",
     main = "Estimativa da proficiência em função do número de itens respondidos",
     ylim = c(min(df, na.rm = T), max(df, na.rm = T)))
lines(df$X2, col = "green")
lines(df$X3, col = "orange")
abline(h = theta, col = "red", lty = 2)
legend("topright",
       legend = c("N = 200", "N = 500", "N = 1000", "Proficiência verdadeira"),
       col = c("blue", "green", "orange", "red"),
       lty = c(1, 1, 1, 2),
       cex = 0.8)

# DIF ----
p_load(ltm,irtoys,mirt,difR)
set.seed(73478)

D <- 1
n.resp <- 1000
n.itens <- 30
prop.0 <- 0.5 # proporcao dos respondentes no grupo 0

n0 <- n.resp*prop.0
n1 <- n.resp-n0

a0 <- 1.5
b0 <- -1
c0 <- 0.2

a1 <- 1.5
b1 <- 1
c1 <- 0.2

### geracao dos valores dos parametros dos demais itens (sem DIF)

aa <- rlnorm((n.itens-1),0,0.5) # discriminacao entre 0.3 e 2.5
bb <- rnorm((n.itens-1),0,1)
cc <- rbeta((n.itens-1),80,320) # "c" entre 0.15 e 0.25

aa <- c((a0*n0 + a1*n1)/n.resp, aa)
bb <- c((b0*n0 + b1*n1)/n.resp, bb)
cc <- c((c0*n0 + c1*n1)/n.resp, cc)

### geracao das proficiencias
theta <- rnorm(n.resp,0,1)

### geracao das respostas 
dados <- matrix(0,n.resp,n.itens)

### alocacao dos respondentes aos grupos 0 e 1
grupo.0.ind <- sample(c(1:n.resp),prop.0*n.resp)
grupo <- rep(1,n.resp)
grupo[grupo.0.ind] <- 0

p <- rep(0,n.resp)
p[grupo==0] <- c0 + (1-c0)/(1+exp(-D*a0*(theta[grupo==0]-b0)))
p[grupo==1] <- c1 + (1-c1)/(1+exp(-D*a1*(theta[grupo==1]-b1)))

p0 <- p[grupo==0]
theta0 <- theta[grupo==0]

p1 <- p[grupo==1]
theta1 <- theta[grupo==1]

dados[grupo==0,1] <- rbinom(n0,1,p0)
dados[grupo==1,1] <- rbinom(n1,1,p1)

#plot(sort(theta0),sort(p0),type="l")
#lines(sort(theta1),sort(p1),lty=2)

for (i in 2:n.itens) 
  dados[,i] <- rbinom(n.resp,1,cc[i]+(1-cc[i])/(1+exp(-D*aa[i]*(theta-bb[i]))))

## Estimacao das proficiencias via EAP

theta.est <- eap(dados,cbind(aa,bb,cc), qu=normal.qu())
theta.est <- theta.est[,1]
























############################################
### Aplicacao do teste de Mantel-Haenszel
############################################

p_value <- rep(0,n.itens)

dados = as.data.frame(dados) 
dados2 = cbind(dados,theta.est,grupo)

for (j in 1:n.itens) {
  
  # Divisão em 4 grupos para o cálculo do teste de Mantel-Haenszel
  
  n.part <- 4
  part <- c(-10,qnorm(c(1:(n.part-1))/n.part,0,1),10)
  Dados_MH <- array(rep(0,2*2*n.part),dim=c(2,2,n.part),
                    dimnames = list(
                      Pontuacao = c("Errado", "Certo"),
                      Grupo = c("Grupo_0", "Grupo_1"),
                      Nivel_proficiencia = c("1", "2", "3", "4")))
  
  dados.j <- cbind(dados[,j],theta.est,grupo)
  for (l in 1:n.part) {
    dados.j.aux <- dados.j[(dados.j[,2] > part[l]) & (dados.j[,2] <= part[l+1]),]
    Dados_MH[,,l] <- table(dados.j.aux[,1],dados.j.aux[,3])   
  }
  
  ######## Mantel-Haenszel
  
  Dados_MH[is.na(Dados_MH)] <- 0
  # Dados_MH
  # Classical Mantel-Haenszel test
  classico <- mantelhaen.test(Dados_MH)
  
  p_value[j]=classico$p.value
}

print(cbind(c(1:n.itens),round(p_value,digits=4)))

#######################################
### Regressão logística
###############################

dados2 <- cbind(dados,theta.est,grupo)
dados2 <- as.data.frame(dados2)

p_value_logist <- matrix(0,n.itens,2)

for (j in 1:n.itens) {
  
  logist.glm <- glm(dados2[,j] ~ dados2$theta.est + dados2$grupo + dados2$theta.est*dados2$grupo, family = binomial)
  # summary(logist.glm)
  
  saida <- anova(logist.glm, test="Chisq")
  # p-valor para o efeito do grupo (DIF uniforme) e 
  # p-valor para a interação theta*grupo (DIF não-uniforme)
  
  p_value_logist[j,1] <- saida$Pr[3]
  p_value_logist[j,2] <- saida$Pr[4]
  
}

print(cbind(c(1:n.itens),round(p_value_logist,digits=3)))
