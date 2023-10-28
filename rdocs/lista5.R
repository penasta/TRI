source("rdocs/source/packages.r")
p_load(irtoys,mirt)

### Estimativas reais dos parametros dos itens utilizadas para simular os dados
#         a      b1    b2    b3
#Item 1 2.258 -0.347 0.527 1.515
#Item 2 2.019 -0.144 0.708 1.560
#Item 3 2.250  0.615 1.342 1.959
#Item 4 2.072  0.317 1.437 1.986
#Item 5 2.654  0.269 1.138 1.940
#Item 6 2.873 -0.321 0.444 1.452
#Item 7 3.475 -0.289 0.592 1.622
#Item 8 3.465 -0.489 0.303 1.210
#Item 9 2.949 -0.547 0.311 1.409


### Valores estimados dos parametros no modelo de resposta gradual
### para o questionario de hiperatividade em 
### Costa, M.C.M (2014) - Aplicando a Teoria de Resposta ao Item a 
### dados psicom�tricos, UFRJ.

a.par <- c(2.258,2.019,2.250,2.072,2.654,2.873,3.475,3.465,2.949)
b1.par <- c(-0.347,-0.144,0.615,0.317,0.269,-0.321,-0.289,-0.489,-0.547)
b2.par <- c(0.527,0.708,1.342,1.437,1.138,0.444,0.592,0.303,0.311)
b3.par <- c(1.515,1.560,1.959,1.986,1.940,1.452,1.622,1.210,1.409)
n.itens <- length(a.par)

#########################
### Gera��o dos dados ###
#########################

set.seed(2345) # semente

nr <- 1000 # numero de respondentes

### geracao das proficiencias

theta <- rnorm(nr,0,1)

par(mfrow=c(2,1))

### item "i"

i <- 4

### Curvas das probabilidades de cada categoria ou superior, P*_{ik}(theta), para cada item

plot(sort(theta),1/(1+exp(-a.par[i]*(sort(theta)-b1.par[i]))),type="l",ylab=c("Prob. da categoria ou superior"),
     xlab=c("theta"),main=c("Item ",i))
lines(sort(theta),1/(1+exp(-a.par[i]*(sort(theta)-b2.par[i]))))
lines(sort(theta),1/(1+exp(-a.par[i]*(sort(theta)-b3.par[i]))))

### Curvas das probabilidades das categorias para cada item

plot(sort(theta),1-1/(1+exp(-a.par[i]*(sort(theta)-b1.par[i]))),type="l",ylab=c("Prob. das categorias"),
     xlab=c("theta"),main=c("Item ",i))
lines(sort(theta),1/(1+exp(-a.par[i]*(sort(theta)-b1.par[i])))-1/(1+exp(-a.par[i]*(sort(theta)-b2.par[i]))))
lines(sort(theta),1/(1+exp(-a.par[i]*(sort(theta)-b2.par[i])))-1/(1+exp(-a.par[i]*(sort(theta)-b3.par[i]))))
lines(sort(theta),1/(1+exp(-a.par[i]*(sort(theta)-b3.par[i]))))


### geracao das respostas

resp <- matrix(0,nr,n.itens)

mat.prob <- matrix(0,n.itens,4)
for (j in 1:nr) {
  mat.prob <- cbind(rep(0,n.itens),exp(-a.par*(theta[j]-b1.par)),exp(-a.par*(theta[j]-b2.par)),exp(-a.par*(theta[j]-b3.par)))
  mat.prob <- 1/(1+mat.prob)
  mat.prob <- cbind(-t(apply(mat.prob,1,diff)),mat.prob[,4])
  for (i in 1:n.itens)
    resp[j,i] <- sample(4,1,replace=F,mat.prob[i,])
}

### ajuste do modelo de resposta gradual via "mirt" utilizando as respostas simuladas

write(t(resp),file="banco/dados.mrg.txt",ncol=n.itens)
resp <- read.table(file="banco/dados.mrg.txt")

mrg <- mirt(resp,1,itemtype=c('graded'))

prof.est <- fscores(mrg, full.scores=TRUE)
par.est <- coef(mrg,IRTpars=TRUE)

par(mfrow=c(1,1))

### gr�ficos dos parametros "reais" x estimados

mat.par.est <- matrix(0,9,4)
mat.par.est[1,] <- par.est$V1
mat.par.est[2,] <- par.est$V2
mat.par.est[3,] <- par.est$V3
mat.par.est[4,] <- par.est$V4
mat.par.est[5,] <- par.est$V5
mat.par.est[6,] <- par.est$V6
mat.par.est[7,] <- par.est$V7
mat.par.est[8,] <- par.est$V8
mat.par.est[9,] <- par.est$V9

plot(a.par,mat.par.est[,1],xlab=c("a"),ylab=c("a estimado"))
abline(0,1)

plot(b1.par,mat.par.est[,2],xlab=c("b1"),ylab=c("b1 estimado"))
abline(0,1)

plot(b2.par,mat.par.est[,3],xlab=c("b2"),ylab=c("b2 estimado"))
abline(0,1)

plot(b3.par,mat.par.est[,4],xlab=c("b3"),ylab=c("b3 estimado"))
abline(0,1)

## Gr�fico das profici�ncias estimadas x profici�ncias reais
plot(theta,prof.est)
abline(0,1)

resp[prof.est==min(prof.est),]

resp[prof.est==max(prof.est),]

## 6) geracao artificial de um numero maior de itens

n.it <- 50 # numero de itens simulados
a.par.sim <- runif(n.it,0.5,3)
b.par.sim <- matrix(rnorm(3*n.it,0,1),n.it,3)
b.par.sim <- t(apply(b.par.sim,1,sort))
b1.par.sim <- b.par.sim[,1]
b2.par.sim <- b.par.sim[,2]
b3.par.sim <- b.par.sim[,3]

theta.sim <- rnorm(nr,0,1)
resp.sim <- matrix(0,nr,n.it)

### geracao das respostas

mat.prob.sim <- matrix(0,n.it,4)
for (j in 1:nr) {
  mat.prob.sim <- cbind(rep(0,n.it),exp(-a.par.sim*(theta.sim[j]-b1.par.sim)),
                        exp(-a.par.sim*(theta.sim[j]-b2.par.sim)),exp(-a.par.sim*(theta.sim[j]-b3.par.sim)))
  mat.prob.sim <- 1/(1+mat.prob.sim)
  mat.prob.sim <- cbind(-t(apply(mat.prob.sim,1,diff)),mat.prob.sim[,4])
  for (i in 1:n.it)
    resp.sim[j,i] <- sample(4,1,replace=F,mat.prob.sim[i,])
}


write(t(resp.sim),file="banco/dados.mrg.txt",ncol=n.it)
resp.sim <- read.table(file="banco/dados.mrg.txt")

mrg.sim <- mirt(resp.sim,1,itemtype=c('graded'))

prof.est.sim <- fscores(mrg.sim, full.scores=TRUE)
par.est.sim <- coef(mrg.sim,IRTpars=TRUE)

plot(theta.sim,prof.est.sim)
abline(0,1)

############################################
##### Modelo de Escala Gradual
############################################

resp.grsm <- mirt(resp.sim,1,itemtype=c('grsm'))
prof.est.grsm <- fscores(resp.grsm, full.scores=TRUE)
par.est.grsm <- coef(resp.grsm)

############################################
##### Modelo de Credito Parcial
############################################

resp.gpcm <- mirt(resp.sim,1,itemtype=c('gpcmIRT'))
prof.est.gpcm <- fscores(resp.gpcm, full.scores=TRUE)
par.est.gpcm <- coef(resp.gpcm)

############################################
##### Modelo de Escala Nominal
############################################

resp.nsm <- mirt(resp.sim,1,itemtype=c('nominal'))
prof.est.nsm <- fscores(resp.nsm, full.scores=TRUE)
par.est.nsm <- coef(resp.nsm)
