saresp <- read.table(file="banco/saresp.txt")
gab2 <- read.table(file="banco/saresp-gabarito.txt")
 
dados <- saresp

manha <- dados[dados[,4]=="m07",]
tarde <- dados[dados[,4]=="t07",]
noite <- dados[dados[,4]=="n07",]

gab3 <- matrix(9,nrow(gab2),ncol(gab2))

for (i in 1:nrow(gab3)) {
  for (j in 1:ncol(gab3)) {
    if (gab2[i,j]=="A") gab3[i,j] <- 1
    if (gab2[i,j]=="B") gab3[i,j] <- 2
    if (gab2[i,j]=="C") gab3[i,j] <- 3
    if (gab2[i,j]=="D") gab3[i,j] <- 4
  }
}

resp.manha <- manha[,5:34]
resp.manha <- as.matrix(resp.manha)
resp.m <- matrix(9,nrow(resp.manha),ncol(resp.manha))
resp.m[resp.manha=="A"] <- 1
resp.m[resp.manha=="B"] <- 2
resp.m[resp.manha=="C"] <- 3
resp.m[resp.manha=="D"] <- 4

for (i in 1:nrow(resp.m)) {
  for (j in 1:ncol(resp.m)) {
    if ((resp.m[i,j]!=gab3[1,j])&&(resp.m[i,j]!=9)) resp.m[i,j] <- 0 
    if (resp.m[i,j]==gab3[1,j]) resp.m[i,j] <- 1 
  }
}

resp.m[resp.m==9] <- NA

resp.tarde <- tarde[,5:34]
resp.tarde <- as.matrix(resp.tarde)
resp.t <- matrix(9,nrow(resp.tarde),ncol(resp.tarde))
resp.t[resp.tarde=="A"] <- 1
resp.t[resp.tarde=="B"] <- 2
resp.t[resp.tarde=="C"] <- 3
resp.t[resp.tarde=="D"] <- 4

for (i in 1:nrow(resp.t)) {
  for (j in 1:ncol(resp.t)) {
    if ((resp.t[i,j]!=gab3[2,j])&&(resp.t[i,j]!=9)) resp.t[i,j] <- 0 
    if (resp.t[i,j]==gab3[2,j]) resp.t[i,j] <- 1 
  }
}

resp.t[resp.t==9] <- NA


resp.noite <- noite[,5:34]
resp.noite <- as.matrix(resp.noite)
resp.n <- matrix(9,nrow(resp.noite),ncol(resp.noite))
resp.n[resp.noite=="A"] <- 1
resp.n[resp.noite=="B"] <- 2
resp.n[resp.noite=="C"] <- 3
resp.n[resp.noite=="D"] <- 4

for (i in 1:nrow(resp.n)) {
  for (j in 1:ncol(resp.n)) {
    if ((resp.n[i,j]!=gab3[3,j])&&(resp.n[i,j]!=9)) resp.n[i,j] <- 0 
    if (resp.n[i,j]==gab3[3,j]) resp.n[i,j] <- 1 
  }
}

resp.n[resp.n==9] <- NA



library(irtoys)
library(ltm)

###### Obtencao das estimativas dos parametros dos itens para turno da manha via pacote "irtoys"

#resp.m.tpm <- tpm(resp.m,constraint=cbind(1:ncol(resp.m),1,0.25))
resp.m.tpm <- tpm(resp.m)
par.m.est <- coef(resp.m.tpm) # cc, bb, aa

### estimacao da proficiencia (funcao "eap" serve apenas para o modelo 3PL)
### matriz com a estimativa da proficiencia e o erro-padr?o (1/sqrt(informacao(theta.est)))

theta.m.est <- eap(resp.m, cbind(par.m.est[,3],par.m.est[,2],par.m.est[,1]), qu=normal.qu())
prof.m.est <- theta.m.est[,1]

###### Obtencao das estimativas dos parametros dos itens para turno da tarde via pacote "irtoys"

#resp.t.tpm <- tpm(resp.t,constraint=cbind(1:ncol(resp.t),1,0.25))
resp.t.tpm <- tpm(resp.t)
par.t.est <- coef(resp.t.tpm) # cc, bb, aa

### estimacao da proficiencia (funcao "eap" serve apenas para o modelo 3PL)
### matriz com a estimativa da proficiencia e o erro-padr?o (1/sqrt(informacao(theta.est)))

theta.t.est <- eap(resp.t, cbind(par.t.est[,3],par.t.est[,2],par.t.est[,1]), qu=normal.qu())
prof.t.est <- theta.t.est[,1]

###### Obtencao das estimativas dos parametros dos itens para turno da noite via pacote "irtoys"

#resp.n.tpm <- tpm(resp.n,constraint=cbind(1:ncol(resp.n),1,0.25))
resp.n.tpm <- tpm(resp.n)
par.n.est <- coef(resp.n.tpm) # cc, bb, aa

### estimacao da proficiencia (funcao "eap" serve apenas para o modelo 3PL)
### matriz com a estimativa da proficiencia e o erro-padr?o (1/sqrt(informacao(theta.est)))

theta.n.est <- eap(resp.n, cbind(par.n.est[,3],par.n.est[,2],par.n.est[,1]), qu=normal.qu())
prof.n.est <- theta.n.est[,1]


########## Equalizacao via regress?o linear

# Equaliza??o 
# plot(par.n.est[15:19,2],par.m.est[15:19,2])
# reg.nm.b <- lm(par.m.est[15:19,2]~par.n.est[15:19,2])
# abline(reg.nm.b$coefficients[1],reg.nm.b$coefficients[2])

library(ggplot2)
ggplot(data.frame(x=par.n.est[15:19,2],y=par.m.est[15:19,2]),aes(x=x,y=y)) +
  geom_point() +
  geom_smooth(method="lm") +
  coord_cartesian(xlim=c(-0.2,1.9),ylim=c(0,2.3))

prof.nm.b <- reg.nm.b$coefficients[2]*prof.n.est + reg.nm.b$coefficients[1]

# plot(prof.n.est,prof.nm.b)
# abline(0,1)

ggplot(data.frame(x=prof.n.est,y=prof.nm),aes(x=x,y=y)) +
  geom_point(color = cores_estat[2]) +
  geom_abline(intercept = 0, slope = 1, color = cores_estat[1]) +
  coord_cartesian(xlim=c(-2,3),ylim=c(-2,3))

########## Equalizacao pelo metodo media-desvio

alfa <- sd(par.m.est[15:19,2])/sd(par.n.est[15:19,2])

beta <- mean(par.m.est[15:19,2])-alfa*mean(par.n.est[15:19,2])

## proficiencia dos alunos da noite na escala da manha

prof.nm <- alfa*prof.n.est+beta
# plot(prof.n.est,prof.nm)
# abline(0,1)
ggplot(data.frame(x=prof.n.est,y=prof.nm),aes(x=x,y=y)) +
  geom_point() +
  geom_smooth(method="lm") +
  coord_cartesian(xlim=c(-2,3),ylim=c(-2,3))


