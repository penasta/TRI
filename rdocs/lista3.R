source("rdocs/source/packages.R")

p_load(irtoys,ltm,mirt)

altura <- read.fwf(file="rdocs/aprender3.unb.br_pluginfile.php_2689255_mod_resource_content_1_altura211.txt",
                   widths=c(3,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1),dec=',')

### A funcao "tpm" do pacote "ltm" ajusta o modelo logistico de 1, 2 ou 3 parametros.
### No nosso caso, queremos ajustar o ML2. Isto e' feito com a restricao "constraint=cbind(1:no.item,1,0)"
### que impoe o valor 0 para o primeiro parametro (o pacote considera a ordem c,b,a, isto e',
### o primeiro parametro e' o de acerto ao acaso (c), o segundo e' o de dificuldade (b)
### e o terceiro e' o de discriminacao (a).

no.item <- ncol(altura[,3:16])
altura.tpm <- tpm(altura[,3:16],constraint=cbind(1:no.item,1,0))

### A funcao "coef" fornece as estimativas dos parametros dos itens.

par.est <- coef(altura.tpm) # cc, bb, aa

### estimacao da proficiencia (funcao "eap" serve apenas para o modelo 3PL)
### matriz com a estimativa da proficiencia e o erro-padrao (1/sqrt(informacao(theta.est)))

prof.est <- eap(altura[,3:16], cbind(par.est[,3],par.est[,2],par.est[,1]), qu=normal.qu())
theta.vec <- sort(prof.est[,1])

results <- data.frame(Proficiencia = theta.vec)
for (i in 1:no.item) {
  prob <- 1 / (1 + exp(-par.est[i, 3] * (theta.vec - par.est[i, 2])))
  results[paste("Item", i)] <- prob
}
results_long <- pivot_longer(results, cols = -Proficiencia, names_to = "Item", values_to = "Probabilidade")

ggplot(results_long, aes(x = Proficiencia, y = Probabilidade, color = Item)) +
  geom_line() +
  labs(
    x = "Proficiencia",
    y = "Probabilidade de resposta positiva",
    title = "Curva Caracteristica do Item (CCI) para Itens",
    color = "Item"
  ) +
  theme_minimal()

### Grafico das funcoes de informacao dos itens

mat.prob <- mat.prob.dif <- mat.info <- matrix(0,no.item,length(theta.vec))

for (i in 1:no.item) {
  for (j in 1:length(theta.vec)) {
    mat.prob[i,j] <- par.est[i,1] + (1-par.est[i,1])/(1+exp(-par.est[i,3]*(theta.vec[j]-par.est[i,2])))
    mat.prob.dif[i,j] <- par.est[i,3]*(1-par.est[i,1])*exp(-par.est[i,3]*(theta.vec[j]-par.est[i,2]))/
      ((1+exp(-par.est[i,3]*(theta.vec[j]-par.est[i,2])))^2)
    mat.info[i,j] <- (mat.prob.dif[i,j]^2)/(mat.prob[i,j]*(1-mat.prob[i,j]))
  }
}

### Grafico das funcoes de informacao dos itens

info_data <- data.frame(Proficiencia = theta.vec)
for (i in 1:no.item) {
  info_data[paste("Item", i)] <- mat.info[i, ]
}

info_data_long <- pivot_longer(info_data, cols = -Proficiencia, names_to = "Item", values_to = "Informacao")

ggplot(info_data_long, aes(x = Proficiencia, y = Informacao, color = Item)) +
  geom_line() +
  labs(
    x = "Proficiencia",
    y = "Informacao",
    title = "Funcoes de Informacao dos Itens",
    color = "Item"
  ) +
  theme_minimal()

### Grafico da funcao de informacao do teste

info_teste <- data.frame(Proficiencia = theta.vec, Informacao = apply(mat.info, 2, sum))

ggplot(info_teste, aes(x = Proficiencia, y = Informacao)) +
  geom_line() +
  labs(
    x = "Proficiencia",
    y = "Informacao",
    title = "Funcao de Informacao do Teste"
  ) +
  theme_minimal()

### estimacao da proficiencia (funcao "eap" do pacote "irtoys" serve apenas para o modelo 3PL)
### matriz com a estimativa da proficiencia e o erro-padrao (1/sqrt(informacao(theta.est)))

theta.est.eap <- eap(altura[3:16], cbind(par.est[,3],par.est[,2],par.est[,1]), qu=normal.qu())

### Transformacao linear da altura estimada com media e variancia iguais `a altura real.

theta.est <- mean(altura[,2]) + sd(altura[,2])*theta.est.eap[,1]

plot(altura[,2],theta.est,xlab=c("Altura real"),ylab=c("Altura estimada"))
abline(0,1)

cor(altura[,2],theta.est)

### Estimacao da altura via TCT

escore <- apply(altura[,3:16],1,sum)
escore.padr <- (escore-mean(escore))/sd(escore)
theta.est.tct <- mean(altura[,2]) + sd(altura[,2])*escore.padr

plot(theta.est.tct,theta.est,xlab=c("Altura estimada via TCT"),ylab=c("Altura estimada via TRI"))
abline(0,1)

cor(theta.est.tct,theta.est)

boxplot(cbind(theta.est.tct,theta.est))

