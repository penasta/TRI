# altura<-read.fwf('http://www.ufpr.br/~aanjos/TRI/sinape/dados/altura211.dat',
# widths=c(3,4,rep(1,14)),header=FALSE,dec=',')

pacman::p_load(irtoys,ltm,mirt,CTT)

altura <- read.fwf(file="banco/aprender3.unb.br_pluginfile.php_2689255_mod_resource_content_1_altura211.txt",
widths=c(3,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1),dec=',')

head(altura)
tail(altura)

colnames(altura) <- c("id","altura",paste(c("i"),1:14,sep=""))
# insere nomes nas colunas
head(altura) # ver os 6 primeiros registros
class(altura) # tipo de objeto
altura.itens <- altura[,3:16] # utilizando apenas as colunas de respostas
altura.desc <- descript(altura.itens)
names(altura.desc)
altura.desc
plot(altura.desc,items=c(1,8:9),type="b", includeFirstLast=TRUE,pch=c("1","8","9"))
plot(altura.desc,type="b",includeFirstLast=TRUE)

# correlação ponto-bisserial entre o escore total e o item 1
biserial.cor(rowSums(altura.itens), altura.itens[[1]])

# Por padrão, a função biserial.cor() utiliza o valor 0 como referência.
# Para utilizar o valor 1, utilize o argumento level=2.
rho.PB <- biserial.cor(rowSums(altura.itens), altura.itens[[1]],level=2)

# Os valores da correlacao ponto bisserial para todos os itens esta' em altura.desc$bisCorr
altura.desc$bisCorr

# correlação bisserial
rho.B.vec <- rep(0,14)
for (i in 1:14) {
  pp <- colSums(altura.itens)[i]/nrow(altura.itens)
  rho.B <- sqrt(pp*(1-pp))*altura.desc$bisCorr[i]/dnorm(qnorm(pp,0,1),0,1)
  rho.B.vec[i] <- rho.B
}

plot(altura.desc$bisCorr, rho.B.vec, xlab=c("correlação ponto bisserial"),
ylab=c("correlação bisserial"),xlim=c(0,1),ylim=c(0,1))
abline(0,1)


# Coeficiente "alpha" de Cronbach
cronbach.alpha(altura.itens)
cronbach.alpha(altura.itens[-1]) # exclui o item 1
# para todos os itens
altura.desc$alpha

# Alternativamente, algumas estatisticas podem ser obtidas utilizando o pacote CTT

#altura.reliab <- reliability(altura.itens)
altura.reliab <- itemAnalysis(altura.itens)
names(altura.reliab)
altura.reliab$pBis

str(altura.reliab)

# Indice de dificuldade dos itens
1-apply(altura.itens,2,sum)/nrow(altura.itens)

# Coeficiente de discriminacao dos itens
escore <- apply(altura.itens,1,sum)
aux <- ceiling(0.27*nrow(altura.itens))
escore.inf <- sort(escore)[aux]
escore.sup <- sort(escore)[nrow(altura.itens)-aux]

altura.inf <- altura.itens[escore<=escore.inf,]
altura.sup <- altura.itens[escore>=escore.sup,]

apply(altura.sup,2,sum)/nrow(altura.sup)-apply(altura.inf,2,sum)/nrow(altura.inf)

# ---------------------------------------------------------------------------- #
source("rdocs/source/packages.R")
pacman::p_load(skimr)
skim(altura$altura)

df <- data.frame(statistic=numeric(14), p.value=numeric(14),id=numeric(14))
for (i in 1:14) {
  df[i,] <- cor.test(altura[[i+2]],altura$altura)[c("statistic","p.value")]
}
df <- round(df,3)
df$id <- 1:14
# Todos apresentam correlação positiva com p-valor <0,05; à exceção do item 1.


