
mat.par.1 <- matrix(c(1.8, .7, 1.8, 1.2, 1.2, .5, 1, 1, 1, -.5, .5, 0, 0.2, 0.2, .25, .2, 0.25, .25),nrow=6)
mat.par.2 <- matrix(c(2, .5, 1.5, 1.3, 1.1, .7, -1, 1, -1.5, .5, 1.5, 2, 0.2, 0.2, .25, .2, 0.25, .25),nrow=6)

theta <- seq(-4,4,0.01)

# Questão 1

mat.prob <- matrix(0,nrow(mat.par.1),length(theta))

# Gráfico das CCI's para o teste 1 

for (i in 1:nrow(mat.par.1)) {
  for (j in 1:length(theta))
    mat.prob[i,j] <- mat.par.1[i,3] + (1-mat.par.1[i,3])/(1+exp(-mat.par.1[i,1]*(theta[j]-mat.par.1[i,2])))
}

plot(theta,mat.prob[1,],type="l",ylim=c(0,1))
for (i in 2:nrow(mat.par.1))
  lines(theta,mat.prob[i,],lty=i)
legend(-4,1,c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6"), lty=c(1:6))

# Gráfico das CCI's para o teste 2

for (i in 1:nrow(mat.par.2)) {
  for (j in 1:length(theta))
    mat.prob[i,j] <- mat.par.2[i,3] + (1-mat.par.2[i,3])/(1+exp(-mat.par.2[i,1]*(theta[j]-mat.par.2[i,2])))
}

plot(theta,mat.prob[1,],type="l",ylim=c(0,1))
for (i in 2:nrow(mat.par.2))
  lines(theta,mat.prob[i,],lty=i)
legend(-4,1,c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6"), lty=c(1:6))

# d) 
vec.prob.1 <- mat.par.1[,3] + (1-mat.par.1[,3])/(1+exp(-mat.par.1[,1]*(0-mat.par.1[,2])))
vec.prob.1 
vec.prob.2 <- mat.par.2[,3] + (1-mat.par.2[,3])/(1+exp(-mat.par.2[,1]*(0-mat.par.2[,2])))
vec.prob.2 

# Questão 2

mat.prob <- mat.prob.dif <- mat.info <- matrix(0,nrow(mat.par.1),length(theta))

for (i in 1:nrow(mat.par.1)) {
  for (j in 1:length(theta)) {
    mat.prob[i,j] <- mat.par.1[i,3] + (1-mat.par.1[i,3])/(1+exp(-mat.par.1[i,1]*(theta[j]-mat.par.1[i,2])))
    mat.prob.dif[i,j] <- mat.par.1[i,1]*(1-mat.par.1[i,3])*exp(-mat.par.1[i,1]*(theta[j]-mat.par.1[i,2]))/
      ((1+exp(-mat.par.1[i,1]*(theta[j]-mat.par.1[i,2])))^2)
    mat.info[i,j] <- (mat.prob.dif[i,j]^2)/(mat.prob[i,j]*(1-mat.prob[i,j]))
  }
}

info.1 <- apply(mat.info,2,sum)

plot(theta,info.1,type="l")

for (i in 1:nrow(mat.par.2)) {
  for (j in 1:length(theta)) {
    mat.prob[i,j] <- mat.par.2[i,3] + (1-mat.par.2[i,3])/(1+exp(-mat.par.2[i,1]*(theta[j]-mat.par.2[i,2])))
    mat.prob.dif[i,j] <- mat.par.2[i,1]*(1-mat.par.2[i,3])*exp(-mat.par.2[i,1]*(theta[j]-mat.par.2[i,2]))/
      ((1+exp(-mat.par.2[i,1]*(theta[j]-mat.par.2[i,2])))^2)
    mat.info[i,j] <- (mat.prob.dif[i,j]^2)/(mat.prob[i,j]*(1-mat.prob[i,j]))
  }
}

info.2 <- apply(mat.info,2,sum)

lines(theta,info.2,lty=2)
legend(-4,1.5,c("Teste 1", "Teste 2"), lty=c(1,2))

# Questao 4

theta.Z <- 1
theta.Y <- -1

for (i in 1:6) {
  print(i)
  p.Z <- mat.par.1[i,3] + (1-mat.par.1[i,3])/(1+exp(-mat.par.1[i,1]*(theta.Z-mat.par.1[i,2])))
  print(p.Z)
  p.Y <- mat.par.1[i,3] + (1-mat.par.1[i,3])/(1+exp(-mat.par.1[i,1]*(theta.Y-mat.par.1[i,2])))
  print(p.Y)
  print(p.Z-p.Y)
}

for (i in 1:6) {
  print(i)
  p.Z <- mat.par.2[i,3] + (1-mat.par.2[i,3])/(1+exp(-mat.par.2[i,1]*(theta.Z-mat.par.2[i,2])))
  print(p.Z)
  p.Y <- mat.par.2[i,3] + (1-mat.par.2[i,3])/(1+exp(-mat.par.2[i,1]*(theta.Y-mat.par.2[i,2])))
  print(p.Y)
  print(p.Z-p.Y)
}









# Pacotes ----

# Análise ----

p_load(ggplot2,reshape2,gridExtra)

for (i in 1:nrow(mat.par.1)) {
  for (j in 1:length(theta))
    mat.prob[i,j] <- mat.par.1[i,3] + (1-mat.par.1[i,3])/(1+exp(-mat.par.1[i,1]*(theta[j]-mat.par.1[i,2])))
}

df1 <- data.frame(theta = theta, prob = mat.prob[1,], item = "Item 1")
df2 <- data.frame(theta = theta, prob = mat.prob[2,], item = "Item 2")
df3 <- data.frame(theta = theta, prob = mat.prob[3,], item = "Item 3")
df4 <- data.frame(theta = theta, prob = mat.prob[4,], item = "Item 4")
df5 <- data.frame(theta = theta, prob = mat.prob[5,], item = "Item 5")
df6 <- data.frame(theta = theta, prob = mat.prob[6,], item = "Item 6")

df <- rbind(df1, df2, df3, df4, df5, df6)

ggplot(df, aes(x = theta, y = prob, color = item)) +
  geom_line() +
  ylim(0, 1) +
  labs(title = "Teste 1",
       x = "Theta",
       y = "Probability") +
  theme_minimal() +
  scale_color_manual(values = c("Item 1" = "blue", "Item 2" = "red", "Item 3" = "green",
                                "Item 4" = "purple", "Item 5" = "orange", "Item 6" = "pink")) +
  theme(legend.position = "top")

# 2 ----

for (i in 1:nrow(mat.par.2)) {
  for (j in 1:length(theta))
    mat.prob[i,j] <- mat.par.2[i,3] + (1-mat.par.2[i,3])/(1+exp(-mat.par.2[i,1]*(theta[j]-mat.par.2[i,2])))
}

df1 <- data.frame(theta = theta, prob = mat.prob[1,], item = "Item 1")
df2 <- data.frame(theta = theta, prob = mat.prob[2,], item = "Item 2")
df3 <- data.frame(theta = theta, prob = mat.prob[3,], item = "Item 3")
df4 <- data.frame(theta = theta, prob = mat.prob[4,], item = "Item 4")
df5 <- data.frame(theta = theta, prob = mat.prob[5,], item = "Item 5")
df6 <- data.frame(theta = theta, prob = mat.prob[6,], item = "Item 6")

df <- rbind(df1, df2, df3, df4, df5, df6)

ggplot(df, aes(x = theta, y = prob, color = item)) +
  geom_line() +
  ylim(0, 1) +
  labs(title = "Teste 2",
       x = "Theta",
       y = "Probability") +
  theme_minimal() +
  scale_color_manual(values = c("Item 1" = "blue", "Item 2" = "red", "Item 3" = "green",
                                "Item 4" = "purple", "Item 5" = "orange", "Item 6" = "pink")) +
  theme(legend.position = "top")

# dois ----

df_info <- data.frame(theta = theta, info1 = info.1, info2 = info.2)

ggplot(df_info, aes(x = theta)) +
  geom_line(aes(y = info1, color = "Teste 1"), linetype = "solid") +
  geom_line(aes(y = info2, color = "Teste 2"), linetype = "dashed") +
  labs(title = "Information Plot", x = "Theta", y = "Information") +
  scale_color_manual(values = c("Teste 1" = "blue", "Teste 2" = "red")) +
  theme_minimal() +
  theme(legend.position = "top") +
  guides(color = guide_legend(title = "Tests", override.aes = list(linetype = c("solid", "dashed"))))



results_1 <- data.frame(Item = 1:6, p.Z = numeric(6), p.Y = numeric(6), p.diff = numeric(6))
results_2 <- data.frame(Item = 1:6, p.Z = numeric(6), p.Y = numeric(6), p.diff = numeric(6))

theta.Z <- 1
theta.Y <- -1

# Loop for mat.par.1
for (i in 1:6) {
  p.Z <- mat.par.1[i, 3] + (1 - mat.par.1[i, 3]) / (1 + exp(-mat.par.1[i, 1] * (theta.Z - mat.par.1[i, 2])))
  p.Y <- mat.par.1[i, 3] + (1 - mat.par.1[i, 3]) / (1 + exp(-mat.par.1[i, 1] * (theta.Y - mat.par.1[i, 2])))
  p.diff <- p.Z - p.Y
  results_1[i, ] <- c(i, p.Z, p.Y, p.diff)
}

# Loop for mat.par.2
for (i in 1:6) {
  p.Z <- mat.par.2[i, 3] + (1 - mat.par.2[i, 3]) / (1 + exp(-mat.par.2[i, 1] * (theta.Z - mat.par.2[i, 2])))
  p.Y <- mat.par.2[i, 3] + (1 - mat.par.2[i, 3]) / (1 + exp(-mat.par.2[i, 1] * (theta.Y - mat.par.2[i, 2])))
  p.diff <- p.Z - p.Y
  results_2[i, ] <- c(i, p.Z, p.Y, p.diff)
}

# Print the results data frames
print("Results for mat.par.1:")
print(results_1)

print("Results for mat.par.2:")
print(results_2)
