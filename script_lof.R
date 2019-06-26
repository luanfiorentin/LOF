


# Pacotes -----------------------------------------------------------------

library(alr3)
library(ggplot2)



# Setando uma semente
set.seed(2019)


# Exemplo 1 ---------------------------------------------------------------
# Simulando valores de X
n <- 10
x <- c(rep(c(20, 22, 24, 26, 28, 30), each = n))

# Simulando valores de y
y <- (0.2 + 0.1*x + 0.001*x^2 + 0.008*x^3) + rnorm(n = (length(unique(x))*n), 
                         mean = 0, sd = 3)

# Organizando os dados
da <- data.frame("y" = y, 
                 "x" = x)


# Ajustando o modelo
fit1 <- lm(y ~ x, data = da)
summary(fit1)

# Gráfico de dispersão
plot(y ~ x, data = da, 
     col = "black", 
     pch = 1, 
     cex = 1.2)
abline(fit1, 
       col = "blue", 
       lwd = 3)
medias <- aggregate(da$y, by = list(da$x), FUN = mean)
points(medias[,1], medias[,2], 
       col = "red", 
       lwd = 2, 
       pch = 4)

### Teste de falta de ajuste
# H0: Não há falta de ajuste, ou seja, 
# modelo linear é adequado.
pureErrorAnova(fit1)

# Se o modelo está bem ajustado, então
# a média de y_i para um valor fico de x_i
# deve ficar próxima do valor predito.
# Essa distância é dada pela falta de ajuste.

# A distância entre valores observados de y_i
# e a respectiva média em x_i estimada pelo modelo 
# é chamada de erro puro.

# Então, conclusão do teste: p-valor < 5%.
# Logo, rejeita-se a H0 ao nível de 5% e 
# conclui-se que há falta de ajuste.
# Em outras palavras, a maior parte da 
# variabilidade (SQres) é devido a falta de ajuste.
# Relação entre as variáveis parece não ser linear.





# Exemplo 2 ---------------------------------------------------------------
# Simulando valores de X
n <- 10
x <- c(rep(c(20, 22, 24, 26, 28, 30), each = n))

# Simulando valores de y
y <- (2 + 5*x) + rnorm(n = (length(unique(x))*n), 
                         mean = 0, sd = 3)

# Organizando os dados
da <- data.frame("y" = y, 
                 "x" = x)


# Ajustando o modelo
fit2 <- lm(y ~ x, data = da)
summary(fit2)

# Gráfico de dispersão
plot(y ~ x, data = da, 
     col = "black", 
     pch = 1, 
     cex = 1.2)
abline(fit2, 
       col = "blue", 
       lwd = 3)
medias <- aggregate(da$y, by = list(da$x), FUN = mean)
points(medias[,1], medias[,2], 
       col = "red", 
       lwd = 2, 
       pch = 4)

# Teste de falta de ajuste
pureErrorAnova(fit2)
# Então, conclusão do teste: p-valor >= 5%.
# Logo, não rejeita-se a H0 ao nível de 5% e 
# conclui-se que não há falta de ajuste.
# Relação entre as variáveis parece ser linear.





# Exemplo -----------------------------------------------------------------

# Carregando os dados
da <- read.table(file = "dados.txt", 
                 header = TRUE, 
                 sep = "\t", 
                 dec = ",")
str(da)
head(da)

age18 <- da[da$idade == 18, ]
age25 <- da[da$idade == 25, ]
age30 <- da[da$idade == 30, ]

# Ajustando o modelo
fit18 <- lm(hdom ~ (IS), data = age18)
summary(fit18)
fit25 <- lm(hdom ~ (IS), data = age25)
summary(fit25)
fit30 <- lm(hdom ~ (IS), data = age30)
summary(fit30)

# Anova para falta de ajuste
pureErrorAnova(fit18)
pureErrorAnova(fit25)
pureErrorAnova(fit30)


# Gráficos
par(mfrow = c(1,3))

# 18 anos
plot(hdom ~ IS, data = age18, 
     main = "18 anos", 
     col = "black", 
     pch = 1, 
     cex = 2)
abline(fit18, 
       col = "blue", 
       lwd = 5)
(medias18 <- aggregate(age18$hdom, by = list(age18$IS), FUN = mean))
(tam18 <- aggregate(age18$hdom, by = list(age18$IS), FUN = length))
points(medias18[,1], medias18[,2], 
       col = "red", 
       lwd = 5, 
       pch = 4)

# 25 anos
plot(hdom ~ IS, data = age25, 
     main = "25 anos", 
     col = "black", 
     pch = 1, 
     cex = 2)
abline(fit25, 
       col = "blue", 
       lwd = 5)
(medias25 <- aggregate(age25$hdom, by = list(age25$IS), FUN = mean))
(tam25 <- aggregate(age25$hdom, by = list(age25$IS), FUN = length))
points(medias25[,1], medias25[,2], 
       col = "red", 
       lwd = 5, 
       pch = 4)

# 30 anos
plot(hdom ~ IS, data = age30, 
     main = "30 anos", 
     col = "black", 
     pch = 1, 
     cex = 2)
abline(fit30, 
       col = "blue", 
       lwd = 5)
(medias30 <- aggregate(age30$hdom, by = list(age30$IS), FUN = mean))
(tam30 <- aggregate(age30$hdom, by = list(age30$IS), FUN = length))
points(medias30[,1], medias30[,2], 
       col = "red", 
       lwd = 5, 
       pch = 4)

par(mfrow = c(1,1))





# Ajuste considerando todas as idades
# Ajustando o modelo
fitall <- lm(hdom ~ (IS), data = da)
summary(fitall)

# Anova para falta de ajuste
pureErrorAnova(fitall)



# Gráfico 1
plot(hdom ~ IS, data = da, 
     main = "Todas as Idades", 
     col = "black", 
     pch = 1, 
     cex = 2)
abline(fitall, 
       col = "blue", 
       lwd = 5)
(mediasall <- aggregate(da$hdom, by = list(da$IS), FUN = mean))
(tamall <- aggregate(da$hdom, by = list(da$IS), FUN = length))
points(mediasall[,1], mediasall[,2], 
       col = "red", 
       lwd = 5, 
       pch = 4)



# Gráfico 2
ggplot(data = da, mapping = aes(x = IS, y = hdom, 
                                colour = as.factor(idade))) + 
        geom_point(size = 3) + 
        geom_smooth(formula = y ~ x, method = "lm")

# 
plot(hdom ~ IS, data = da, 
     main = "Todas as Idades", 
     col = "black", 
     pch = 1, 
     cex = 2)
abline(fitall, 
       col = "blue", 
       lwd = 5)
abline(fit18, 
       col = "green", 
       lwd = 2)
abline(fit25, 
       col = "yellow", 
       lwd = 2)
abline(fit30, 
       col = "red", 
       lwd = 2)
(mediasall <- aggregate(da$hdom, by = list(da$IS), FUN = mean))
(tamall <- aggregate(da$hdom, by = list(da$IS), FUN = length))
points(mediasall[,1], mediasall[,2], 
       col = "red", 
       lwd = 5, 
       pch = 4)
