
rm(list = ls())
library(tidyverse)

# 1
dados <- read.table("dados_L2_MLG.txt", skip = 1, col.names = c("estado", "y", "x1")) %>% select(-estado) %>% as_tibble()
dados %>% head()

## b)
m1 <- glm(formula = y ~ x1, family = Gamma(link="log"), data = dados)
resumo <- m1 %>% summary()

coef <- coef(m1)
coef
desvio <- resumo$coefficients[,2]
desvio

## c)
qt <- qt(1 - 0.05/2, df = nrow(dados) - ncol(dados)) # quantil da T
erro =  qt * desvio
IC <- list(LI = coef(m1) - erro,
     LS = coef(m1) + erro) %>% data.frame()
row.names(IC) <- c("Intercepto", "x1")
IC %>% round(5)

## d)
alpha <- resumo$dispersion
alpha

## e) 
m1$fitted.values

## h)
beta0_chapeu = coef[1]
beta1_chapeu = coef[2]
mu = exp(beta0_chapeu + beta1_chapeu*dados$x1)
de = 2*alpha*sum(log(mu/dados$y)-((dados$y-mu)/mu))
de # tinha que ser o mesmo da saida do summary
resumo$deviance

# 2
y <- c(175, 108, 95, 82, 71, 50, 49, 31, 28, 17, 16, 11)
x1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

