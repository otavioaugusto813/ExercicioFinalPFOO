# Atividade Final PFOO
# Otávio Augusto Alves Coelho


install.packages('ISLR')
install.packages('dplyr')
install.packages('descr')
library(descr)
library(ISLR)
library(dplyr)

data("Hitters")
head(Hitters)

# 1- tirando estatísticas descritivas
summary(Hitters)

# desvio padrão de todas as variáveis
apply(Hitters, 2, sd, na.rm = T)
#  ou
sapply(Hitters[c(1:13, 16:19)], sd, na.rm = T)

# tabela de frequência das variáveis categóricas
freq(Hitters$League, p = F)
freq(Hitters$Division, p = F)

# 2 - Valores missing
sapply(Hitters, function(x) sum(is.na(x)))


### 3 - Crie uma variável categórica chamada BigHitter com a seguinte
# lógica: se o batedor bateu mais que a média, marque Yes. Senão, 
# marque No.

head(Hitters)
str(Hitters)
media = mean(Hitters$Hits)

Hitters$BigHitter = Hitters$Hits > media
Hitters$BigHitter = factor(Hitters$BigHitter, levels=c(F,T), labels=c('No', 'Yes'))
head(Hitters)


# 4 - Crie uma variável chamada LnSal como o logaritmo natural do salário

LnSal = log(Hitters$Salary)
  
# 5 - Elabore um modelo de regressão linear para explicar LnSal usando 
# as seguintes variáveis como preditores: Hits, HmRun, Years, League
# e Division

y = lm(LnSal ~ Hits + HmRun + Years + League + Division, data=Hitters)
summary(y)

# como se observa, apenas HmRun não tem valor explicativo para a variável LnSal, que é
# o logaritmo natural do salário, já que seu p-valor está acima de 0.05.

# 6 - Usando um for loop, tire a média e o desvio padrão de todas as
# variáveis numéricas do banco

medias = c()
desvios = c()
for (i in 1:20){
  medias[i] = mean(Hitters[[i]], na.rm = T)
  desvios = sd(Hitters[[i]], na.rm = T)
}

medias
desvios

# Repita o exercício 6 usando functionals (apply ou map)

apply(Hitters, 2, sd)
sapply(Hitters, mean)

