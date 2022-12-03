library(data.table)
library(ggplot2)
library(DataExplorer)
library(visreg)

tamanho_da_amostra <- 1000
Base <- read.csv('/Users/jessicasouza/Downloads/salt.csv')
Base <- Base[0:tamanho_da_amostra, c('T_degC','Salnty','Depthm')]

# Visualizar Dataframe
View(Base)
DataExplorer::create_report(Base)

# Remover linhas com nulls
Base <- na.omit(Base)

# Gráfico
plot(Base, main="Dataset Scatterplot")

analisador <- function(x, y) {
  print(cor.test(x, y, conf.level=0.95))
  x_str <- deparse(substitute(x))
  y_str <- deparse(substitute(y))
  plot(
    x,
    y,
    main="Modelos sobre Dados",
    xlab=x_str,
    ylab=y_str
  )
  
  # Modelo Liear
  modelo_linear <- lm(y ~ x)
  print(">>>>>>>>>>>> Coeficientes do modelo linear <<<<<<<<<<<<<<<")
  print(coef(modelo_linear))
  abline(modelo_linear)
  print(">>>>>>>>>>>> Outras informações do modelo linear <<<<<<<<<<<<<<<")
  print(summary(modelo_linear))
  
  # Modelo polinomial
  modelo_poly = lm(y ~ poly(x, 3))
  print(">>>>>>>>>>>> Coeficientes do modelo polinomial <<<<<<<<<<<<<<<")
  print(coef(modelo_poly))
  sorted_index <- sort(x, index.return=T)$ix
  lines(
    x[sorted_index],
    predict(modelo_poly)[sorted_index],
    col='red',
    lwd=2
  )
  print(">>>>>>>>>>>> Outras informações do modelo polinomial <<<<<<<<<<<<<<<")
  print(summary(modelo_poly))
  
  # Análise de resíduos
  plot(
    predict(modelo_linear),
    rstandard(modelo_linear),
    main=paste("Análise de Resíduos Modelo Linear para ", x_str),
  )
  plot(
    predict(modelo_poly),
    rstandard(modelo_poly),
    main=paste("Análise de Resíduos Modelo Polinomial para ", x_str),
  )
}

"------------------"
"Analisando Salnty em relação a T_degC"
"------------------"
analisador(x=Base$Salnty, y=Base$T_degC)

"------------------"
"Analisando Depthm em relação a T_degC"
"------------------"
analisador(x=Base$Depthm, y=Base$T_degC)


analisador(y=Base$Depthm, x=Base$T_degC)

ml <- lm(T_degC ~ Salnty, data=Base)

## Regressão multipla

model <- lm(T_degC ~ Salnty + Depthm, data=Base)
coef(model)

# Análise de Residuos fitted
ggplot(data = model) + 
  geom_point(aes(x=.fitted, 
                 y=.stdresid)) +
  geom_hline(yintercept = 0) +
  labs(x = 'Valores preditos', 
       y = 'Resíduos padronizados') + 
  theme_minimal()

ggplot(data = model, 
       aes(sample = .stdresid)) + 
  stat_qq() + 
  stat_qq_line() + 
  labs(x = 'Valores esperados pela normal', 
       y = 'Resíduos padronizados') +
  theme_minimal()

ggplot(data = model) + 
  geom_histogram(aes(x = .stdresid),
                 bins = 5,
                 fill = 'lightgrey',
                 colour = 'black') + 
  labs(x = 'Resíduos padronizados', 
       y = 'Frequência') +
  theme_minimal()

# TESTE DE SHAPIRO-WILK
shapiro.test(rstandard(model))

# Variável explicativa Salnty versus Resíduos padronizados
ggplot(data = model) + 
  geom_point(aes(x=Base$Salnty, y=.stdresid)) +
  geom_hline(yintercept = 0) +
  labs(x = 'Salnty', y = 'Resíduos padronizados') + 
  theme_minimal()

# Variável explicativa Depthm versus Resíduos padronizados
ggplot(data = model) + 
  geom_point(aes(x=Base$Depthm, y=.stdresid)) +
  geom_hline(yintercept = 0) +
  labs(x = 'Salnty', y = 'Resíduos padronizados') + 
  theme_minimal()

summary(model)

# Intervalo de confiança e de predição para os dados da base
IC1 <- predict(model, interval="confidence",level = 0.95)
IC2 <- predict(model, interval="predict",level = 0.95)
new <- data.frame(Base$T_deg,Base$Depthm,Base$Salnty,IC1,IC2)

# Predição para profundidade em X e salinidade em X
Depthm <- Base$Depthm 
Salnty <- Base$Salnty
novo = data.frame(Depthm=0,Salnty=33)
predict(model, novo, interval="confidence")
predict(model, novo, interval="predict")
