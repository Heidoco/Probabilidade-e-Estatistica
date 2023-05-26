grafico_histograma1 = ggplot(dados_df, aes(x=intervalo,fill=CO_GRUPO)) + 
  geom_histogram(stat = "count", binwidth = 500) +
  ggtitle("Gráfico histograma do salário por grupo de graduação") +
  xlab("Margem Salarial") +
  ylab("Frequência simples")
library(plotly)
library(ggplot2)


library(ggplot2)

Filtrar o conjunto de dados para CO_GRUPO igual a 4004 ou 4006
dados_filtrados <- Dataset[Dataset$CO_GRUPO %in% c(4004, 4006), ]

Criar o gráfico de barras para cada grupo
graficos_grupo <- lapply(unique(dados_filtrados$CO_GRUPO), function(grupo) {
  data_grupo <- dados_filtrados[dados_filtrados$CO_GRUPO == grupo, ]
  freq_questao30 <- table(data_grupo$QE_I30)

Mapear o nome do grupo para o título
  nome_grupo <- ifelse(grupo == 4004, "Ciência da Computação", "Sistemas da Informação")

  ggplot(data = as.data.frame(freq_questao30), aes(x = Var1, y = Freq)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    labs(title = paste("Frequência Q30 -", nome_grupo),
         x = "Resposta", y = "Frequência")
})

Mostrar os gráficos em uma única janela
library(gridExtra)
grid.arrange(grobs = graficos_grupo, ncol = 2)

library(ggplot2)

graficos_regiao <- lapply(unique(Dataset$CO_REGIAO_CURSO), function(regiao) {
  data_regiao <- Dataset[Dataset$CO_REGIAO_CURSO == regiao, ]
  freq_questao30 <- table(data_regiao$QE_I30)

  ggplot(data = as.data.frame(freq_questao30), aes(x = Var1, y = Freq)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    labs(title = paste("Frequência da Questão 30 - ", regiao),
         x = "Resposta", y = "Frequência")
})

library(gridExtra)
grid.arrange(grobs = graficos_regiao, ncol = 2)

ggplot(data = Dataset, aes(x = CO_MODALIDADE)) +
  geom_bar(fill = "steelblue", color = "white") +
  theme_classic()+
  labs(x = "Modalidade", y = "Estudantes") +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1)) +
  geom_text(stat = "count", aes(label = stat(count)), vjust = -0.5)

with(dataset, {
  Hist(QE_I76, scale = "frequency", breaks = seq(0, 500, by = 100), col = "darkgray",
       ylim = c(0, 80000), main = "Horas de estágio", xlab = "Quantidade de horas de estágio", ylab = "Quantidade de pessoas")
  abline(h = seq(0, 80000, by = 10000), col = "lightgray", lty = "dashed")
}) 

