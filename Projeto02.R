# Formação Cientista de Dados - Projeto com Feedback 2
# Prevendo Demanda de Estoque com Base em Vendas
#
# Nome do Aluno: Victor Hugo Nishitani
#
# O Grupo Bimbo (https://www.grupobimbo.com), se esforça para atender a demanda diária 
# dos consumidores por produtos frescos de panificação nas prateleiras de mais de 1 milhão 
# de lojas ao longo das suas 45.000 lojas em todo o México.
#
# Atualmente, os cálculos diários de estoque são realizados por funcionários de vendas de entregas
# diretas, que devem, sozinhos, prever a necessidade de estoque dos produtos e demanda com base em 
# suas experiências pessoais em cada loja. Como alguns pães têm uma vida útil de uma semana, a 
# margem aceitável para o erro é pequena.
# 
# Neste projeto de aprendizado de máquina, você deve desenvolver um modelo para prever com precisão
# a demanda de estoque com base nos dados históricos de vendas. Isso fará com que os consumidores 
# dos mais de 100 produtos de panificação não fiquem olhando para as prateleiras vazias, além de 
# reduzir o valor gasto com reembolsos para os proprietários de lojas com produtos excedentes 
# impróprios para venda.
#
# Dicionário de Dados:
# DiaSemana — Número da Semana (de Quinta a Quarta)
# Id_Agencia — ID do Depósito de Vendas
# Id_Canal — ID do Canal de Vendas
# Id_Rota — ID da Rota (Várias Rotas = Depósito de Vendas)
# Id_Cliente — ID do Cliente
# Id_Produto — ID do Produto
# UnidadeVendas — Unidade de Vendas esta Semana (Inteiro)
# PesoUnidadeVendas — Vendas esta Semana (Unidade: Pesos)
# UnidadeDevolução — Retorna a Unidade na Próxima Semana (Inteiro)
# PesoUnidadeDevolução — Retorna na Próxima Semana (Unidade: Pesos)
# DemandaAjustada — Demanda Ajustada (Inteiro) - Coluna Alvo que iremos prever

# Configurando o diretório de trabalho
setwd("/Users/nishi/Desktop/FCD/BigDataRAzure/Cap18/Projeto 2")
getwd()

# Carrega os pacotes na sessão R
library(data.table)
library(caTools)
require(lattice)
library(dplyr)
library(ggplot2)
library(randomForest)


# Primeiro foi criada uma amostra aleatória de 100.000 registros,
# do dataset original "train.csv", com o nome "sample_train.csv".
# Depois, criamos outra amostra aleatória (do mesmo dataset) de  
# 30.000 registros, com o nome "sample_test.csv".
# amostra <- dados[sample(nrow(dados), size = 30000),]
# cols1 <- c("Id_Produto", "UnidadeVendas", "PesoUnidadeVendas")
# amostra <- amostra[, ..cols1]
# rm(cols1)
# fwrite(amostra, "sample_test.csv")


## Etapa 1 - Coletando os Dados
##### Carga dos Dados ##### 

# Carrega o dataset antes da transformação
dados <- fread("sample_train.csv", stringsAsFactors = F, sep = ",", header =T)



## Etapa 2 - Pré-Processamento
##### Análise Exploratória dos Dados - Limpeza e Organização de Dados ##### 

# Visualizando os dados
View(dados)
str(dados)
dim(dados)

# Verificando se temos valores ausentes
sum(is.na(dados))
sum(!complete.cases(dados))

# Renomear as colunas com nomes mais amigáveis
myColumns <- colnames(dados)
myColumns

myColumns[1] <- "DiaSemana"
myColumns[2] <- "Id_Agencia"
myColumns[3] <- "Id_Canal"
myColumns[4] <- "Id_Rota"
myColumns[5] <- "Id_Cliente"
myColumns[6] <- "Id_Produto"
myColumns[7] <- "UnidadeVendas"
myColumns[8] <- "PesoUnidadeVendas"
myColumns[9] <- "UnidadeDevolução"
myColumns[10] <- "PesoUnidadeDevolução"
myColumns[11] <- "DemandaAjustada"

myColumns

colnames(dados) <- myColumns
rm(myColumns)

# Obtendo apenas as colunas numéricas
colunas_numericas <- sapply(dados, is.numeric)
colunas_numericas

# Filtrando as colunas numéricas para correlação
data_cor <- cor(dados[,..colunas_numericas])
data_cor
head(data_cor)

# Utilizamos o modelo randomForest para criar um plot de importância das variáveis.
# Utilizando um vetor com os métodos de correlação "Pearson" e "Spearman"

# Definindo as colunas para a análise de correlação 
cols <- colunas_numericas

metodos <- c("pearson", "spearman")

# Aplicando os métodos de correlação com a função cor()
cors <- lapply(metodos, function(method) 
  (cor(dados[ , ..cols], method = method)))

head(cors)

# Preparando o plot
plot.cors <- function(x, labs){
  diag(x) <- 0.0 
  plot( levelplot(x, 
                  main = paste("Plot de Correlação usando Método", labs),
                  scales = list(x = list(rot = 90), cex = 1.0)) )
}

# Mapa de Correlação
Map(plot.cors, cors, metodos)

# Selecionar as variáveis que inicialmente serão usadas como mais importantes
cols <- c("DiaSemana", "Id_Cliente", "Id_Produto",
          "UnidadeVendas", "PesoUnidadeVendas",
          "UnidadeDevolução", "PesoUnidadeDevolução",
          "DemandaAjustada")

# Criando um subset dos dados para filtrar nosso dataset somente com as variáveis
# que selecionamos como importantes
dados <- dados[, ..cols]
rm (cols)

# Verificando os valores únicos nas variáveis
length(unique(dados$DiaSemana))
table(dados$DiaSemana)
length(unique(dados$Id_Cliente))
table(dados$Id_Cliente)
length(unique(dados$Id_Produto))
table(dados$Id_Produto)

# Medias de Tendência Central da variável Demanda Ajustada
summary(dados$DemandaAjustada)
min(dados$DemandaAjustada)

# Construindo um histograma
hist(dados$DemandaAjustada, main = 'Histograma', xlab = 'Demanda Ajustada', breaks = 3)
hist(dados$UnidadeVendas, main = 'Histograma', xlab = 'Unidades Vendidas', breaks = 3)
hist(dados$PesoUnidadeVendas, main = 'Histograma', xlab = 'Qtd Unidades Vendidas (Peso)', breaks = 3)
hist(dados$Id_Produto, main = 'Histograma', xlab = 'Produtos')
hist(dados$DiaSemana, main = 'Histograma', xlab = 'Dias da Semana')

# Boxplots para análise de correlações e outliers das variáveis
# Leitura de Baixo para Cima - Min, Q1, Q2 (Mediana), Q3 e Max

boxplot(dados$DemandaAjustada, main = "Boxplot para Demanda Ajustada", 
        ylab = "Demanda Ajustada")

boxplot(dados$UnidadeVendas, main = "Boxplot para Unidades Vendidas", 
        ylab = "Unidades Vendidas")

boxplot(dados$UnidadeVendas, main = "Boxplot para Qtd Unidades Vendidas (Peso)", 
        ylab = "Qtd Unidades Vendidas (Peso)")

boxplot(dados$DiaSemana, main = "Boxplot para Dia da Semana", 
        ylab = "Dia da Semana")

boxplot(dados$Id_Produto, main = "Boxplot para Produtos", 
        ylab = "Produtos")

ggplot(dados, aes(x = DiaSemana, y = DemandaAjustada)) + 
  geom_boxplot( ) +
  ylab("Demanda Ajustada") +
  labs(title = "Demanda por Dia da Semana")

# Pode-se dizer observando nos ScatterPlots abaixo, que a medida que a Unidade 
# de Vendas e o Peso da Unidade Vendas aumentam, a Demanda Ajustada também aumenta,
# ou seja, há uma correlação positiva entre as variáveis. Assim como há correlação
# positiva entre as variáveis Unidade de Vendas e Peso de Unidade de Vendas
ggplot(dados, aes(x = UnidadeVendas, y = DemandaAjustada)) + 
  geom_point(shape = 1) +
  geom_smooth(method = lm , color = "red", se = FALSE) + 
  ylab("Demanda Ajustada") +
  labs(title = "Demanda por Unidade de Vendas")

ggplot(dados, aes(x = PesoUnidadeVendas, y = DemandaAjustada)) + 
  geom_point(shape = 1) +
  geom_smooth(method = lm , color = "red", se = FALSE) +  
  ylab("Demanda Ajustada") +
  labs(title = "Demanda por Unidade de Vendas (Peso)")

ggplot(dados, aes(x = UnidadeVendas, y = PesoUnidadeVendas)) + 
  geom_point( ) +
  ylab("Unidade de Vendas") +
  labs(title = "Demanda por Unidade de Vendas (Peso)")

# Feature Selection

# Criando um modelo para identificar os atributos com maior importância para o modelo preditivo
# Avalidando a importância de todas as variaveis
modelo <- randomForest(DemandaAjustada ~ ., 
                       data = dados, 
                       ntree = 100, 
                       nodesize = 10,
                       importance = TRUE)

varImpPlot(modelo)

# Removendo variáveis colineares
modelo <- randomForest(DemandaAjustada ~ .
                       - DiaSemana
                       - Id_Cliente
                       - UnidadeDevolução
                       - PesoUnidadeDevolução, 
                       data = dados, 
                       ntree = 100, 
                       nodesize = 10,
                       importance = TRUE)

varImpPlot(modelo)

# Criar um fator ordenado para o número semana, começando na Quarta e terminando na Quinta
dados$DiaSemana <- factor(dados$DiaSemana, levels= 3:9,
                           labels=c("Quarta Feira", 
                                    "Quinta Feira", 
                                    "Sexta Feira", 
                                    "Sábado", 
                                    "Domingo", 
                                    "Segunda Feira", 
                                    "Terça Feira"))

str(dados$DiaSemana)

# Tabela de contingência dia da semana 
table(dados$DiaSemana)

## Etapa 3: Treinando o modelo e Criando o Modelo Preditivo no R

# Vamos dividir os dados em treino e teste, sendo 70% para dados de treino e 
# 30% para dados de teste
set.seed(2)
split = sample.split(dados$DemandaAjustada, SplitRatio = 0.70)
dados_treino = subset(dados, split == TRUE)
dados_teste = subset(dados, split == FALSE)

# Verificando o número de linhas
nrow(dados_treino)
nrow(dados_teste)

# Vamos utilizar o método "Random Forest" com as variáveis que selecionamos como
# mais importantes com os dados de treino
set.seed(123)
modelo_v1 <- randomForest(DemandaAjustada ~ UnidadeVendas
                          + PesoUnidadeVendas
                          + Id_Produto,
                          data = dados_treino, 
                          ntree = 100, 
                          nodesize = 10)



# Etapa 4: Avaliando a Performance do Modelo
# Mais detalhes sobre o modelo
print(modelo_v1)

# Agora fazemos as previsões com o modelo usando dados de teste
set.seed(456)
previsao_v1 <- data.frame(Real = dados_teste$DemandaAjustada,
                          Previsto = predict(modelo_v1, newdata = dados_teste))

min(previsao_v1)

# Nas observações do dataset é informado que a DemandaAjustada (Demanda_uni_equil) é 
# sempre >= 0, pois a demanda deve ser 0 ou um valor positivo. Sendo assim, precisamos
# tratar os valores negativos da nossa previsão

# Tratando os valores negativos
trata_zero <- function(x){
  if  (x < 0){
    return(0)
  }else{
    return(x)
  }
}

# Aplicando a função para tratar valores negativos em nossa previsão
previsao_v1$Previsto <- sapply(previsao_v1$Previsto, trata_zero)
previsao_v1$Previsto
min(previsao_v1)

# Interpretando o Modelo

# Calculando o erro médio ao Quadrado
## Quão distantes seus valores previstos estão dos valores observados
# MSE
set.seed(7)
mse <- mean((previsao_v1$Real - previsao_v1$Previsto)^2)
print(mse) # 19.27801

# RMSE (Raiz Quadrada)
set.seed(8)
caret::RMSE(previsao_v1$Previsto,previsao_v1$Real) # 4.390673

# R-squared (Coeficiente de Determinação)
set.seed(9)
caret::R2(previsao_v1$Previsto, previsao_v1$Real) # 0.9273474

# Temos a acurácia de 93% do nosso modelo utilizando o método "Random Forest"
# o que é excelente para um primeiro modelo

# Também podemos analisar os Resíduos criando um dataframe dos seus valores.
# Assim, podemos verificar detalhadamente os erros ocorridos no nosso modelo
inFrame <- previsao_v1[, c("Real", "Previsto")]
refFrame <- dados_teste

inFrame[, c("DiaSemana", "Id_Produto", "UnidadeVendas", "PesoUnidadeVendas")] <- 
  refFrame[, c("DiaSemana", "Id_Produto", "UnidadeVendas", "PesoUnidadeVendas")]

# Plot mostrando a diferença entre valores reais e valores previstos
inFrame <- inFrame[order(inFrame$Id_Produto),]
      ggplot() +
        geom_line(data = inFrame, 
                  aes(x = Id_Produto, y = Real), color = "blue") +
        geom_line(data = inFrame, 
                  aes(x = Id_Produto, y = Previsto), color = "red") +
        ylab("Demanda por ID do Produto - Real vs Previsto")
      theme(text = element_text(size = 20))
      
# Note que há um oulier da demanda um produto que nosso modelo não conseguiu prever      
            
# Computando os resíduos
inFrame <-  mutate(inFrame, Residuos = Previsto - Real)    

# Plotando os resíduos
ggplot(inFrame, aes(x = Residuos)) + 
  geom_histogram(binwidth = 1, fill = "white", color = "black")

# Veja que os resíduos estão praticamente todos próximos de zero, o que é um bom sinal

qqnorm(inFrame$Residuos)
qqline(inFrame$Residuos)

# Mediana dos resíduos por Produto
evalFrame <- inFrame %>%
  group_by(Id_Produto) %>%
  summarise(medResidByProduto = format(round(
    median(Previsto - Real), 2), 
    nsmall = 2)) 

# Computando a mediana dos resíduos
tempFrame <- inFrame %>%
  group_by(Id_Produto) %>%
  summarise(medResid = median(Previsto - Real)) 

evalFrame$Id_Produto <- tempFrame$Id_Produto
evalFrame$medResidByDemanda <- format(round(
  tempFrame$medResid, 2), 
  nsmall = 2)

print("Resumo dos residuos")
print(evalFrame)



## Etapa 5: Otimizando o Modelo preditivo

# Treinando o Modelo (usando os dados de treino)
modelo_v2 <- lm(DemandaAjustada ~ UnidadeVendas
                + PesoUnidadeVendas
                + Id_Produto,
                data = dados_treino)

# Visualizando os coeficientes
modelo_v2

# Obtendo os resíduos
res <- residuals(modelo_v2)

# Convertendo o objeto para um dataframe
res <- as.data.frame(res)
head(res)

# Histograma dos resíduos
ggplot(res, aes(res)) +  
  geom_histogram(fill = 'blue', 
                 alpha = 0.5, 
                 binwidth = 1)

# Nota-se que os resíduos do modelo Linear também estão próximos de zero

# Plot do Modelo
plot(modelo_v2)

# Prevendo a Demanda com Dados de teste
previsao_v2 <- predict(modelo_v2, dados_teste)
summary(previsao_v2)
View(previsao_v2)

# Mais detalhes sobre o modelo
summary(modelo_v2)
# Percebe-se que há uma forte correlação entre as variáveis, pois há muitos  
# astericos na frente das variáveis e o p-value é bem baixo (notação científica).
# Veja que o R-squared é de 0.9976, o que é muito bom, mas vamos calcular de forma
# manual para confirmar nosso resultado

# Visualizando os valores previstos e observados
resultados <- cbind(previsao_v2, dados_teste$DemandaAjustada) 
colnames(resultados) <- c('Previsto','Real')
resultados <- as.data.frame(resultados)
resultados
min(resultados) # -0.02701271

# Note que aqui também precisamos tratar os valores negativos na nossa previsão
# Como já temos a função trata_zero criada, só precisamos aplicá-la

# Aplicando a função para tratar valores negativos em nossa previsão
resultados$Previsto <- sapply(resultados$Previsto, trata_zero)
resultados$Previsto
min(resultados)

# Interpretando o Modelo

# Calculando o erro médio
# Quão distantes seus valores previstos estão dos valores observados
# MSE
mse <- mean((resultados$Real - resultados$Previsto)^2)
print(mse) # 0.6949942

# RMSE (Raiz Quadrada)
caret::RMSE(resultados$Previsto,resultados$Real) #  0.8336631

# R-squared (Coeficiente de Determinação)
caret::R2(resultados$Previsto,resultados$Real) # 0.9973052


# Observe que conseguimos aumentar o nível de precisão do nosso modelo de 93% para 99%,
# e também, diminuir consideravelmente nosso erro médio. Sendo assim, podemos confirmar
# que neste caso, o modelo_v2 (Lienar) é melhor e mais estável que o modelo_v1 (Random Forest)


# Ainda podemos fazer um teste para prever a demanda dos produtos com os dados de teste 
# sem a informação da Demanda Ajustada (Demanda Real)
sample_test <- fread("sample_test.csv", stringsAsFactors = F, sep = ",", header =T)
View(sample_test)
previsao_v3 <- predict(modelo_v2, sample_test)
View(previsao_v3)
summary(previsao_v3)
class(previsao_v3)

# Criando um dataframe com os resultados previstos
results <- as.data.frame(previsao_v3)
colnames(results) <- c('Previsto')

# Aplicando a função para tratar valores negativos em nossa previsão
results$Previsto <- sapply(results$Previsto, trata_zero)
results$Previsto
min(results)

# Dataframe com os valores previstos
results[, c("Id_Produto", "UnidadeVendas", "PesoUnidadeVendas")] <- 
  sample_test[, c("Id_Produto", "UnidadeVendas", "PesoUnidadeVendas")]

View(results)

# Verificando se temos valores ausentes
sum(is.na(results))
sum(!complete.cases(results))

# Portanto, nosso modelo mostra-se consistente e generalista, pois pode ser
# utilizado com outros modelos



