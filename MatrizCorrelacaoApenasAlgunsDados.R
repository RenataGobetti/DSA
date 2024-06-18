# Pacotes utilizados
library(dplyr)
library(tidyr)
library(forecast)
library(corrplot)
library(ggplot2)
library(zoo)

# Ler o arquivo CSV do tcc_databrasil
tcc_databrasil <- read.csv(file.choose())

# Verificar a estrutura dos dados
str(tcc_databrasil)

# Função para interpolar e preencher valores ausentes no início e no final
preencher_e_interpolar <- function(x) {
  x <- ifelse(is.na(x), mean(x, na.rm = TRUE), x)  # Preencher valores iniciais/finais ausentes com a média
  na.approx(x, rule = 2)  # Aplicar interpolação linear
}

# Aplicar a função de preenchimento e interpolação apenas às colunas relevantes
tcc_databrasil <- tcc_databrasil %>%
  mutate(acesso_eletricidade_populacao_porcent = preencher_e_interpolar(acesso_eletricidade_populacao_porcent),
         pib_per_capita_dolar = preencher_e_interpolar(pib_per_capita_dolar),
         expectativa_vida = preencher_e_interpolar(expectativa_vida),
         CO2_emissao_kt_por_pais = preencher_e_interpolar(CO2_emissao_kt_por_pais),
         consumo_kwh_per_capita = preencher_e_interpolar(consumo_kwh_per_capita),
         idh_valor = preencher_e_interpolar(idh_valor))

# Verificar novamente a estrutura dos dados
str(tcc_databrasil)

# Selecionar apenas as colunas relevantes para calcular a matriz de correlação
variaveis_interesse <- tcc_databrasil %>%
  select(acesso_eletricidade_populacao_porcent, pib_per_capita_dolar, expectativa_vida, CO2_emissao_kt_por_pais, consumo_kwh_per_capita, idh_valor)

# Calcular a Matriz de Correlação com dados completos após tratamento
matriz_correlacao <- cor(variaveis_interesse, use = "complete.obs")

# Exibir a Matriz de Correlação
print(matriz_correlacao)

# Verificar e destacar possíveis correlações negativas
correlacoes_negativas <- matriz_correlacao[matriz_correlacao < 0]
print(correlacoes_negativas)

# Visualização da Matriz de Correlação
corrplot(matriz_correlacao, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

# Se houver correlações negativas, criar gráficos de dispersão para visualizá-las
if (any(matriz_correlacao < 0)) {
  # Encontrar pares de variáveis com correlações negativas
  pares_negativos <- which(matriz_correlacao < 0, arr.ind = TRUE)
  
  # Criar gráficos de dispersão para cada par de variáveis com correlação negativa
  for (i in 1:nrow(pares_negativos)) {
    var1 <- colnames(variaveis_interesse)[pares_negativos[i, 1]]
    var2 <- colnames(variaveis_interesse)[pares_negativos[i, 2]]
    
    # Gráfico de dispersão
    p <- ggplot(tcc_databrasil, aes_string(x = var1, y = var2)) +
      geom_point() +
      geom_smooth(method = "lm", col = "red") +
      labs(title = paste("Correlação Negativa entre", var1, "e", var2),
           x = var1, y = var2) +
      theme_minimal()
    
    print(p)
  }
} else {
  print("Nenhuma correlação negativa encontrada.")
}



# SEQUÊNCIA PARA ANÁLISE DE SÉRIE TEMPORAL

# Substitua `ano` pelo nome real da sua coluna de ano, se for diferente
ano_inicial <- min(tcc_databrasil$ano)
ano_final <- max(tcc_databrasil$ano)

# Convertendo a coluna de CO2 em série temporal
co2_ts <- ts(tcc_databrasil$CO2_emissao_kt_por_pais, start = c(ano_inicial), frequency = 1)

# Plotar a série temporal
plot(co2_ts, main="Emissões de CO2 por Ano", ylab="CO2 em kt", xlab="Ano")

# Ajustar o modelo ARIMA
modelo_arima <- auto.arima(co2_ts)

# Resumo do modelo
summary(modelo_arima)

# Fazer previsões para os próximos 10 anos (2021 a 2030)
previsoes <- forecast(modelo_arima, h=10)

# Plotar previsões
plot(previsoes, main="Previsões de Emissões de CO2", ylab="CO2 em kt", xlab="Ano")

# Exibir as previsões
print(previsoes)

# Comparar as previsões com as metas do Acordo de Paris
# Emissões de CO2 em 2005 (substitua `emissoes_2005` pelo valor real das emissões em 2005)
emissoes_2005 <- 1000000  # Substitua pelo valor correto

meta_2025 <- emissoes_2005 * (1 - 0.37)
meta_2030 <- emissoes_2005 * (1 - 0.43)

print(paste("Meta de redução de CO2 para 2025: ", meta_2025, " kt"))
print(paste("Meta de redução de CO2 para 2030: ", meta_2030, " kt"))

# Verificar se as previsões atendem às metas
previsoes_2025 <- previsoes$mean[5]  # Previsão para 2025 (5º valor da previsão)
previsoes_2030 <- previsoes$mean[10] # Previsão para 2030 (10º valor da previsão)

print(paste("Previsão de CO2 para 2025: ", previsoes_2025, " kt"))
print(paste("Previsão de CO2 para 2030: ", previsoes_2030, " kt"))

if (previsoes_2025 <= meta_2025) {
  print("O Brasil está no caminho para atingir a meta de 2025.")
} else {
  print("O Brasil NÃO está no caminho para atingir a meta de 2025.")
}

if (previsoes_2030 <= meta_2030) {
  print("O Brasil está no caminho para atingir a meta de 2030.")
} else {
  print("O Brasil NÃO está no caminho para atingir a meta de 2030.")
}
