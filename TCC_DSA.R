#DATA WRANGLING

#Pacotes utilizados
library(dplyr)
library(tidyr)

#Nesse projeto estamos lidando com diferentes dataframes que iremos mesclar no data_tcc qdo se trata de todos os paises e no data_tccbrasil
#para dados do Brasil.

#Manipulacao do dataset baixado do Kaggle

# Ler o arquivo CSV do Kaggle
kaggledatabase <- read.csv(file.choose())

#Selecionar apenas as colunas relevantes
kaggledatabase <- kaggledatabase[, c("Entity", "Year", "Access.to.electricity....of.population.", "Value_co2_emissions_kt_by_country",
                                       "gdp_per_capita", "Land.Area.Km2.")]

#Renomear as colunas
names(kaggledatabase) <- c("pais", "ano", "acesso_eletricidade_populacao_porcent",
                             "CO2_emissao_kt_por_pais",
                             "pib_per_capita_dolar", "area_km2")

#Manipulacao do dataset com o IDH
hdi <- read.csv(choose.files())

#Selecionar apenas as colunas relevantes
colunas <- c("country", "hdicode", paste0("hdi_", 2000:2020))
hdi <- hdi %>%
  select(all_of(colunas))

#Filtrar dados relevantes em Country 
hdi <- hdi %>%
  filter(country != "World" & 
           country != "Very high human development" & 
           country != "Sub-Saharan Africa" & 
           country != "South Asia" & 
           country != "Monaco" & 
           country != "Medium human development" & 
           country != "Low human development" & 
           country != "Latin America and the Caribbean" & 
           country != "Korea (Democratic People's Rep. of)" & 
           country != "High human development" & 
           country != "Europe and Central Asia" & 
           country != "East Asia and the Pacific")

#Renomear as colunas country e hdicode para nomes em portugues
names(hdi)[1] <- "pais"
names(hdi)[2] <- "idh_nivel"

#Renomear as colunas para substituir "hdi_" pelo ano correspondente
names(hdi)[grep("^hdi_", names(hdi))] <- gsub("^hdi_(\\d{4})$", "\\1", names(hdi)[grep("^hdi_", names(hdi))])

#Transpor a tabela 
hdi <- pivot_longer(hdi, cols = -c(pais, idh_nivel), names_to = "ano", values_to = "idh_valor")

#Manipulacao de dados e data wrangling em arquivo baixado diretamente do databank.worldbank.org

#Ler o arquivo CSV do World Bank
worldbankdatabase <- read.csv(file.choose())

#Remover colunas desnecessarias
worldbankdatabase <- worldbankdatabase[, -c(2, 4)]

#Renomear colunas com anos
novos_nomes <- c("pais", "series.name", paste0(2000:2020))
colnames(worldbankdatabase) <- novos_nomes

#Restruturacao de dados
worldbankdatabase <- worldbankdatabase %>%
  pivot_longer(cols = -c(pais, series.name), names_to = "ano", values_to = "valor")

#Deletar linhas vazias
worldbankdatabase <- worldbankdatabase %>%
  filter(pais != "", pais != "Data from database: World Development Indicators", pais != "Last Updated: 03/28/2024")

#Converter os dados de volta para o formato wide
worldbankdatabase <- worldbankdatabase %>%
  pivot_wider(names_from = series.name, values_from = valor)

#Juncao dos dois conjuntos de dados: kaggledatabase e worldbankdatabase
data_tcc <- merge(kaggledatabase, worldbankdatabase, by = c("pais", "ano"))

#Filtrar apenas os paises presentes no conjunto de dados kaggledatabase
data_tcc <- data_tcc[data_tcc$pais %in% kaggledatabase$pais, ]

#Converter o ano para formato numerico
data_tcc$ano <- as.numeric(data_tcc$ano)

#Arredondar a coluna "pib_per_capita_dolar" para duas casas decimais
data_tcc$pib_per_capita_dolar <- round(data_tcc$pib_per_capita_dolar, 2)

#Renomear a coluna "Life expectancy at birth, total (years)" para "expectativa_vida"
names(data_tcc)[names(data_tcc) == "Life expectancy at birth, total (years)"] <- "expectativa_vida"

#Converter a coluna "expectativa_vida" para numerico
data_tcc$expectativa_vida <- as.numeric(data_tcc$expectativa_vida)

#Remover casas decimais da coluna "expectativa_vida"
data_tcc$expectativa_vida <- round(data_tcc$expectativa_vida, digits = 0)

#Renomear a coluna "Population, total" para "total_populacao"
names(data_tcc)[names(data_tcc) == "Population, total"] <- "total_populacao"

#Converter total_populacao para formato numero
data_tcc$total_populacao <- as.numeric(data_tcc$total_populacao)

#Renomear a coluna "Electric power consumption (kWh per capita)"
names(data_tcc)[names(data_tcc) == "Electric power consumption (kWh per capita)"] <- "consumo_kwh_per_capita"

#Converter consumo_kwh_per_capita para formato numero
data_tcc$consumo_kwh_per_capita <- as.numeric(data_tcc$consumo_kwh_per_capita)

#Remover casas decimais da coluna "consumo_kwh_per_capita"
data_tcc$`consumo_kwh_per_capita` <- round(data_tcc$`consumo_kwh_per_capita`, digits = 0)

#Merge da tabela hdi com data_tcc
data_tcc <- merge(data_tcc, hdi, by = c("pais", "ano"), all.x = TRUE)

#Verificar paises da tabela (atualmente temos 195 paises reconhecidos pela ONU)
paises_unicos_data_tcc <- unique(data_tcc$pais)

#Lista de paises para remover
paises_remover <- c("Aruba", "Bermuda", "Cayman Islands", "New Caledonia", "Puerto Rico")

#Remover linhas correspondentes aos paises a remover
data_tcc <- data_tcc[!(data_tcc$`pais` %in% paises_remover), ]

#Adicionar mais dados sobre energia
dataenergia <- read.csv(file.choose())

# Renomeando as colunas
colnames(dataenergia) <- c("pais", "pais_codigo", "series_name", "series_code", 
                           as.numeric(2000:2020))

# Removendo as colunas pais_codigo e series_code
dataenergia <- dataenergia[, !(colnames(dataenergia) %in% c("pais_codigo", "series_code"))]

# Transpor o dataenergia usando pivot_longer
dataenergia <- pivot_longer(dataenergia, cols = starts_with("20"),
                                       names_to = "ano", values_to = "valor")

# Convertendo a coluna 'valor' para numerica
dataenergia$valor <- as.numeric(dataenergia$valor)

# Agrupando os dados por pais, ano e tipo de serie e calculando a media dos valores
dataenergia <- dataenergia %>%
  group_by(pais, ano, series_name) %>%
  summarise(valor = mean(valor))

# Espalhando os valores da coluna series_name
dataenergia <- spread(dataenergia, key = series_name, value = valor)

# Realizar o merge dos datasets, mantendo apenas os paises presentes em data_tcc
data_tcc <- merge(data_tcc, dataenergia, by = c("ano", "pais"), all.x = TRUE)

# Desativar a formatacao em notacao cientifica
options(scipen = 999)

# Usando subset para excluir a coluna V1
data_tcc <- subset(data_tcc, select = -V1)

# Ou usando o operador de selecao de colunas
data_tcc <- data_tcc[, !names(data_tcc) %in% "V1"]

# Verificar novamente a estrutura do dataframe data_tcc
str(data_tcc)

#Filtrar tabela para dados apenas do Brasil
data_tccbrasil <- subset(data_tcc, pais == "Brazil")

#Verificar nome das colunas do data_tccbrasil
colunas <- names(data_tccbrasil)
print(colunas)

# Renomear as colunas em portugues
colnames(data_tccbrasil) <- c(
  "ano",
  "pais",
  "acesso_eletricidade_populacao_porcent",
  "CO2_emissao_kt_por_pais",
  "pib_per_capita_dolar",
  "area_km2",
  "expectativa_vida",
  "total_populacao",
  "consumo_kwh_per_capita",
  "idh_nivel",
  "idh_valor",
  "Energia_Alternativa_e_Nuclear_percent_total",
  "Producao_Eletricidade_de_Carvao_percent_total",
  "Producao_Eletricidade_de_Hidreletrica_percent_total",
  "Producao_Eletricidade_de_Gas_Natural_percent_total",
  "Producao_Eletricidade_de_Nuclear_percent_total",
  "Producao_Eletricidade_de_Petroleo_percent_total",
  "Producao_Eletricidade_de_Combustiveis_Fosseis_percent_total",
  "Producao_Eletricidade_de_Renovavel_excluindo_Hidreletrica_percent_total",
  "Producao_Eletricidade_de_Renovavel_excluindo_Hidreletrica_kWh",
  "Consumo_Energia_Combustiveis_Fosseis_percent_total",
  "Producao_Energia_Eletrica_Renovavel_percent_total",
  "Consumo_Energia_Renovavel_percent_total_consumo_final_energia"
)

# Renomear as colunas em portugues
colnames(data_tcc) <- c(
  "ano",
  "pais",
  "acesso_eletricidade_populacao_porcent",
  "CO2_emissao_kt_por_pais",
  "pib_per_capita_dolar",
  "area_km2",
  "expectativa_vida",
  "total_populacao",
  "consumo_kwh_per_capita",
  "idh_nivel",
  "idh_valor",
  "Energia_Alternativa_e_Nuclear_percent_total",
  "Producao_Eletricidade_de_Carvao_percent_total",
  "Producao_Eletricidade_de_Hidreletrica_percent_total",
  "Producao_Eletricidade_de_Gas_Natural_percent_total",
  "Producao_Eletricidade_de_Nuclear_percent_total",
  "Producao_Eletricidade_de_Petroleo_percent_total",
  "Producao_Eletricidade_de_Combustiveis_Fosseis_percent_total",
  "Producao_Eletricidade_de_Renovavel_excluindo_Hidreletrica_percent_total",
  "Producao_Eletricidade_de_Renovavel_excluindo_Hidreletrica_kWh",
  "Consumo_Energia_Combustiveis_Fosseis_percent_total",
  "Producao_Energia_Eletrica_Renovavel_percent_total",
  "Consumo_Energia_Renovavel_percent_total_consumo_final_energia"
)

# Alterar o valor "Brazil" para "Brasil" na coluna pais
data_tccbrasil$pais <- gsub("\\bBrazil\\b", "Brasil", data_tccbrasil$pais)

#ANALISE DESCRITIVA#

# Verificar onde há NA em cada coluna
na_count <- colSums(is.na(data_tccbrasil))

# Exibir contagem de NA por coluna
print(na_count)

# Verificar proporção de valores ausentes em cada coluna
na_proportion <- na_count / nrow(data_tccbrasil)
print(na_proportion)

# Selecionar apenas as colunas numéricas
colunas_numericas <- sapply(data_tccbrasil, is.numeric)

# Calcular a média de cada coluna numérica
media_colunas <- colMeans(data_tccbrasil[, colunas_numericas], na.rm = TRUE)

# Preencher os valores ausentes com a média de cada coluna
data_tccbrasil <- data_tccbrasil %>%
  mutate(across(where(is.numeric), ~replace_na(., media_colunas[match(cur_column(), names(media_colunas))])))

# Verificar novamente se há valores ausentes
na_count <- colSums(is.na(data_tccbrasil))
print(na_count)

# Carregar pacotes necessários
library(dplyr)
library(tibble)
library(knitr)

# Função para calcular a moda
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Calcular estatísticas descritivas manualmente para cada variável
acesso_stats <- c(min(data_tccbrasil$acesso_eletricidade_populacao_porcent, na.rm = TRUE),
                  quantile(data_tccbrasil$acesso_eletricidade_populacao_porcent, 0.25, na.rm = TRUE),
                  median(data_tccbrasil$acesso_eletricidade_populacao_porcent, na.rm = TRUE),
                  mean(data_tccbrasil$acesso_eletricidade_populacao_porcent, na.rm = TRUE),
                  quantile(data_tccbrasil$acesso_eletricidade_populacao_porcent, 0.75, na.rm = TRUE),
                  max(data_tccbrasil$acesso_eletricidade_populacao_porcent, na.rm = TRUE),
                  mode(data_tccbrasil$acesso_eletricidade_populacao_porcent),
                  var(data_tccbrasil$acesso_eletricidade_populacao_porcent, na.rm = TRUE),
                  sd(data_tccbrasil$acesso_eletricidade_populacao_porcent, na.rm = TRUE))

co2_stats <- c(min(data_tccbrasil$CO2_emissao_kt_por_pais, na.rm = TRUE),
               quantile(data_tccbrasil$CO2_emissao_kt_por_pais, 0.25, na.rm = TRUE),
               median(data_tccbrasil$CO2_emissao_kt_por_pais, na.rm = TRUE),
               mean(data_tccbrasil$CO2_emissao_kt_por_pais, na.rm = TRUE),
               quantile(data_tccbrasil$CO2_emissao_kt_por_pais, 0.75, na.rm = TRUE),
               max(data_tccbrasil$CO2_emissao_kt_por_pais, na.rm = TRUE),
               mode(data_tccbrasil$CO2_emissao_kt_por_pais),
               var(data_tccbrasil$CO2_emissao_kt_por_pais, na.rm = TRUE),
               sd(data_tccbrasil$CO2_emissao_kt_por_pais, na.rm = TRUE))

pib_stats <- c(min(data_tccbrasil$pib_per_capita_dolar, na.rm = TRUE),
               quantile(data_tccbrasil$pib_per_capita_dolar, 0.25, na.rm = TRUE),
               median(data_tccbrasil$pib_per_capita_dolar, na.rm = TRUE),
               mean(data_tccbrasil$pib_per_capita_dolar, na.rm = TRUE),
               quantile(data_tccbrasil$pib_per_capita_dolar, 0.75, na.rm = TRUE),
               max(data_tccbrasil$pib_per_capita_dolar, na.rm = TRUE),
               mode(data_tccbrasil$pib_per_capita_dolar),
               var(data_tccbrasil$pib_per_capita_dolar, na.rm = TRUE),
               sd(data_tccbrasil$pib_per_capita_dolar, na.rm = TRUE))

expectativa_stats <- c(min(data_tccbrasil$expectativa_vida, na.rm = TRUE),
                       quantile(data_tccbrasil$expectativa_vida, 0.25, na.rm = TRUE),
                       median(data_tccbrasil$expectativa_vida, na.rm = TRUE),
                       mean(data_tccbrasil$expectativa_vida, na.rm = TRUE),
                       quantile(data_tccbrasil$expectativa_vida, 0.75, na.rm = TRUE),
                       max(data_tccbrasil$expectativa_vida, na.rm = TRUE),
                       mode(data_tccbrasil$expectativa_vida),
                       var(data_tccbrasil$expectativa_vida, na.rm = TRUE),
                       sd(data_tccbrasil$expectativa_vida, na.rm = TRUE))

consumo_stats <- c(min(data_tccbrasil$consumo_kwh_per_capita, na.rm = TRUE),
                   quantile(data_tccbrasil$consumo_kwh_per_capita, 0.25, na.rm = TRUE),
                   median(data_tccbrasil$consumo_kwh_per_capita, na.rm = TRUE),
                   mean(data_tccbrasil$consumo_kwh_per_capita, na.rm = TRUE),
                   quantile(data_tccbrasil$consumo_kwh_per_capita, 0.75, na.rm = TRUE),
                   max(data_tccbrasil$consumo_kwh_per_capita, na.rm = TRUE),
                   mode(data_tccbrasil$consumo_kwh_per_capita),
                   var(data_tccbrasil$consumo_kwh_per_capita, na.rm = TRUE),
                   sd(data_tccbrasil$consumo_kwh_per_capita, na.rm = TRUE))

idh_stats <- c(min(data_tccbrasil$idh_valor, na.rm = TRUE),
               quantile(data_tccbrasil$idh_valor, 0.25, na.rm = TRUE),
               median(data_tccbrasil$idh_valor, na.rm = TRUE),
               mean(data_tccbrasil$idh_valor, na.rm = TRUE),
               quantile(data_tccbrasil$idh_valor, 0.75, na.rm = TRUE),
               max(data_tccbrasil$idh_valor, na.rm = TRUE),
               mode(data_tccbrasil$idh_valor),
               var(data_tccbrasil$idh_valor, na.rm = TRUE),
               sd(data_tccbrasil$idh_valor, na.rm = TRUE))

# Criar um dataframe com as estatísticas descritivas
stats_df <- data.frame(
  Estatística = c("Mínimo", "1º Quartil", "Mediana", "Média", "3º Quartil", "Máximo", "Moda", "Variância", "Desvio Padrão"),
  `Acesso à Eletricidade (%)` = acesso_stats,
  `Emissão de CO2 (kt)` = co2_stats,
  `PIB per Capita (USD)` = pib_stats,
  `Expectativa de Vida (anos)` = expectativa_stats,
  `Consumo de Energia Elétrica (kWh per capita)` = consumo_stats,
  `IDH` = idh_stats
)

# Exibir a tabela
print(stats_df)


# Carregar o pacote knitr
library(knitr)

# Exibir a tabela de forma amigável
kable(stats_df, caption = "Estatísticas Descritivas dos Indicadores do Brasil (2000-2020)")

# Instalar e carregar o pacote ggplot2
install.packages("ggplot2")
library(ggplot2)

# Gráfico de Linha para Mostrar a Evolução do Acesso à Eletricidade ao Longo do Tempo
ggplot(data_tccbrasil, aes(x = ano, y = acesso_eletricidade_populacao_porcent)) +
  geom_line(color = "blue") +
  labs(title = "Acesso à Eletricidade (% da População) ao Longo do Tempo", x = "Ano", y = "Acesso à Eletricidade (%)")

# Emissão de CO2 ao longo do tempo
ggplot(data_tccbrasil, aes(x = ano, y = CO2_emissao_kt_por_pais)) +
  geom_line(color = "red") +
  labs(title = "Emissão de CO2 ao Longo do Tempo", x = "Ano", y = "Emissão de CO2 (kt)")

# PIB per capita ao longo do tempo
ggplot(data_tccbrasil, aes(x = ano, y = pib_per_capita_dolar)) +
  geom_line(color = "green") +
  labs(title = "PIB per Capita ao Longo do Tempo", x = "Ano", y = "PIB per Capita (USD)")

# Expectativa de vida ao longo do tempo
ggplot(data_tccbrasil, aes(x = ano, y = expectativa_vida)) +
  geom_line(color = "purple") +
  labs(title = "Expectativa de Vida ao Longo do Tempo", x = "Ano", y = "Expectativa de Vida (anos)")

# Consumo de energia elétrica per capita ao longo do tempo
ggplot(data_tccbrasil, aes(x = ano, y = consumo_kwh_per_capita)) +
  geom_line(color = "orange") +
  labs(title = "Consumo de Energia Elétrica per Capita ao Longo do Tempo", x = "Ano", y = "Consumo de Energia Elétrica (kWh per capita)")

# IDH ao longo do tempo
ggplot(data_tccbrasil, aes(x = ano, y = idh_valor)) +
  geom_line(color = "darkblue") +
  labs(title = "Índice de Desenvolvimento Humano (IDH) ao Longo do Tempo", x = "Ano", y = "IDH")

#Analise Quantitativa

# Verificar os tipos de colunas
str(data_tccbrasil)

# Selecionar apenas colunas numéricas
numeric_cols <- sapply(data_tccbrasil, is.numeric)
data_numeric <- data_tccbrasil[, numeric_cols]

# Calcular a matriz de correlação
cor_matrix <- cor(data_numeric, use = "complete.obs")

# Calcular o desvio padrão de cada coluna
std_devs <- apply(data_numeric, 2, sd, na.rm = TRUE)

# Identificar colunas com desvio padrão zero
zero_sd_cols <- names(std_devs[std_devs == 0])
print(zero_sd_cols)

# Instalar e carregar os pacotes necessários
install.packages("ggplot2")
install.packages("ggcorrplot")
library(ggplot2)
library(ggcorrplot)

# Verificar os tipos de colunas
str(data_tccbrasil)

# Exibir a matriz de correlação
print(cor_matrix)


# Calcular a matriz de correlação
cor_matrix <- cor(data_tccbrasil[, c("acesso_eletricidade_populacao_porcent", "CO2_emissao_kt_por_pais", 
                                     "pib_per_capita_dolar", "expectativa_vida", 
                                     "consumo_kwh_per_capita", "idh_valor")], use="complete.obs")

# Visualizar a matriz de correlação
print(cor_matrix)

#Regressao Linear Simples
# Regressão linear simples para prever o IDH com base no PIB per capita
modelo_pib_idh <- lm(idh_valor ~ pib_per_capita_dolar, data = data_tccbrasil)
summary(modelo_pib_idh)

# Visualização
ggplot(data_tccbrasil, aes(x = pib_per_capita_dolar, y = idh_valor)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Relação entre PIB per Capita e IDH", x = "PIB per Capita (USD)", y = "IDH")

#Regressao Linear Multipla
# Regressão linear múltipla para prever o IDH com base em múltiplas variáveis
modelo_multiplo <- lm(idh_valor ~ pib_per_capita_dolar + acesso_eletricidade_populacao_porcent + 
                        expectativa_vida + consumo_kwh_per_capita, data = data_tccbrasil)
summary(modelo_multiplo)

#Analise de Series Temporais
# Carregar o pacote forecast
install.packages("forecast")
library(forecast)

# Preparar os dados para a análise de séries temporais
ts_data <- ts(data_tccbrasil$idh_valor, start = c(2000), frequency = 1)

# Modelo ARIMA para previsão do IDH
modelo_arima <- auto.arima(ts_data)
summary(modelo_arima)

# Previsão para os próximos 5 anos
forecast_arima <- forecast(modelo_arima, h = 5)
plot(forecast_arima)

#Extracao para PowerBI
# Exportar o dataframe data_tccbrasil para um arquivo CSV
write.csv(data_tccbrasil, "data_tccbrasil.csv", row.names = FALSE)

# Exportar o dataframe data_tccbrasil para um arquivo CSV no diretório especificado
write.csv(data_tccbrasil, "C:/Users/renat/OneDrive/Área de Trabalho/Renata/GitHub/data_tccbrasil.csv", row.names = FALSE)

# Exportar o dataframe data_tcc para um arquivo CSV
write.csv(data_tcc, "data_tcc.csv", row.names = FALSE)

# Exportar o dataframe data_tccbrasil para um arquivo CSV no diretório especificado
write.csv(data_tcc, "C:/Users/renat/OneDrive/Área de Trabalho/Renata/GitHub/data_tcc.csv", row.names = FALSE)

