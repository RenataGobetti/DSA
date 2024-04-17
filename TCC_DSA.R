#####DATA WRANGLING#####

# Manipulacao do dataset baixado do Kaggle

# Ler o arquivo CSV do Kaggle
kaggledatabase <- read.csv(file.choose())

# Selecionar apenas as colunas relevantes
kaggledatabasewr <- kaggledatabase[, c("Entity", "Year", "Access.to.electricity....of.population.", "Value_co2_emissions_kt_by_country",
                                       "gdp_per_capita", "Land.Area.Km2.")]

# Renomear as colunas para nomes mais descritivos
names(kaggledatabasewr) <- c("pais", "ano", "acesso_eletricidade_populacao_percent",
                             "CO2_emissao_kt_por_pais",
                             "pib_per_capita_dolar", "area_km2")

# Manipulacao de dados e data wrangling em arquivo baixado diretamente do databank.worldbank.org

# Ler o arquivo CSV do World Bank
worldbankdatabase <- read.csv(file.choose())

# Remover colunas desnecessarias
worldbankdatabase <- worldbankdatabase[, -c(2, 4)]

# Renomear colunas com anos
novos_nomes <- c("pais", "series.name", paste0(2000:2020))
colnames(worldbankdatabase) <- novos_nomes

# Usar a biblioteca tidyr para converter os dados para o formato "long"
library(tidyr)
worldbankdatabase <- worldbankdatabase %>%
  pivot_longer(cols = -c(pais, series.name), names_to = "ano", values_to = "valor")

# Deletar linhas vazias
library(dplyr)
worldbankdatabase <- worldbankdatabase %>%
  filter(pais != "", pais != "Data from database: World Development Indicators", pais != "Last Updated: 03/28/2024")

# Usar a biblioteca tidyr para converter os dados de volta para o formato "wide"
worldbankdatabase <- worldbankdatabase %>%
  pivot_wider(names_from = series.name, values_from = valor)

# Juncao dos dois conjuntos de dados: kaggledatabasewr e worldbankdatabase
data_tcc <- merge(kaggledatabasewr, worldbankdatabase, by = c("pais", "ano"))

# Filtrar apenas os paises presentes no conjunto de dados kaggledatabasewr
data_tcc <- data_tcc[data_tcc$pais %in% kaggledatabasewr$pais, ]

# Converter o ano para formato numerico
data_tcc$ano <- as.numeric(data_tcc$ano)

# Arredondar a coluna "acesso_eletricidade_populacao_percent" para duas casas decimais
data_tcc$acesso_eletricidade_populacao_percent <- round(data_tcc$acesso_eletricidade_populacao_percent, 2)

# Arredondar a coluna "pib_per_capita_dolar" para duas casas decimais
data_tcc$pib_per_capita_dolar <- round(data_tcc$pib_per_capita_dolar, 2)

# Renomear a coluna "Life expectancy at birth, total (years)" para "life_expectancy_years"
names(data_tcc)[names(data_tcc) == "Life expectancy at birth, total (years)"] <- "life_expectancy_years"

# Converter a coluna "life_expectancy_years" para numerico
data_tcc$life_expectancy_years <- as.numeric(data_tcc$life_expectancy_years)

# Remover casas decimais da coluna "life_expectancy_years"
data_tcc$life_expectancy_years <- round(data_tcc$life_expectancy_years, digits = 0)

# Renomear a coluna "Population, total" para "total_population"
names(data_tcc)[names(data_tcc) == "Population, total"] <- "total_population"

#Converter total_population para formato numero
data_tcc$total_population <- as.numeric(data_tcc$total_population)

#Renomear a coluna "Electric power consumption (kWh per capita)"
names(data_tcc)[names(data_tcc) == "Electric power consumption (kWh per capita)"] <- "consumption_kwh_per_capita"

#Converter consumption_kwh_per_capita para formato numero
data_tcc$consumption_kwh_per_capita <- as.numeric(data_tcc$consumption_kwh_per_capita)

#Remover casas decimais da coluna "consumption_kwh_per_capita"
data_tcc$`consumption_kwh_per_capita` <- round(data_tcc$`consumption_kwh_per_capita`, digits = 0)

#####CORRELACAO ENTRE OS DADOS#####

# Calcula a matriz de correlação entre as variáveis selecionadas
correlation_matrix <- cor(data_tcc[, c("acesso_eletricidade_populacao_percent", 
                                       "CO2_emissao_kt_por_pais", 
                                       "pib_per_capita_dolar", 
                                       "life_expectancy_years", 
                                       "total_population")], 
                          use="pairwise.complete.obs")

# Imprime a matriz de correlação
print(correlation_matrix)

#Essa matriz mostra as correlações entre as variáveis. 
#Cada célula indica o coeficiente de correlação entre duas variáveis. 
#Um valor próximo de 1 indica uma correlação positiva forte,
#enquanto um valor próximo de -1 indica uma correlação negativa forte. 
#Um valor próximo de 0 indica uma correlação fraca. 
