#DATA WRANGLING

#Pacotes utilizados
library(dplyr)
library(tidyr)

#Nesse projeto estamos lidando com tres diferentes dataframes que iremos mesclar no data_tcc.
#Utilizaremos o data_tcc para as analises.

#Manipulacao do dataset baixado do Kaggle

# Ler o arquivo CSV do Kaggle
kaggledatabase <- read.csv(file.choose())

#Selecionar apenas as colunas relevantes
kaggledatabasewr <- kaggledatabase[, c("Entity", "Year", "Access.to.electricity....of.population.", "Value_co2_emissions_kt_by_country",
                                       "gdp_per_capita", "Land.Area.Km2.")]

#Renomear as colunas
names(kaggledatabasewr) <- c("pais", "ano", "acesso_eletricidade_populacao_porcent",
                             "CO2_emissao_kt_por_pais",
                             "pib_per_capita_dolar", "area_km2")

#Manipulacao do dataset com o IDH
hdi <- read.csv(choose.files())

#Selecionar apenas as colunas relevantes
colunas <- c("country", "hdicode", paste0("hdi_", 2000:2020))
idh_tcc <- hdi %>%
  select(all_of(colunas))

#Filtrar dados relevantes em Country 
idh_tcc <- idh_tcc %>%
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
names(idh_tcc)[1] <- "pais"
names(idh_tcc)[2] <- "idh_nivel"

#Renomear as colunas para substituir "hdi_" pelo ano correspondente
names(idh_tcc)[grep("^hdi_", names(idh_tcc))] <- gsub("^hdi_(\\d{4})$", "\\1", names(idh_tcc)[grep("^hdi_", names(idh_tcc))])

#Transpor a tabela idh_tcc
idh_transposta <- pivot_longer(idh_tcc, cols = -c(pais, idh_nivel), names_to = "ano", values_to = "idh_valor")

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

#Juncao dos dois conjuntos de dados: kaggledatabasewr e worldbankdatabase
data_tcc <- merge(kaggledatabasewr, worldbankdatabase, by = c("pais", "ano"))

#Filtrar apenas os paises presentes no conjunto de dados kaggledatabasewr
data_tcc <- data_tcc[data_tcc$pais %in% kaggledatabasewr$pais, ]

#Converter o ano para formato numerico
data_tcc$ano <- as.numeric(data_tcc$ano)

#Arredondar a coluna "acesso_eletricidade_populacao_percent" para duas casas decimais
data_tcc$acesso_eletricidade_populacao_percent <- round(data_tcc$acesso_eletricidade_populacao_percent, 2)

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

#Merge da tabela idh_tcc com data_tcc
data_tcc <- merge(data_tcc, idh_transposta, by = c("pais", "ano"), all.x = TRUE)

#Verificar paises da tabela (atualmente temos 195 paises reconhecidos pela ONU)
paises_unicos_data_tcc <- unique(data_tcc$pais)

#Lista de países para remover
paises_remover <- c("Aruba", "Bermuda", "Cayman Islands", "New Caledonia", "Puerto Rico")

#Remover linhas correspondentes aos países a remover
data_tcc <- data_tcc[!(data_tcc$`pais` %in% paises_remover), ]


#Tratamento de Dados Ausentes

#Verificar o numero de dados ausentes em cada coluna
missing_data <- colSums(is.na(data_tcc))

#Mostrar o resultado
print(missing_data)


#Correlacao entre os dados

#Matriz de correlação entre as variáveis selecionadas

#Imprime a matriz de correlação
print(correlation_matrix)

#Essa matriz mostra as correlações entre as variáveis. 
#Cada célula indica o coeficiente de correlação entre duas variáveis. 
#Um valor próximo de 1 indica uma correlação positiva forte,
#enquanto um valor próximo de -1 indica uma correlação negativa forte. 
#Um valor próximo de 0 indica uma correlação fraca ou inexistente.
#
#Podemos ver que
#Ha uma forte correlacao positiva entre acesso a eletricidade e expectativa de vida, 
#assim como entre acesso a eletricidade e PIB per capita.
#Ha uma correlacao positiva moderada entre acesso a eletricidade e CO2 emissao, 
#bem como entre PIB per capita e expectativa de vida.
#Ha uma correlacao positiva fraca entre CO2 emissao e expectativa de vida, 
#e entre CO2 emissao e PIB per capita.
#A populacao total tem uma correlacao moderada positiva com CO2 emissao 
#e uma correlacao fraca negativa com PIB per capita.


#Analise exploratoria de dados

