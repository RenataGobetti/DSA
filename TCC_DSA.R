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

# Verificar onde há NA em cada coluna
na_count <- colSums(is.na(data_tccbrasil))

# Exibir contagem de NA por coluna
print(na_count)

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
