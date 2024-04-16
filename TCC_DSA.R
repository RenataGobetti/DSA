#Manipulacao do dataset baixado do kaggle

database <- read.csv(file.choose())

database_estudo <- database[, c("Entity", "Year", "Access.to.electricity....of.population.", "Value_co2_emissions_kt_by_country"
                                , "gdp_per_capita", "Land.Area.Km2.")]

names(database_estudo) <- c("pais", "ano", "acesso_eletricidade_populacao_%", 
                            "CO2_emissao_kt_por_pais", 
                            "pib_per_capita_US$", "area_km2")

#Manipulacao de dados e data wrangling em arquivo baixado diretamente do databank.workdbank.org

novodatabase <- read.csv(file.choose(
))

novodatabase <- novodatabase[, -c(2, 4)]

novos_nomes <- c("pais", "series.name", paste0(2000:2020))

colnames(novodatabase) <- novos_nomes

head(novodatabase_long)

library(dplyr)

novodatabase_long <- novodatabase_long %>%
  filter(pais != "")

str(novodatabase_long)

novodatabase_long <- novodatabase_long %>%
  filter(pais != "Data from database: World Development Indicators")

novodatabase_long <- novodatabase_long %>%
  filter(pais != "Last Updated: 03/28/2024")

library(tidyr)

novodatabase_long <- novodatabase_long %>%
  pivot_wider(names_from = series.name, values_from = valor)

#Juncao desses dois arquivos


