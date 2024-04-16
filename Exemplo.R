database <- read.csv(file.choose())

str(database)

database_estudo <- database[, c("Entity", "Year", "Access.to.electricity....of.population.", 
                                "Access.to.clean.fuels.for.cooking", 
                                "Renewable.electricity.generating.capacity.per.capita", 
                                "gdp_per_capita")]

names(database_estudo) <- c("pais", "ano", "acesso_eletricidade_pct", 
                            "acesso_combustiveis_limpos_pct", 
                            "capacidade_geracao_electricidade_renovavel_per_capita", 
                            "pib_per_capita")

head(database_estudo)

