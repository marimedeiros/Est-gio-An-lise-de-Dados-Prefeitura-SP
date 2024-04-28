## Importando arquivos
library(readr)
geosampa_distritos <- read_csv("Downloads/desafio_estagio_dados_pmsp/dados/geosampa_distritos.csv")
View(geosampa_distritos)

library(readr)
observasampa_familias_extrema_pobreza <- read_csv("Downloads/desafio_estagio_dados_pmsp/dados/observasampa_familias_extrema_pobreza.csv")


## Pacotes utilizados
library(reshape2)
library(ggplot2)

### 1. Quantas famílias em situação de extrema pobreza existiam em São Paulo em 2023?

# Filtrando os dados para o ano de 2023
dados_2023 <- subset(observasampa_familias_extrema_pobreza, ano == 2023)

# Somando a quantidade de famílias em situação de extrema pobreza em 2023
total_familias_extrema_pobreza_2023 <- sum(dados_2023$qtd_familias)

# Resultado
print(total_familias_extrema_pobreza_2023)




### 2. Qual o percentual (em relação ao total da cidade) de famílias em situação de extrema pobreza por Distrito em 2023?

# Calculando a soma das famílias em situação de extrema pobreza por distrito em 2023
total_por_distrito_2023 <- aggregate(qtd_familias ~ distrito, data = dados_2023, sum)

# Calculando o total de famílias em situação de extrema pobreza em 2023 na cidade de São Paulo
total_cidade_2023 <- sum(dados_2023$qtd_familias)

# Calculando o percentual por distrito em relação ao total da cidade
total_por_distrito_2023$percentual <- (total_por_distrito_2023$qtd_familias / total_cidade_2023) * 100

# Resultado
print(total_por_distrito_2023)




### 3. Qual distrito apresentou o maior aumento de famílias em situação de pobreza entre 2022 e 2023?

# Filtrando os dados para os anos de 2022 e 2023
dados_2022_2023 <- subset(observasampa_familias_extrema_pobreza, ano %in% c(2022, 2023))

# Agregando os dados por distrito e ano
dados_agrupados_2022_2023 <- aggregate(qtd_familias ~ distrito + ano, data = dados_2022_2023, sum)

# Pivotando os dados para ter uma linha para cada distrito com as quantidades de famílias em 2022 e 2023
dados_pivotados <- dcast(dados_agrupados_2022_2023, distrito ~ ano, value.var = "qtd_familias")

# Calculando a diferença entre 2023 e 2022
dados_pivotados$diferenca <- dados_pivotados$`2023` - dados_pivotados$`2022`

# Identificando o distrito com o maior aumento
distrito_maior_aumento <- dados_pivotados[which.max(dados_pivotados$diferenca), ]

# Resultado
print(distrito_maior_aumento)




### 4. Quantas famílias em situação de extrema pobreza existiam em cada Subprefeitura de São Paulo em 2023?

# Fundir os dados com base na coluna ds_nome (nome do distrito)
dados_completos <- merge(geosampa_distritos, observasampa_familias_extrema_pobreza, by = "ds_nome")

# Filtrar os dados para o ano de 2023
dados_completos_2023 <- subset(dados_completos, ano == 2023)

# Calcular o total de famílias em situação de extrema pobreza por Subprefeitura
total_por_subprefeitura <- aggregate(qtd_familias ~ ds_subpref, data = dados_completos_2023, sum)

# Exibir o resultado
print(total_por_subprefeitura)




### 5. Como variou o número de famílias em situação de pobreza entre 2013 e 2023 nos Distritos de Grajaú, Jardim Ângela, Cidade Ademar? Exiba graficamente o resultado, escolhendo a visualização que achar mais adequada.

# Filtrar os dados para incluir apenas os distritos desejados
distritos_desejados <- c("GRAJAU", "JARDIM ANGELA", "CIDADE ADEMAR")
dados_filtrados <- subset(dados_completos, ds_nome %in% distritos_desejados)

# Filtrar os dados para os anos de 2013 e 2023
dados_filtrados_2013_2023 <- subset(dados_filtrados, ano %in% c(2013, 2023))

# Agrupar os dados por ano e distrito
dados_agrupados_2013_2023 <- aggregate(qtd_familias ~ ds_nome + ano, data = dados_filtrados_2013_2023, sum)

# Plotar o gráfico
ggplot(dados_agrupados_2013_2023, aes(x = ano, y = qtd_familias, color = ds_nome, group = ds_nome)) +
  geom_line() +
  labs(title = "Variação do número de famílias em situação de pobreza (2013-2023)",
       x = "Ano",
       y = "Número de Famílias",
       color = "Distrito")



### 6. Complete a seguinte frase: "De acordo com os dados do ObservaSampa, em 2023 metade dos Distritos de São Paulo, no máximo, ___ famílias em situação de extrema pobreza". Qual informação você utilizou par acompletar a frase?

## A frase ficou um pouco confusa, mas para completá-la, pensei em calcular a porcentagem de distritos com número de famílias em situação de extrema pobreza igual ou superior à média de São Paulo em 2023

# Calcular a média do número de famílias em situação de extrema pobreza em 2023
media_familias_extrema_pobreza_2023 <- mean(dados_completos_2023$qtd_familias)

# Contar quantos distritos têm um número de famílias em situação de extrema pobreza igual ou superior à média
distritos_acima_da_media <- sum(dados_completos_2023$qtd_familias >= media_familias_extrema_pobreza_2023)

# Calcular a porcentagem de distritos com número de famílias em situação de extrema pobreza igual ou superior à média
porcentagem_distritos_acima_da_media <- (distritos_acima_da_media / nrow(dados_completos_2023)) * 100

# Exibir a porcentagem
print(porcentagem_distritos_acima_da_media)

## Portanto, "De acordo com os dados do ObservaSampa, em 2023, dos Distritos de São Paulo, 35,4% tinham famílias em situação de extrema pobreza acima da média do município"

