library(tidyverse)
library(lubridate)

# --- 1. METODOLOGIA: TRATAMENTO E LIMPEZA DOS DADOS ---

# Lendo o dataset original
dados_filmes <- read.csv("dataset.csv")

# 1.1. Tratamento de Valores Ausentes (NAs)
dados_filmes <- dados_filmes %>%
  mutate(
    Plataforma_Streaming = ifelse(is.na(Plataforma_Streaming) | Plataforma_Streaming == "nan", "desconhecido", Plataforma_Streaming),
    Usuario_Premium = replace_na(Usuario_Premium, "desconhecido")
  )

# 1.2. Conversão de Tipos de Dados
dados_filmes$Data_Avaliacao <- as_date(dados_filmes$Data_Avaliacao)

# 1.3. Padronização de Variáveis Categóricas
dados_filmes <- dados_filmes %>%
  mutate(
    Genero_Filme = str_trim(tolower(Genero_Filme)),
    Plataforma_Streaming = str_trim(tolower(Plataforma_Streaming)),
    Pais_Origem = str_trim(tolower(Pais_Origem)),
    Usuario_Premium = str_trim(tolower(Usuario_Premium))
  )

# 1.4. Salvar a planilha tratada final para uso futuro
write.csv(dados_filmes, "results/dataset_tratado.csv", row.names = FALSE)

# --- 2. ANÁLISE EXPLORATÓRIA E GERAÇÃO DOS GRÁFICOS ---

# 1: Histograma
print(
  ggplot(dados_filmes, aes(x = Nota_Avaliacao)) +
    geom_histogram(bins = 5, fill = "#1f77b4", color = "black") +
    labs(title = "Distribuição das Notas de Avaliação", x = "Nota", y = "Frequência")
)
ggsave("results/1_histograma.png", width = 8, height = 6)


# 2: Gráfico de Linhas
media_por_ano <- dados_filmes %>%
  group_by(Ano_Lancamento) %>%
  summarise(Nota_Media = mean(Nota_Avaliacao, na.rm = TRUE))

print(
  ggplot(media_por_ano, aes(x = Ano_Lancamento, y = Nota_Media)) +
    geom_line(color = "#1f77b4", size = 1) +
    geom_point(color = "#1f77b4") +
    labs(title = "Média das Avaliações por Ano de Lançamento", x = "Ano de Lançamento", y = "Nota Média")
)
ggsave("results/2_linha.png", width = 8, height = 6)


# 3: Boxplot
print(
  ggplot(dados_filmes, aes(x = Plataforma_Streaming, y = Nota_Avaliacao, fill = Plataforma_Streaming)) +
    geom_boxplot(fill = "#1f77b4") +
    labs(title = "Distribuição das Notas por Plataforma de Streaming", x = "Plataforma de Streaming", y = "Nota de Avaliação") +
    theme(legend.position = "none")
)
ggsave("results/3_boxplot.png", width = 8, height = 6)


# 4: Gráfico de Barras
top_10_paises <- dados_filmes %>%
  filter(!is.na(Pais_Origem) & Pais_Origem != "") %>%
  count(Pais_Origem, sort = TRUE) %>%
  top_n(10, wt = n)

print(
  ggplot(top_10_paises, aes(x = reorder(Pais_Origem, n), y = n, fill = Pais_Origem)) +
    geom_bar(fill = "#1f77b4", stat = "identity") +
    coord_flip() +
    labs(title = "Top 10 Países com Mais Filmes Avaliados", x = "País de Origem", y = "Quantidade de Filmes") +
    theme(legend.position = "none")
)
ggsave("results/4_barra.png", width = 8, height = 6)


# 5: Gráfico de Dispersão
print(
  ggplot(dados_filmes, aes(x = Duracao_Minutos, y = Nota_Avaliacao)) +
    geom_point(alpha = 0.5, color = "#1f77b4") + 
    geom_smooth(method = "loess", color = "red") + 
    labs(title = "Relação entre Duração do Filme e Nota de Avaliação", x = "Duração (minutos)", y = "Nota de Avaliação")
)
ggsave("results/5_dispersao.png", width = 8, height = 6)
