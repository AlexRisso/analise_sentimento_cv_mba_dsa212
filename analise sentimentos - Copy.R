# Instalação e Carregamento dos Pacotes
pacotes <- c("tidytext", "stringr","lexiconPT", "tidyverse", "ggExtra", 
             "magrittr", "lubridate", "ggplot2", "fmsb")

# lexiconPT: Pacote para análise em português
# ver https://sillasgonzaga.github.io/2017-09-23-sensacionalista-pt01/

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

library(stringr)
library(readxl)
library(tidyverse)
library(ggraph)
library(fmsb)

# Gera o data frame
candidatos_linkedin <- read_excel("candidatos texto linkedin.xlsx")
# Corrigir encoding do texto da coluna "sobre"
candidatos_linkedin$sobre %>% iconv(to = "ASCII//TRANSLIT")
# Remover emojis
candidatos_linkedin$sobre %>% iconv(sub="", 'UTF-8', 'ASCII')
# Remover caracter especial
candidatos_linkedin$sobre <- candidatos_linkedin$sobre %>%
lapply(gsub, pattern = '[^[:alnum:]]', replace = ' ') 
# Visualizar dataframe
glimpse(candidatos_linkedin)

# Carregar datasets
data("oplexicon_v3.0")
data("sentiLex_lem_PT02")

oplexicon30 <- oplexicon_v3.0 %>% glimpse
sentiLexPT02 <- sentiLex_lem_PT02 %>% glimpse

# tidytext: criar uma linha para cada palavra de um dos campo "sobre"
candidatos_linkedin_unnested <- candidatos_linkedin %>%
  unnest_tokens(term, sobre)

# Quantificar o sentimento do campo "sobre"
candidatos_linkedin_unnested %>% 
  left_join(oplexicon30, by = join_by(term),) %>% 
  left_join(sentiLexPT02 %>% select(term, lex_polarity = polarity), by = join_by(term)) %>% 
  select(id_candidato, term, polarity, lex_polarity) %>% 
  head(5)

# Manter apenas as palavras que possuem polaridade em ambos os léxicos: "oplexicon_v3.0" e "sentiLex_lem_PT02"
candidatos_linkedin_unnested <- candidatos_linkedin_unnested %>% 
  inner_join(oplexicon30, by = "term") %>% 
  inner_join(sentiLexPT02 %>% select(term, lex_polarity = polarity), by = "term") %>% 
  group_by(id_candidato) %>% 
  summarise(
    sobre_sent_op30 = sum(polarity),
    sobre_sent_lexPT02 = sum(lex_polarity),
    sum_words = n()
  ) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    most_neg = min(sobre_sent_lexPT02, sobre_sent_op30),
    most_pos = max(sobre_sent_lexPT02, sobre_sent_op30)
  )
head(candidatos_linkedin_unnested)

# Plotar o gráfico
linkedin_sent_graph <- candidatos_linkedin_unnested %>% 
  ggplot(aes(x = sobre_sent_op30, y = sobre_sent_lexPT02)) +
  geom_point(aes(color = sum_words)) + 
  scale_color_continuous(low = "green", high = "red") +
  labs(x = "Polaridade no OpLexicon", y = "Polaridade no SentiLex") +
  #geom_smooth(method = "lm") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed")

plot(linkedin_sent_graph)

# Outliers podem ser causados pela grande quantidade de palavras no texto.
# Tão somente somar a polaridade de cada palavra não é um bom método.
# Existem anomalias nos dados, pois as mesmas palavras possuem sentimentos 
# diferentes de acordo com o léxico usado.

# O conteúdo mais positivo dos textos dos candidatos
most_pos <- which.max(candidatos_linkedin_unnested$most_pos)
most_neg <- which.min(candidatos_linkedin_unnested$most_neg)
# Conteúdo mais positivo
most_pos_sobre <- dplyr::filter(candidatos_linkedin, id_candidato == candidatos_linkedin_unnested$id_candidato[most_pos])
cat(unlist(most_pos_sobre$sobre[1]))
cat(most_pos_sobre$id_candidato[1])
# Conteúdo mais negitivo
most_neg_sobre <- dplyr::filter(candidatos_linkedin, id_candidato == candidatos_linkedin_unnested$id_candidato[most_neg])
cat(unlist(most_neg_sobre$sobre[1]))
cat(most_neg_sobre$id_candidato[1])

# Resumo do deck
# Número de termos do deck após o processamento do stopwords
dplyr::summarise(corpus_deck, 
                 word=n(),
                 sort = TRUE)
# Agrupando os termos
corpus_deck_sent_sum <- corpus_deck %>% 
  dplyr::count(word)
head(corpus_deck_sent_sum)
# Quantidade de termoa após agrupamento
dplyr::summarise(corpus_deck_sent_sum, word=n())

# Analise de sentimento do deck usando o "oplexicon_v3.0"
corpus_deck_sent_op30 <- rename(corpus_deck, 
                                term = word) %>% 
  mutate(origem = "deck") %>% 
  select(origem, term)

corpus_deck_sent_op30 <- corpus_deck_sent_op30 %>% 
  inner_join(oplexicon30, by = "term") %>% 
  summarise(
    sobre_sent_op30 = sum(polarity),
    sum_words = n()
  )
head(corpus_deck_sent_op30)

# Analise de sentimento do deck usando o "sentiLex_lem_PT02"
corpus_deck_sent_op30 <- rename(corpus_deck, 
                                term = word) %>% 
  mutate(origem = "deck") %>% 
  select(origem, term)

corpus_deck_sent_op30 <- corpus_deck_sent_op30 %>% 
  inner_join(oplexicon30, by = "term") %>% 
  summarise(
    sobre_sent_op30 = sum(polarity),
    sum_words = n()
  )
head(corpus_deck_sent_op30)

# Analise de sentimento do deck usando o "oplexicon_v3.0" e "sentiLex_lem_PT02"
corpus_deck_sent_full <- rename(corpus_deck, 
                                  term = word) %>% 
mutate(origem = "deck") %>% 
  select(origem, term)
View(corpus_deck_sent_full)
corpus_deck_sent_full <- corpus_deck_sent_full %>% 
  inner_join(oplexicon30, by = "term") %>% 
  inner_join(sentiLexPT02 %>% select(term, lex_polarity = polarity), by = "term") %>% 
  group_by(origem) %>% 
  summarise(
    sobre_sent_op30 = sum(polarity),
    sobre_sent_lexPT02 = sum(lex_polarity),
    sum_words = n()
    )
head(corpus_deck_sent_full)

# Histograma de polaridade compardo para os candidatos aprovados e recusados

#Adicionando a coluna de condição
candidatos_linkedin_unnested <- candidatos_linkedin_unnested %>% 
  inner_join(candidatos_linkedin, by = join_by(id_candidato))
candidatos_linkedin_unnested$sobre <- NULL
head(candidatos_linkedin_unnested)

# Definindo dados do gráfico
#Título Geral
title_graph = "Frequencia de polaridades entre candidatos contratados e recusados"
# Excluindo do gráfico análises com baixo número de palavras analisadas
min_terms_analyzed = 3

# Plotar os gráficos
# oplexicon_v3.0
candidatos_linkedin_unnested_graph <- candidatos_linkedin_unnested %>% 
  ggplot(aes(sobre_sent_op30, fill = id_candidato)) +
  geom_histogram(show.legend = FALSE) +
  labs(x = "polaridade", 
       y = "frequência", 
       title = title_graph, 
       subtitle = "oplexicon_v3.0") +
  facet_wrap(~condicao, ncol = 2)
plot(candidatos_linkedin_unnested_graph)

# sentiLex_lem_PT02
candidatos_linkedin_unnested_graph <- candidatos_linkedin_unnested %>% 
  ggplot(aes(sobre_sent_lexPT02, fill = id_candidato)) +
  geom_histogram(show.legend = FALSE) +
  labs(x = "polaridade", 
       y = "frequência", 
       title = title_graph, 
       subtitle = "sentiLex_lem_PT02") +
  facet_wrap(~condicao, ncol = 2)
plot(candidatos_linkedin_unnested_graph)

# oplexicon_v3.0 sum_words >= 3
candidatos_linkedin_unnested_graph <- candidatos_linkedin_unnested %>% 
  filter(sum_words >= min_terms_analyzed)  %>%
  ggplot(aes(sobre_sent_op30, fill = id_candidato)) +
  geom_histogram(show.legend = FALSE) +
  labs(x = "polaridade", 
       y = "frequência", 
       title = title_graph, 
       subtitle = paste("oplexicon_v3.0: número mínimo de termos analisados >=", min_terms_analyzed, "para cada candidato")) +
  facet_wrap(~condicao, ncol = 2)
plot(candidatos_linkedin_unnested_graph)

# sentiLex_lem_PT02 sum_words >= 3
min_terms_analyzed = 3
candidatos_linkedin_unnested_graph <- candidatos_linkedin_unnested %>% 
  filter(sum_words >= min_terms_analyzed)  %>%
  ggplot(aes(sobre_sent_lexPT02, fill = id_candidato)) +
  geom_histogram(show.legend = FALSE) +
  labs(x = "polaridade", 
       y = "frequência", 
       title = "Frequencia de polaridades entre candidatos contratados e recusados", 
       subtitle = paste("sentiLex_lem_PT02: número mínimo de termos analisados >=", min_terms_analyzed, "para cada candidato")) +
  facet_wrap(~condicao, ncol = 2)
plot(candidatos_linkedin_unnested_graph)

# Gráfico de rede de palavras
# Carrega a biblioteca
library(ggraph)
# Prepara os dados
word_pairs_graph <- word_pairs %>%
  filter(n >= 4) # %>%
# Trava a apresentação da nuvem
set.seed(137)
# Plotar o gráfico
ggraph(word_pairs_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, 
                     edge_width = n), 
                 edge_colour = "gray50") +
  geom_node_point() +
  geom_node_text(aes(label = name), 
                 repel = TRUE,
                 point.padding = unit(0.25, "lines"), 
                 vjust = 1, 
                 hjust = 1) + 
  ggtitle('Rede de Palavras do "Deck" de Cultura da NT') + 
  theme_void()

### --->>> Análise usando o lexicon NRC traduzido para o PTBR --->>> START

# Extraindo lexicon de análise de sentimentos
#lexiconPT::get_word_sentiment("temer")
loughran_lex <- get_sentiments("loughran")
bing_lex <- get_sentiments("bing")
afinn_lex <- get_sentiments("afinn")
nrc_lex <- get_sentiments("nrc")
#write.table(loughran_lex, file = "loughran_lex.csv", sep = ";", na = "", quote = TRUE, row.names = FALSE, eol = "\r\n")
# Gerar arquivo CSV para usar na tradução do ncr
write.table(nrc_lex,      file = "nrc_lex.csv",      sep = ";", na = "", quote = TRUE, row.names = FALSE, eol = "\r\n")

# Lendo a base do ncr traduzida
nrc_lex_PTBR_df <- read_excel("nrc_lex_PTBR_df.xlsx",
                              sheet = "nrc_lex_PTBR_df",
                              col_types = c("text", "text", "text", "text", "text", "numeric"))
glimpse(nrc_lex_PTBR_df)
# Rows: 13,872
# Columns: 6
# $ word_eng             <chr> "abacus", "abandon", "abandon", "abandon", "abandoned", "abandoned", "abandoned", "abandoned", "abandonment"…
# $ sentiment_eng        <chr> "trust", "sadness", "negative", "fear", "negative", "sadness", "anger", "fear", "sadness", "surprise", "nega…
# $ word_ptbr            <chr> "ábaco", "abandonar", "abandonar", "abandonar", "abandonado", "abandonado", "abandonado", "abandonado", "aba…
# $ sentiment_ptbr       <chr> "confiança", "tristeza", "negativo", "medo", "negativo", "tristeza", "raiva", "medo", "tristeza", "surpresa"…
# $ context              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
# $ not_to_used_for_ptbr <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#
# Definição das colunas
# word_eng             => termo original do inglês
# sentiment_eng        => sentimento original do inglês
# word_ptbr            => termo traduzido para o português
# sentiment_ptbr       => sentimento traduzido para o português
# context              => explicação do contexto da aplicação do termo em português, e/ou outras possíveis traduções
# not_to_used_for_ptbr => "0" para termos que devem ser revisados para serem usados; "1" para termos que não serão usados.


# Filtrar os termos que não serão comparados (not_to_used_for_ptbr == 0)
nrc_lex_PTBR_df <- nrc_lex_PTBR_df %>%
  filter(not_to_used_for_ptbr == 0)
nrc_lex_PTBR_df <- nrc_lex_PTBR_df %>% 
  dplyr::select(word_ptbr,
                sentiment_ptbr)
glimpse(nrc_lex_PTBR_df) 
# Rows: 13,666
# Columns: 6
# $ word_eng             <chr> "abacus", "abandon", "abandon", "abandon", "abandoned", "abandoned", "abandoned", "abandoned", "abandonment"…
# $ sentiment_eng        <chr> "trust", "sadness", "negative", "fear", "negative", "sadness", "anger", "fear", "sadness", "surprise", "nega…
# $ word_ptbr            <chr> "ábaco", "abandonar", "abandonar", "abandonar", "abandonado", "abandonado", "abandonado", "abandonado", "aba…
# $ sentiment_ptbr       <chr> "confiança", "tristeza", "negativo", "medo", "negativo", "tristeza", "raiva", "medo", "tristeza", "surpresa"…
# $ context              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
# $ not_to_used_for_ptbr <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …

# Separa os tokens
candidatos_linkedin_unnested <- candidatos_linkedin %>%
  tidytext::unnest_tokens(output = term, 
                          input = sobre, 
                          token = "words",
                          to_lower = TRUE)
glimpse(candidatos_linkedin_unnested)

# # Conta o total de palavras no texto "sobre" depois do stop_words
# candidatos_linkedin_unnested_count_words <- candidatos_linkedin_unnested %>%
#   count(id_candidato,
#         sort = FALSE) %>% 
#   rename(words_total = n)
# glimpse(candidatos_linkedin_unnested_count_words)

# Gera o resumo de candidato por sentimento
candidatos_linkedin_unnested_sent <- candidatos_linkedin_unnested %>%
  inner_join(nrc_lex_PTBR_df, join_by(term == word_ptbr)) %>%
  count(id_candidato,
        condicao,
        sentiment_ptbr)
glimpse(candidatos_linkedin_unnested_sent)

# Gera o total de palavras avaliadas com ncr_ptbr
candidatos_linkedin_unnested_sent_count_words <- candidatos_linkedin_unnested %>%
  inner_join(nrc_lex_PTBR_df, join_by(term == word_ptbr)) %>%
  count(id_candidato,
        condicao) %>% 
  rename(words_total = n)
glimpse(candidatos_linkedin_unnested_sent_count_words)

# Adiciona coluna com o número total de palavras "words_total"
candidatos_linkedin_unnested_sent <- candidatos_linkedin_unnested_sent %>%
  left_join(candidatos_linkedin_unnested_sent_count_words)
glimpse(candidatos_linkedin_unnested_sent)

# Calcula o fator
candidatos_linkedin_unnested_sent <- candidatos_linkedin_unnested_sent %>%
  mutate(fator = n/words_total)
glimpse(candidatos_linkedin_unnested_sent)

# Gerar arquivo CSV para ser transposta no excel
write.table(candidatos_linkedin_unnested_sent, 
            file = "candidatos_linkedin_unnested_sent.csv", 
            quote = TRUE, 
            sep = ";", 
            eol = "\r\n",
            na = "", 
            row.names = FALSE,
            fileEncoding = "UTF-8")

# Lendo a base do candidatos_linkedin_unnested_sent transposto
candidatos_linkedin_unnested_sent_transpose <- read_excel("candidatos_linkedin_unnested_sent.xlsx",
                              sheet = "transpose",
                              col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
glimpse(candidatos_linkedin_unnested_sent_transpose)












candidatos_linkedin_unnested_sent_transpose_graph <- candidatos_linkedin_unnested_sent_transpose %>% 
  filter(condicao == "recusado") %>%
  slice_head(n = 3)
# candidatos_linkedin_unnested_sent_transpose <- df[c(candidatos_linkedin_unnested_sent_transpose$antecipação,
#                                                     candidatos_linkedin_unnested_sent_transpose$nojo)]

candidatos_linkedin_unnested_sent_transpose_graph <- candidatos_linkedin_unnested_sent_transpose_graph[,3:12]
candidatos_linkedin_unnested_sent_transpose_graph <- rbind(rep(1,10) , rep(0,10), candidatos_linkedin_unnested_sent_transpose_graph)
radarchart(candidatos_linkedin_unnested_sent_transpose_graph)

# Cores de preenchimento
areas <- c(rgb(1, 0, 0, 0.25),
           rgb(0, 1, 0, 0.25),
           rgb(0, 0, 1, 0.25))

radarchart(candidatos_linkedin_unnested_sent_transpose_graph,
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           pcol = 4:4,      # Color for each line
           plwd = 2,        # Width for each line
           plty = 1,        # Line type for each line
           pfcol = areas)   # Color of the areas
legenda_graph <- candidatos_linkedin_unnested_sent_transpose %>% 
  filter(condicao == "recusado") %>%
  slice_head(n = 3)

legend("topright",
       legend = paste(legenda_graph[1,1],legenda_graph[2,1],legenda_graph[3,1]),
       bty = "n", pch = 20, col = areas,
       text.col = "grey25", pt.cex = 2)

library(ggradar)

candidatos_linkedin_unnested_sent_transpose_ggradar <- candidatos_linkedin_unnested_sent_transpose %>% 
  filter(condicao == "recusado") %>%
  slice_head(n = 3)
candidatos_linkedin_unnested_sent_transpose_ggradar$condicao <- NULL
ggradar(candidatos_linkedin_unnested_sent_transpose_ggradar,
        plot.title = "Candidatos Recusados",
        base.size = 5, 
        values.radar = c(0, 0.5, 1),
        #legend.title = "Candidatos Recusados",
        legend.text.size = 10,
        legend.position = "bottom",
        group.line.width = 0.5,
        group.point.size = 2,
        fill = TRUE,
        fill.alpha = 0.25
        )



data <- as.data.frame(matrix( sample( 2:20 , 10 ,
                                      replace=T) ,
                              ncol=10))

colnames(data) <- c("Mumbai" , "Tamil" , "Noida" ,
                    "Kerala" , "Patna", "Assam" ,
                    "Ranchi" , "Bhopal", "Delhi",
                    "Indore" )

data <- rbind(rep(39,10) , rep(0,10) , data)

# Library
library(fmsb)

# The default radar chart
radarchart(data)














### --->>> Análise usando o lexicon NRC traduzido para o PTBR --->>> END
