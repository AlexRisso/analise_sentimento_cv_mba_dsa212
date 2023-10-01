# Gera o data frame
candidatos_linkedin <- read_excel("candidatos texto linkedin.xlsx")
# Corrigir encoding do texto da coluna "sobre"
candidatos_linkedin$sobre <- iconv(candidatos_linkedin$sobre, to = "ASCII//TRANSLIT")
# Remover emojis
candidatos_linkedin$sobre <- iconv(candidatos_linkedin$sobre, sub="", 'UTF-8', 'ASCII')
# Remover caracter especial
candidatos_linkedin$sobre <- candidatos_linkedin$sobre %>%
lapply(gsub, pattern = '[^[:alnum:]]', replace = ' ') 
# Visualizar dataframe
dplyr::glimpse(candidatos_linkedin)

# Carregar datasets
data("oplexicon_v3.0")
data("sentiLex_lem_PT02")

oplexicon30 <- oplexicon_v3.0
dplyr::glimpse(oplexicon30)
sentiLexPT02 <- sentiLex_lem_PT02
dplyr::glimpse(sentiLexPT02)

# Criar uma linha para cada palavra de um dos campo "sobre"
candidatos_linkedin_unnested <- candidatos_linkedin %>%
  tidytext::unnest_tokens(term, sobre)
dplyr::glimpse(candidatos_linkedin_unnested)

# Quantificar o sentimento do campo "sobre"
candidatos_linkedin_unnested_op30_senPT02 <- candidatos_linkedin_unnested %>% 
  inner_join(oplexicon30, by = join_by(term)) %>% 
  inner_join(sentiLexPT02 %>% 
              select(term, lex_polarity = polarity), by = join_by(term)) %>% 
  select(id_candidato, term, polarity, lex_polarity) %>%
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
dplyr::glimpse(candidatos_linkedin_unnested_op30_senPT02)
# Plotar o gráfico
linkedin_sent_graph <- candidatos_linkedin_unnested_op30_senPT02 %>% 
  ggplot(aes(x = sobre_sent_op30, y = sobre_sent_lexPT02)) +
  geom_point(aes(color = sum_words)) + 
  scale_color_continuous(low = "green", high = "red") +
  labs(x = "Polaridade no OpLexicon", y = "Polaridade no SentiLex") +
  #geom_smooth(method = "lm") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed")
plot(linkedin_sent_graph)

# Tão somente somar a polaridade de cada palavra não é um bom método.
# Existem anomalias nos dados, pois as mesmas palavras possuem sentimentos 
# diferentes de acordo com o léxicon usado.
# O conteúdo mais positivo dos textos dos candidatos
most_pos <- which.max(candidatos_linkedin_unnested_op30_senPT02$most_pos)
most_neg <- which.min(candidatos_linkedin_unnested_op30_senPT02$most_neg)
# Conteúdo mais positivo
most_pos_sobre <- 
  dplyr::filter(candidatos_linkedin,
                id_candidato == candidatos_linkedin_unnested_op30_senPT02$id_candidato[most_pos])
cat(unlist(most_pos_sobre$sobre[1]))
cat(most_pos_sobre$id_candidato[1])
# Conteúdo mais negativo
most_neg_sobre <- 
  dplyr::filter(candidatos_linkedin,
                id_candidato == candidatos_linkedin_unnested_op30_senPT02$id_candidato[most_neg])
cat(unlist(most_neg_sobre$sobre[1]))
cat(most_neg_sobre$id_candidato[1])

# Resumo do deck - número de termos após o processamento do stopwords
dplyr::summarise(corpus_deck, 
                 word=n(),
                 sort = TRUE)
# Agrupando os termos
corpus_deck_sent_sum <- corpus_deck %>% 
  dplyr::count(word)
dplyr::glimpse(corpus_deck_sent_sum)
# Quantidade de termos após agrupamento
dplyr::summarise(corpus_deck_sent_sum, word=n())

# Analise de sentimento do deck usando o "oplexicon_v3.0"
corpus_deck_sent_op30 <- dplyr::rename(corpus_deck,
                                       term = word) %>% 
  mutate(origem = "deck") %>% 
  select(origem, term)

corpus_deck_sent_op30 <- corpus_deck_sent_op30 %>% 
  inner_join(oplexicon30, by = join_by(term)) %>% 
  summarise(
    sobre_sent_op30 = sum(polarity),
    sum_words = n()
  )
dplyr::glimpse(corpus_deck_sent_op30)

# Analise de sentimento do deck usando o "sentiLex_lem_PT02"
corpus_deck_sent_lexpt02 <- dplyr::rename(corpus_deck,
                                          term = word) %>% 
  mutate(origem = "deck") %>% 
  select(origem, term)

corpus_deck_sent_lexpt02 <- corpus_deck_sent_lexpt02 %>% 
  inner_join(sentiLexPT02, by = join_by(term)) %>% 
  summarise(
    sobre_sent_lexpt02 = sum(polarity),
    sum_words = n()
  )
dplyr::glimpse(corpus_deck_sent_lexpt02)

# Termos que são compartilhados simultaneamente por “oplexicon_v3.0”, “sentiLex_lem_PT02” e no “deck”
corpus_deck_sent_full <- dplyr::rename(corpus_deck,
                                       term = word) %>%
  mutate(origem = "deck") %>%
  select(origem, term)
dplyr::glimpse(corpus_deck_sent_full)
corpus_deck_sent_full <- corpus_deck_sent_full %>% 
  inner_join(oplexicon30, by = join_by(term)) %>% 
  inner_join(sentiLexPT02 %>% select(term, lex_polarity = polarity), by = join_by(term)) %>% 
  group_by(origem) %>% 
  summarise(
    sobre_sent_op30 = sum(polarity),
    sobre_sent_lexPT02 = sum(lex_polarity),
    sum_words = n()
    )
dplyr::glimpse(corpus_deck_sent_full)

# Histograma de polaridade compardo para os candidatos contratados e recusados
#Adicionando a coluna de condição ao candidatos_linkedin_unnested_op30_senPT02
candidatos_linkedin_unnested_op30_senPT02 <- candidatos_linkedin_unnested_op30_senPT02 %>% 
  inner_join(candidatos_linkedin, by = join_by(id_candidato))
candidatos_linkedin_unnested_op30_senPT02$sobre <- NULL
dplyr::glimpse(candidatos_linkedin_unnested_op30_senPT02)
# Definindo dados do gráfico
#Título Geral
title_graph = "Frequencia de polaridades entre candidatos contratados e recusados"
# Plotar os gráficos
# oplexicon_v3.0
candidatos_linkedin_unnested_op30_senPT02_graph <- candidatos_linkedin_unnested_op30_senPT02 %>% 
  ggplot(aes(sobre_sent_op30, fill = id_candidato)) +
  geom_histogram(show.legend = FALSE) +
  labs(x = "polaridade", 
       y = "frequência", 
       title = title_graph, 
       subtitle = "oplexicon_v3.0") +
  facet_wrap(~condicao, ncol = 2)
plot(candidatos_linkedin_unnested_op30_senPT02_graph)

# sentiLex_lem_PT02
candidatos_linkedin_unnested_op30_senPT02_graph <- candidatos_linkedin_unnested_op30_senPT02 %>% 
  ggplot(aes(sobre_sent_lexPT02, fill = id_candidato)) +
  geom_histogram(show.legend = FALSE) +
  labs(x = "polaridade", 
       y = "frequência", 
       title = title_graph, 
       subtitle = "sentiLex_lem_PT02") +
  facet_wrap(~condicao, ncol = 2)
plot(candidatos_linkedin_unnested_op30_senPT02_graph)

# Excluindo do gráfico análises com baixo número de palavras analisadas
min_terms_analyzed = 3
# oplexicon_v3.0 sum_words >= 3
candidatos_linkedin_unnested_op30_senPT02_graph <- candidatos_linkedin_unnested_op30_senPT02 %>% 
  filter(sum_words >= min_terms_analyzed)  %>%
  ggplot(aes(sobre_sent_op30, fill = id_candidato)) +
  geom_histogram(show.legend = FALSE) +
  labs(x = "polaridade", 
       y = "frequência", 
       title = title_graph, 
       subtitle = paste("oplexicon_v3.0: termos com frequência >=", min_terms_analyzed, "em cada candidato")) +
  facet_wrap(~condicao, ncol = 2)
plot(candidatos_linkedin_unnested_op30_senPT02_graph)

# sentiLex_lem_PT02 sum_words >= 3
candidatos_linkedin_unnested_op30_senPT02_graph <- candidatos_linkedin_unnested_op30_senPT02 %>% 
  filter(sum_words >= min_terms_analyzed)  %>%
  ggplot(aes(sobre_sent_lexPT02, fill = id_candidato)) +
  geom_histogram(show.legend = FALSE) +
  labs(x = "polaridade", 
       y = "frequência", 
       title = "Frequencia de polaridades entre candidatos contratados e recusados", 
       subtitle = paste("sentiLex_lem_PT02: termos com frequência >=", min_terms_analyzed, "em cada candidato")) +
  facet_wrap(~condicao, ncol = 2)
plot(candidatos_linkedin_unnested_op30_senPT02_graph)

# Gráfico de rede de palavras
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
write.table(nrc_lex, file = "nrc_lex.csv", sep = ";", na = "", quote = TRUE, row.names = FALSE, eol = "\r\n")

# Lendo a base do ncr traduzida
nrc_lex_PTBR_df <- read_excel("nrc_lex_PTBR_df.xlsx",
                              sheet = "nrc_lex_PTBR_df",
                              col_types = c("text", "text", "text", "text", "text", "numeric"))
dplyr::glimpse(nrc_lex_PTBR_df)
# Definição das colunas
# word_eng             => termo original do inglês
# sentiment_eng        => sentimento original do inglês
# word_ptbr            => termo traduzido para o português
# sentiment_ptbr       => sentimento traduzido para o português
# context              => explicação do contexto da aplicação do termo em português, e/ou outras possíveis traduções
# not_to_used_for_ptbr => "0" para termos que devem ser revisados para serem usados; "1" para termos que não serão usados.

# Filtrar os termos que serão comparados (not_to_used_for_ptbr == 0)
nrc_lex_PTBR_df <- nrc_lex_PTBR_df %>%
  filter(not_to_used_for_ptbr == 0)
nrc_lex_PTBR_df <- nrc_lex_PTBR_df %>% 
  dplyr::select(word_ptbr,
                sentiment_ptbr)
dplyr::glimpse(nrc_lex_PTBR_df) 

# Separa os tokens
candidatos_linkedin_unnested_nrc <- candidatos_linkedin %>%
  tidytext::unnest_tokens(output = term, 
                          input = sobre, 
                          token = "words",
                          to_lower = TRUE)
dplyr::glimpse(candidatos_linkedin_unnested_nrc)

# Gera o resumo de candidato por sentimento
candidatos_linkedin_unnested_sent <- candidatos_linkedin_unnested_nrc %>%
  inner_join(nrc_lex_PTBR_df, join_by(term == word_ptbr)) %>%
  count(id_candidato,
        condicao,
        sentiment_ptbr)
dplyr::glimpse(candidatos_linkedin_unnested_sent)

# Gera o total de palavras avaliadas com ncr_ptbr
candidatos_linkedin_unnested_sent_count_words <- candidatos_linkedin_unnested_nrc %>%
  inner_join(nrc_lex_PTBR_df, join_by(term == word_ptbr)) %>%
  count(id_candidato,
        condicao) %>% 
  dplyr::rename(words_total = n)
dplyr::glimpse(candidatos_linkedin_unnested_sent_count_words)

# Adiciona coluna com o número total de palavras "words_total"
candidatos_linkedin_unnested_sent <- candidatos_linkedin_unnested_sent %>%
  left_join(candidatos_linkedin_unnested_sent_count_words)
dplyr::glimpse(candidatos_linkedin_unnested_sent)

# Calcula o fator
candidatos_linkedin_unnested_sent <- candidatos_linkedin_unnested_sent %>%
  mutate(fator = n/words_total)
dplyr::glimpse(candidatos_linkedin_unnested_sent)

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
dplyr::glimpse(candidatos_linkedin_unnested_sent_transpose)

# CONTRATADOS
# Filtrando a base para amostra de candidatos contratados
candidatos_linkedin_unnested_sent_transpose_graph <- candidatos_linkedin_unnested_sent_transpose %>% 
  filter(condicao == "contratado") %>%
  slice_head(n = 3)
# Removendo coluna para possibilitar a plotagem do gráfico radar
candidatos_linkedin_unnested_sent_transpose_graph$condicao <- NULL
# Alterando nome da coluna id_candidato
candidatos_linkedin_unnested_sent_transpose_graph <- candidatos_linkedin_unnested_sent_transpose_graph %>%
  dplyr::rename(candidato = id_candidato)
# Reordenando as colunas para padronizar a visualização do gráfico
candidatos_linkedin_unnested_sent_transpose_graph <- 
  candidatos_linkedin_unnested_sent_transpose_graph[,
                                                    c("candidato",
                                                      "antecipação",
                                                      "confiança",
                                                      "negativo",
                                                      "positivo",
                                                      "tristeza",
                                                      "alegria",
                                                      "medo",
                                                      "raiva",
                                                      "surpresa",
                                                      "nojo")]
# Plotar gráfico
library(ggradar)
ggradar(candidatos_linkedin_unnested_sent_transpose_graph,
        plot.title = "Candidatos Contratados - Amostra - Análise nrc_PTBR",
        base.size = 20, 
        values.radar = c(0, 0.5, 1),
        grid.label.size = 4,
        legend.text.size = 15,
        legend.position = "bottom",
        group.line.width = 0.75,
        group.point.size = 2,
        group.colours = c("coral", "cyan", "gray25"),
        fill = TRUE,
        fill.alpha = 0.25,
        )

# RECUSADOS
# Filtrando a base para amostra de candidatos recusados
candidatos_linkedin_unnested_sent_transpose_graph <- candidatos_linkedin_unnested_sent_transpose %>% 
  filter(condicao == "recusado") %>%
  slice_head(n = 3)
# Removendo coluna para possibilitar a plotagem do gráfico radar
candidatos_linkedin_unnested_sent_transpose_graph$condicao <- NULL
# Alterando nome da coluna id_candidato
candidatos_linkedin_unnested_sent_transpose_graph <- candidatos_linkedin_unnested_sent_transpose_graph %>%
  dplyr::rename(candidato = id_candidato)
# Reordenando as colunas para padronizar a visualização do gráfico
candidatos_linkedin_unnested_sent_transpose_graph <- 
  candidatos_linkedin_unnested_sent_transpose_graph[,
                                                    c("candidato",
                                                      "antecipação",
                                                      "confiança",
                                                      "negativo",
                                                      "positivo",
                                                      "tristeza",
                                                      "alegria",
                                                      "medo",
                                                      "raiva",
                                                      "surpresa",
                                                      "nojo")]
# Plotar gráfico
ggradar(candidatos_linkedin_unnested_sent_transpose_graph,
        plot.title = "Candidatos Recusados - Amostra - Análise nrc_PTBR",
        base.size = 20, 
        values.radar = c(0, 0.5, 1),
        grid.label.size = 4,
        legend.text.size = 15,
        legend.position = "bottom",
        group.line.width = 0.75,
        group.point.size = 2,
        group.colours = c("coral", "cyan", "gray25"),
        fill = TRUE,
        fill.alpha = 0.25
        )

# Analise geral de "contratados" e "recusados"
# Gera o resumo por sentimento
candidatos_linkedin_unnested_sent_geral <- candidatos_linkedin_unnested_nrc %>%
  inner_join(nrc_lex_PTBR_df, join_by(term == word_ptbr)) %>%
  count(condicao,
        sentiment_ptbr)
dplyr::glimpse(candidatos_linkedin_unnested_sent_geral)
# Gera o total de palavras avaliadas com ncr_ptbr
candidatos_linkedin_unnested_sent_count_words_geral <- candidatos_linkedin_unnested_nrc %>%
  inner_join(nrc_lex_PTBR_df, join_by(term == word_ptbr)) %>%
  count(condicao) %>% 
  dplyr::rename(words_total = n)
dplyr::glimpse(candidatos_linkedin_unnested_sent_count_words_geral)
# Adiciona coluna com o número total de palavras "words_total"
candidatos_linkedin_unnested_sent_geral <- candidatos_linkedin_unnested_sent_geral %>%
  left_join(candidatos_linkedin_unnested_sent_count_words_geral)
dplyr::glimpse(candidatos_linkedin_unnested_sent_geral)
# Calcula o fator
candidatos_linkedin_unnested_sent_geral <- candidatos_linkedin_unnested_sent_geral %>%
  mutate(fator = n/words_total)
dplyr::glimpse(candidatos_linkedin_unnested_sent_geral)
# Gerar arquivo CSV para ser transposta no excel
write.table(candidatos_linkedin_unnested_sent_geral, 
            file = "candidatos_linkedin_unnested_sent_geral.csv", 
            quote = TRUE, 
            sep = ";", 
            eol = "\r\n",
            na = "", 
            row.names = FALSE,
            fileEncoding = "UTF-8")
# Lendo a base do candidatos_linkedin_unnested_sent transposto
candidatos_linkedin_unnested_sent_geral_transpose <- read_excel("candidatos_linkedin_unnested_sent_geral.xlsx",
                                                          sheet = "transpose",
                                                          col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
# Reordenando as colunas para padronizar a visualização do gráfico
candidatos_linkedin_unnested_sent_geral_transpose <- 
  candidatos_linkedin_unnested_sent_geral_transpose[,
                                                    c("condicao",
                                                      "antecipação",
                                                      "confiança",
                                                      "negativo",
                                                      "positivo",
                                                      "tristeza",
                                                      "alegria",
                                                      "medo",
                                                      "raiva",
                                                      "surpresa",
                                                      "nojo")]
dplyr::glimpse(candidatos_linkedin_unnested_sent_geral_transpose)
# Plotar gráfico
ggradar(candidatos_linkedin_unnested_sent_geral_transpose,
        plot.title = "Dados Gerais de Candidatos - Análise nrc_PTBR",
        base.size = 20, 
        values.radar = c(0, 0.25, 0.5),
        grid.min = 0,
        grid.mid = 0.25,
        grid.max = 0.5,
        grid.label.size = 4,
        legend.text.size = 15,
        legend.position = "bottom",
        group.line.width = 0.75,
        group.point.size = 2,
        group.colours = c("coral", "cyan"),
        fill = TRUE,
        fill.alpha = 0.25,
        )

# Análise de sentimento com nrc para o Deck de Cultura
# Usaremos o 'corpus_deck' já preparado
# Gera o resumo de candidato por sentimento
corpus_deck_sent <- corpus_deck %>%
  inner_join(nrc_lex_PTBR_df, join_by(word == word_ptbr)) %>%
  count(sentiment_ptbr)
dplyr::glimpse(corpus_deck_sent)
# Gera o total de palavras avaliadas com ncr_ptbr
corpus_deck_sent_count_words <- sum(corpus_deck_sent$n) 
# Calcula o fator
corpus_deck_sent <- corpus_deck_sent %>%
  mutate(fator = n/corpus_deck_sent_count_words)
dplyr::glimpse(corpus_deck_sent)
# Deve ser igual a'1'
sum(corpus_deck_sent$fator)
# Ajustando o df para gráfico radar
corpus_deck_sent$n <- NULL
# Função para transpor um df tipo "tibble"
# https://stackoverflow.com/questions/28917076/transposing-data-frames/28917212#28917212
tibble_df_transpose <- function(tibble_df) {
  
  tibble_df %>% 
    tidyr::pivot_longer(-1) %>%
    tidyr::pivot_wider(names_from = 1, values_from = value)
  
}
# Aplicando a função
corpus_deck_sent_t <- tibble_df_transpose(corpus_deck_sent)
# Excluindo colua não necessária para o gráfico
#corpus_deck_sent_t$name <- NULL
# Reordenando as colunas para padronizar a visualização do gráfico
corpus_deck_sent_t <- corpus_deck_sent_t[,
                                         c("name",
                                           "antecipação",
                                           "confiança",
                                           "negativo",
                                           "positivo",
                                           "tristeza",
                                           "alegria",
                                           "medo",
                                           "raiva",
                                           "surpresa",
                                           "nojo")]
# Plotar gráfico
ggradar(corpus_deck_sent_t,
        plot.title = "Deck de Cultura - Análise nrc_PTBR",
        base.size = 20, 
        values.radar = c(0, 0.25, 0.5),
        grid.min = 0,
        grid.mid = 0.25,
        grid.max = 0.5,
        grid.label.size = 4,
        plot.legend = FALSE,
        group.line.width = 0.75,
        group.point.size = 2,
        group.colours = c("coral"),
        fill = TRUE,
        fill.alpha = 0.25,
        )

### --->>> Análise usando o lexicon NRC traduzido para o PTBR --->>> END

### --->>> Análise a sessão "Sobre" para o grupo de candidatos contratados e recusados --->>> Start
# Conta número de palavras da sessão "Sobre"
candidatos_unnested_count <- candidatos_linkedin %>%
  unnest_tokens(term, sobre) %>% 
  dplyr::count(id_candidato,condicao)
candidatos_unnested_count <- candidatos_unnested_count %>%
  dplyr::rename(Condição = condicao)
dplyr::glimpse(candidatos_unnested_count, 5)

# Média de contagem de palavras para o grupo de candidatos contratados e recusados
candidatos_unnested_m <- candidatos_unnested_count %>%
  group_by(Condição) %>%
  dplyr::summarize(n = mean(n, na.rm=TRUE))
dplyr::glimpse(candidatos_unnested_m)
# Quantidade de candidatos contratados e recusados
candidatos_condicao <- candidatos_unnested_count %>%
  mutate(count = 1) %>% 
  dplyr::count(Condição,count)
candidatos_condicao$count <- NULL
dplyr::glimpse(candidatos_condicao)

# Plota o histograma
ggplot(candidatos_unnested_count, aes(x=n, color=Condição, fill=Condição)) + 
  geom_histogram(aes(y=..density..), 
                 alpha=0.5, 
                 position="identity",
                 bins = 15) +
  geom_density(alpha=0.2)+
  facet_grid(Condição ~ .) +
  theme(legend.title = element_text(face = "bold"), legend.position="none") + 
  labs(x = "número de palavras (-stowords) na sessão 'Sobre'",
       y = "Contagem") + 
  scale_color_manual(values=c("coral", "cyan")) +
  scale_fill_manual(values=c("coral", "cyan"))
### --->>> Análise a sessão "Sobre" para o grupo de candidatos contratados e recusados --->>> END