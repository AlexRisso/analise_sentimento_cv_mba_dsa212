# Instalação e Carregamento dos Pacotes
pacotes <- c("tidytext","ggplot2","dplyr","tibble","gutenbergr","wordcloud",
             "stringr","SnowballC","widyr","janeaustenr","lexiconPT",
             "tidyr","readxl","tm","e1071","gmodels","caret","reshape2")

# lexiconPT: Pacote para análise em português

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

devtools::install_github("ropensci/gutenbergr")

##################################################################################
# caso ocorra erro no pacote
# tentar instalar: 
# install.packages("devtools")
# e na sequencia:
# devtools::install_github("ropensci/gutenbergr", force=TRUE)
# Referências:
# https://cran.r-project.org/src/contrib/Archive/gutenbergr/
# https://www.rdocumentation.org/packages/gutenbergr/versions/0.2.1
# https://www.gutenberg.org/
# https://ladal.edu.au/gutenberg.html
# https://github.com/r-lib/devtools
# OPICIONAL: https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html
##################################################################################

# Preparando dados do Deck de Cultura do zero
# deck <- tibble::tibble(text = readLines("deck de cultura.txt", 
#                                         encoding = 'UTF-8')) %>% 
#   mutate(text_id=1, "text") %>% 
#   select(text_id, text)

# Preparando variáveis
deck_index = 1
candidato_index = 2

# Aproveitando o tratamento do Deck de Cultura executado no módulo específico
deck <- deck_cultura_df %>% 
  # Adiciona coluna text_id com o valor fixo 'deck_index'
  mutate(text_id = deck_index, "text") %>% 
  # Altera a ordem das colunas
  select(text_id, text)


# Tratamento do texto do candidato
# Coletar dados do arquivo
candidato <- tibble::tibble(text = readLines("candidato exemplo.txt", 
                                             encoding = 'UTF-8'))  %>% 
  # Adicionar coluna text_id com o valor fixo 'candidato_index'
  mutate(text_id = candidato_index, "text") %>% 
  # Alterar a ordem das colunas
  select(text_id, text)

# Ajustar o texto do candidato
candidato$text <- candidato$text %>%
  # Aplicar função tolower a todas as linhas no tibble
  lapply(tolower) %>%
  # Remover caracter especial
  lapply(gsub, pattern = '[^[:alnum:]]', replace = ' ') %>%
  # Remover "não palavras"
  lapply(gsub, pattern = '\\W', replace = ' ') %>%
  # Remover dígitos
  lapply(gsub, pattern = '\\d', replace = '') %>%
  # Remover stopwords
  lapply(tm::removeWords, stopwords_full_join$word) %>%
  # Caso necessite remover acentuação do português
  #lapply(iconv, from = 'UTF-8', to = 'ASCII//TRANSLIT') %>%
  lapply(tm::stripWhitespace) %>%
  # Multiplos espaços em branco são convertidos em um único espaço
  trimws()

# Agrupando
deck_candidato <- bind_rows(deck, candidato)

#Unnest tokes para análise
book_words <- deck_candidato %>%
  unnest_tokens(word,
                text,
                token = "words") %>%
  count(text_id, word, sort = TRUE)

# TF-IDF candidato vs deck
# O dados do deck e candidato já estão pré-processados
books_tf_idf <- book_words %>% bind_tf_idf(word, text_id, n)

# Separar para ver palavras mais importantes por texto
deck_tf_idf <- books_tf_idf %>% filter(text_id == deck_index)
candidato_tf_idf <- books_tf_idf %>% filter(text_id == candidato_index)

# Plotar gráfico com palavras mais importantes
books_graph <- books_tf_idf %>% 
  group_by(text_id) %>% 
  slice_max(tf_idf, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) 

# Ajuste de variáveis
books_graph <- mutate(books_graph, 
                      text_id = replace(text_id, text_id==deck_index, "deck"), 
                      text_id = replace(text_id, text_id==candidato_index, "candidato"))

books_graph %>% ggplot(aes(tf_idf, word, fill = text_id)) +
  geom_col(show.legend = FALSE) +
  labs(x = "TF-IDF", y = NULL) +
  facet_wrap(~text_id, ncol = 2, scales = "free")

# Avaliar Lei de ZIPF: 
# A frequência com que uma palavra aparece é inversamente proporcional a sua classificação.
deck_candidato_zipf <- deck_candidato %>%
  unnest_tokens(word,
                text,
                token = "words") %>%
  count(text_id, word, sort = TRUE) %>%
  ungroup()

# Calcular o total no texto do candidato e do deck
deck_candidato_zipf_total <- deck_candidato_zipf %>%
  group_by(text_id) %>%
  summarize(total = sum(n))

# Fazer merge da base com o total
deck_candidato_zipf <- left_join(deck_candidato_zipf, 
                                 deck_candidato_zipf_total)

# Ajustar valores das variáveis
books_graph <- mutate(deck_candidato_zipf, 
                      text_id = replace(text_id, text_id==deck_index, "deck"), 
                      text_id = replace(text_id, text_id==candidato_index, "candidato"))

# Plotar o gráfico
books_graph %>% 
  ggplot(aes(n/total, fill = text_id)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.05) +
  labs(x = "frequência/total", 
       y = "contagem de termos") +
  #facet_wrap(~text_id, ncol = 2, scales = "free_y")
  facet_wrap(~text_id, ncol = 2)

# Zipf’s Law
books_freq_by_rank <- books_graph %>%
  group_by(text_id) %>%
  mutate(rank = row_number(),
         frequencia = n/total)

books_freq_by_rank %>%
  ggplot(aes(x = rank, 
             y = frequencia, 
             color = text_id)) +
  labs(x = "classificação", 
       y = "frequência dos termos", 
       color= "Texto") +
  theme(legend.title = element_text(face = "bold")) + 
  geom_line(size = 1, 
            alpha = 1) +
  scale_x_log10() +
  scale_y_log10()

# Teste da lei de Zipf
# A lei de Zipf clássica têm classificação de frequência de ~ (1/rank)
# A inclinação foi ~0,5 para o deck
books_freq_by_rank_subset_candidato <- books_freq_by_rank %>%
  filter(text_id == "candidato")
lm(log10(frequencia) ~ log10(rank), data = books_freq_by_rank_subset_candidato)
# Coefficients:
# (Intercept)  log10(rank)  
#     -1.706       -0.329  
books_freq_by_rank_subset_deck <- books_freq_by_rank %>%
  filter(text_id == "deck")
lm(log10(frequencia) ~ log10(rank), data = books_freq_by_rank_subset_deck)
# Coefficients:
# (Intercept)  log10(rank)  
#     -1.6774      -0.5324  

# Plotar o gráfico
books_freq_by_rank_graph <- books_freq_by_rank %>%
  ggplot(aes(x = rank, 
             y = frequencia, 
             colour=text_id)) +
  geom_line(size = 1.5, 
            alpha = 1) + 
  labs(x = "classificação", 
       y = "frequência dos termos", 
       colour = "Texto", 
       title = "Lei de Zipf") + 
  theme(legend.title = element_text(face = "bold")) + 
  scale_x_log10() + 
  scale_y_log10() + 
  # geom_abline(aes(intercept = -1.706, 
  #                 slope = -0.329, 
  #                 colour = paste("Zipf regressão","\n","para o candidato")), 
  #             size = 1, 
  #             linetype = 3) + 
  geom_smooth(data = books_freq_by_rank_subset_candidato,
              method = "lm", 
              formula = y ~ x, 
              se = FALSE, 
              aes(x = rank, 
                  y = frequencia, 
                  colour = paste("Zipf regressão","\n","para o candidato")),
              size = 1, 
              linetype = 2) +  
  # geom_abline(aes(intercept = -1.6774, 
  #                 slope = -0.5324, 
  #                 colour = paste("Zipf regressão","\n","para o deck")), 
  #             size = 1, 
  #             linetype = 2) + 
  geom_smooth(data = books_freq_by_rank_subset_deck,
              method = "lm", 
              formula = y ~ x, 
              se = FALSE, 
              aes(x = rank, 
                  y = frequencia, 
                  colour = paste("Zipf regressão","\n","para o deck")),
              size = 1, 
              linetype = 2) + 
  scale_color_manual(values = c("blue","green","blue","green"))
plot(books_freq_by_rank_graph)

books_tf_idf