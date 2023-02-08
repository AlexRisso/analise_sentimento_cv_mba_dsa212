# Instalação e Carregamento dos Pacotes

pacotes <- c("tm", "dplyr", "wordcloud", "stringr", "tidyverse", "tidytext", "tibble", "utils")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

### --->>> Tratamento do Deck De Cultura NTConsult  --->>> START

# Ler o deck de cultura
deck_cultura_df <- tibble::tibble(text = readLines("deck de cultura.txt", 
                                                   encoding = 'UTF-8'))

# Gerar lista de stopwords
# Stopwords proveniente do Python
stopwords_py <- read.csv("stopwords_pt_py.csv", 
                         encoding = 'UTF-8', 
                         header = TRUE)
# Stopwords proveniente do R
stopwords_R <- tibble::tibble(word = stopwords('pt'))
# Criar arquivo CSV para o stopwords do R
write.csv(stopwords_R, "stopwords_pt_r.csv", row.names=FALSE)
# Stopwords proveniente de alopes (github), no link:
# https://gist.github.com/alopes/5358189#file-stopwords-txt
stopwords_alopes <- read.csv("stopwords_pt_alopes.csv",
                             encoding = 'UTF-8',
                             header = TRUE)
# Unir as listas de stopwords
stopwords_full_join <- dplyr::full_join(stopwords_py,
                                        stopwords_R,
                                        by = "word",
                                        keep = FALSE)
stopwords_full_join <- dplyr::full_join(stopwords_full_join,
                                        stopwords_alopes,
                                        by = "word",
                                        keep = FALSE)

# Tratamento do Deck de Cultura
deck_cultura_df$text <- deck_cultura_df$text %>%
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

# Criar corpus_deck separando cada palavra em um novo registro
corpus_deck <- deck_cultura_df %>% 
               tidytext::unnest_tokens(word,
                                       text,
                                       token = "words")
# Contar aparições de palavras usando uma função dplyr
word_count <- corpus_deck %>% 
              dplyr::count(word,
                           sort = TRUE)

# Criar uma nuvem de palavras usando uma função wordcloud
# Define a paleta de cores
pal <- brewer.pal(8,"Dark2")
# Alternativa de paleta de cores
#pal <- rainbow(3)
# Número máximo de palavras no wordcloud e no dicionário de sentimentos
max_words <- 25
# Nuvem de palavras
wordcloud::wordcloud(corpus_deck$word,
                     random.order = FALSE,
                     colors = pal,
                     scale = c(3,.4),
                     max.words = max_words)

# Gerar ngrams para ser usado na análise de sentimentos
# Número de palavras no ngram
k_grams <- 2
# Gerar ngram baseada em k_grams
kgrams <- deck_cultura_df %>% 
          tidytext::unnest_tokens(output = word, 
                                  input = text, 
                                  token = 'ngrams', 
                                  n = k_grams) %>%
          tidyr::separate(word, c('word1', 'word2'), sep = ' ') %>%
          dplyr::filter(!word1 %in% stopwords_full_join) %>%
          dplyr::filter(!word2 %in% stopwords_full_join) %>%
          tidyr::unite(word, word1, word2, sep = ' ', na.rm = TRUE) %>%
          dplyr::count(word, sort = TRUE)

# Multiplos espaços em branco são convertidos em um único espaço
kgrams$word <- kgrams$word %>% trimws()
# Remove "NA" e registros vazios
kgrams <- kgrams[!apply(is.na(kgrams) | kgrams$word == "", 1, all),]

# Número de ngram no dicionário de sentimentos
max_ngram <- 10

# Gerar dicionário para análise de sentimentos
# Adicionar ao dicionário de sentimentos as "max_words" palavras que mais aparecem
as_dictionary <- tibble::tibble(line = 1:max_words,
                             text = word_count$word[1:max_words])
# Adicionar ao dicionário de sentimentos os "max_ngram" kgrams que mais aparecem
as_dictionary <- as_dictionary %>%
              tibble::add_row(line = (max_words+1):(max_words+max_ngram),
                              text = kgrams$word[1:max_ngram])
as_dictionary <- as_dictionary %>% 
              tibble::add_column(value = 0)

# Definir valor "as_value_ngrams" de sentimento para os ngrams
as_value_ngrams <- 5
as_dictionary$value[(max_words+1):(max_words+max_ngram)] <- as_value_ngrams

# Definir valor "as_value_main_words" de sentimento para as palavras
as_value_main_words <- 4
as_dictionary$value[1:max_words] <- as_value_main_words
# Criar valor aleatório entre 0 e 4
#as_dictionary$value[1:max_words] <- floor(runif(max_words, min = 0, max = 4))

# Criar arquivo CSV para o dicionário
write.csv(as_dictionary, "as_dictionary.csv", row.names=FALSE)

dplyr::summarise(as_dictionary, observações=n())
utils::str(as_dictionary)

### --->>> Tratamento do Deck De Cultura NTConsult  --->>> END