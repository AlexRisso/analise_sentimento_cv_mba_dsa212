---
title: "Tratamento do Deck de Cultura"
author: "ASR"
date: "2023-02-08"
output:
  word_document: default
  html_document: default
  pdf_document: default
---




```r
# Ler o deck de cultura
deck_cultura_df <- tibble::tibble(text = readLines("deck de cultura.txt", 
                                                   encoding = 'UTF-8'))
dplyr::summarise(deck_cultura_df)
```

```
## # A tibble: 1 × 0
```

```r
utils::str(deck_cultura_df)
```

```
## tibble [134 × 1] (S3: tbl_df/tbl/data.frame)
##  $ text: chr [1:134] "A nossa Cultura" "Este documento expressa em parte quem já somos, e em parte quem aspiramos ser" "Neste documento apresentamos:" "Nosso Propósito e Nossa Missão" ...
```


```r
# Gerar lista de stopwords
# Stopwords proveniente do Python
stopwords_py <- read.csv("stopwords_pt_py.csv", 
                         encoding = 'UTF-8', 
                         header = TRUE)
# Stopwords proveniente do R
stopwords_R <- tibble::tibble(word = tm::stopwords('pt'))
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
                                        keep = FALSE,
                                        multiple = "all")
dplyr::summarise(stopwords_full_join)
```

```
## data frame with 0 columns and 1 row
```

```r
utils::str(stopwords_full_join)
```

```
## 'data.frame':	222 obs. of  1 variable:
##  $ word: chr  "a" "à" "ao" "aos" ...
```










