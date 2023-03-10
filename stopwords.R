# Instalação e Carregamento dos Pacotes

pacotes <- c("tm", "dplyr")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

stopwords_r <- data.frame(word = c(stopwords('pt')))
stopwords_r

stopwords_py <- read.csv("stopwords_pt_py.csv", encoding = 'UTF-8')
stopwords_py

dif <- anti_join(stopwords_py, stopwords_r, by= 'word')
dif