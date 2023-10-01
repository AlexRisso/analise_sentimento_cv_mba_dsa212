# Instalação e Carregamento dos Pacotes
pacotes <- c("tm", "dplyr","wordcloud","stringr","tidyverse","tidytext",
             "tibble","utils","widyr","janeaustenr","ggplot2","gutenbergr",
             "SnowballC","lexiconPT","tidyr","readxl","gmodels","reshape2",
             "ggExtra","magrittr","lubridate","fmsb","ggraph")

# lexiconPT: Pacote para análise em português
# ver https://sillasgonzaga.github.io/2017-09-23-sensacionalista-pt01/
# https://r-charts.com/ranking/ggradar/

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#devtools::install_github("ropensci/gutenbergr")
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