library('readxl')
library('dplyr')
library(tidytext)
library(tidyr)
library(ggplot2)
library(ggrepel)

data = read_excel("data/clean_data_final.xlsx")
text_data <- data %>% mutate(text= paste(AB, TI)) %>% select('MYSQL_ID', 'PY','text')
head(text_data)
#

text_bigram <- text_data %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  filter(!is.na(bigram))

text_bigram
#
text_bigram %>% 
  group_by(PY) %>%
  count(bigram, sort = TRUE, .drop = FALSE) 

bigrams_separated <- text_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")
#
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_counts
#
bigrams_united <- bigrams_filtered %>%   
  unite(bigram, word1, word2, sep = " ") %>% 
  group_by(PY) %>%
  count(bigram, sort = TRUE, .drop = FALSE)

bigrams_united
#

remove_terms <- c('based investing','bibliometric analysis','content analysis','design methodology','economic development','esg data','eu sustainable','faith based','finance research','financial knowledge','findings reveal','future research','governance esg','green financial','inclusive growth','investment decisions','literature review','methodology approachthe','national development','paper aims','practical implications','promoting sustainable','rights reserved','study contributes','study examines','study investigates','sustainable financial','valuable insights')

p <- 
  bigrams_united %>% 
  filter(n > 10) %>% 
  filter(! bigram %in% remove_terms) %>% 
  group_by(PY,n) %>%
  summarise(combined_words=paste(bigram,collapse=",\n")) %>%
  ggplot( aes(PY, n, label=combined_words))

#ploting

p +labs(y="frequency of terms", x="year of publish") +geom_point() + geom_text_repel(
  hjust=-0.05, 
  min.segment.length=0.5,
  #segment.colour=NA,
  force_pull = 5,
  position = "identity", 
  max.time = 1,
  max.iter = 10000,
  max.overlaps = getOption("ggrepel.max.overlaps", default = 10),
  size=3, 
  angle=50,
  arrow = NULL
  
) + ylim(25,110)+ xlim(c(2020,2025)) 