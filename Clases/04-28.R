# https://www.tidytextmining.com/tfidf (cap 3) Analyzing word and document frequency: tf-idf


install.packages("gutenbergr")
install.packages("tidytext")
library(gutenbergr)
library(dplyr)
library(tidytext)
library(ggplot2)
physics <- gutenberg_download(c(37729, 14725, 13476, 30155), meta_fields = "author")
physics_words <- physics |>
    unnest_tokens(word, text) |>
    count(author, word, sort = TRUE)

plot_physics <- physics_words |>
    bind_tf_idf(word, author, n) |>
    mutate(author = factor(author, levels = c("Galilei, Galileo",
                                              "Huygens, Christiaan",
                                              "Tesla, Nikola",
                                              "Einstein, Albert")))

plot_physics |>
    group_by(author) |>
    slice_max(tf_idf, n = 15) |>
    ungroup() |>
    mutate(word = reorder(word, tf_idf)) |>
    ggplot(aes(tf_idf, word, fill = author)) +
    geom_col(show.legend = FALSE) +
    labs(x = "tf-idf", y = NULL) +
    facet_wrap(~author, ncol = 2, scales = "free")


library(stringr)

physics |>
    filter(str_detect(text, "_k_")) |>
    select(text)


# ... terminarlo to-do

# https://www.tidytextmining.com/ngrams cap 4 Relationships between words: n-grams and correlations

library(dplyr)
library(tidytext)
library(janeaustenr)

austen_bigrams <- austen_books() |>
    unnest_tokens(bigram, text, token = "ngrams", n = 2) |>
    filter(!is.na(bigram))

print(austen_bigrams, n=30)

austen_bigrams %>%
    count(bigram, sort = TRUE)

library(tidyr)

bigrams_separated <- austen_bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ") # separamos cada elemento del n-gram para tratarlos por su parte

bigrams_filtered <- bigrams_separated %>% # filtramos para quedarnos únicamente con aquellas filas donde ninguna de las 2 palabras sea una stop-word
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% # contamos cuántos combos de 2 palabras hay
    count(word1, word2, sort = TRUE)

bigram_counts


bigrams_united <- bigrams_filtered %>%  # volvemos a combinar las dos palabras del bigrama ya filtradas
    unite(bigram, word1, word2, sep = " ")

bigrams_united

# to-do terminar trigrams



# TF - IDF


bigram_tf_idf <- bigrams_united %>%
    count(book, bigram) %>% # bigrams más populares por libro
    bind_tf_idf(bigram, book, n) %>%
    arrange(desc(tf_idf))

print(bigram_tf_idf , n= 30)


bigram_tf_idf <- bigrams_united %>%
    count(book, bigram) %>%
    bind_tf_idf(bigram, book, n) %>%
    arrange(desc(tf_idf))

bigram_tf_idf

# to-do realizar el gráfico mostrado en el tutorial " Figure 4.1: Bigrams with the highest tf-idf from each Jane Austen novel"


#  4.1.4 Visualizing a network of bigrams with ggraph
# un grafo relaciona objetos entre si
# cada elemento es un nodo, puede ser de salida o llegada y la relación entre esos 2 elementos tiene un peso


install.packages("igraph")
library(igraph)

# original counts
bigram_counts


install.packages("ggraph")
library(ggraph)

bigram_graph <- bigram_counts %>%
    filter(n > 20) %>% # sólo quiero los de mayor frecuencia
    graph_from_data_frame()

bigram_graph

set.seed(2017) # pseudo random

ggraph(bigram_graph, layout = "fr") +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1)



# to-do armar un grafo de las que no son stop-words
# to-do terminar capítulo 4
#
# https://www.tidytextmining.com/topicmodeling cap 6 Topic modeling

install.packages("topicmodels") # requirió que instalara primero el paquete "gsl"
library(topicmodels)

data("AssociatedPress") # cada documento tiene una mezcla de varios tópicos y cada tópico tiene una mezcla de palabras (o tokens, que pueden bigramas, n-gramas o lo que sea)
AssociatedPress

