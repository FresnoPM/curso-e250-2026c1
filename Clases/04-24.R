library(janeaustenr)
library(stringr) # cheatsheet de regexp: https://opensource.posit.co/resources/cheatsheets/strings/
library(tidytext)
library(dplyr)
library(tidyr) # acá están los lexicones de sentimiento


original_books <- austen_books() |>
    group_by(book) |>
    mutate(
        line = row_number(),
        chapter = cumsum(str_detect(
            text,
            regex("^chapter [\\divxlc]", ignore_case = TRUE)
        ))
    ) |>
    ungroup()

original_books

tidy_books <- original_books |> # parte de la librería de janeausten
    unnest_tokens(output = word, input = text)

cleaned_books <- tidy_books |>
    anti_join(get_stopwords()) # quitamos las palabrass menos informativas, las más frecuentes

cleaned_books |> count(word, sort = TRUE)


positive <- get_sentiments("bing") |> # bing es un categorizador que binariza en sentimientos positivos y negativos
    filter(sentiment == "positive")

tidy_books |>
    filter(book == "Emma") |>
    semi_join(positive) |> # usamos semi_join porque hay muchas palabras que no están categorizadas
    count(word, sort = TRUE)

bing <- get_sentiments("bing")
janeaustensentiment <- tidy_books |>
    inner_join(bing, relationship = "many-to-many") |>
    count(book, index = line %/% 80, sentiment) |>
    pivot_wider(names_from = sentiment, values_from = n, values_fill  = 0) |>
    mutate(sentiment = positive - negative)


library(ggplot2)
ggplot(janeaustensentiment, aes(index,sentiment,fill=book)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    facet_wrap(vars(book), ncol = 2, scales = "free_x")


library(wordcloud)


############################
############################
############################


install.packages("gutenbergr")
library(gutenbergr)

hgwells <- gutenberg_download(c(35,36,5230, 159)) # estos son los ids de los libros que nos interesan, canrga los 4 libros, 2 columnas (is y text) y 20000 filas , una fila por línea de texto de cada libro

tidy_hgwells <- hgwells |>
    unnest_tokens(word, text) |> # desacopla las unidades línea y reagrupa por unidad palabra
    anti_join(stop_words) # elimina las palabras al pedo
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- bronte %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)


tidy_bronte %>%
    count(word, sort = TRUE)


library(tidyr)

frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"),
                       mutate(tidy_books, author = "Jane Austen")) %>%
    mutate(word = str_extract(word, "[a-z']+")) %>%
    count(author, word) %>%
    group_by(author) %>%
    mutate(proportion = n / sum(n)) %>%
    select(-n) %>%
    pivot_wider(names_from = author, values_from = proportion) %>%
    pivot_longer(`Brontë Sisters`:`H.G. Wells`,
                 names_to = "author", values_to = "proportion")

frequency
