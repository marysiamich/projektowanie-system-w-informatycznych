
#' ---
#' title: "Analiza sentymentu oraz TF-IDF aplikacji randkowych"
#' author: "Katarzyna Zrobek, Barbara Sawa, Maria Mich "
#' date:   "07.06_2026"
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: lumen      # Wygląd (bootstrap, cerulean, darkly, journal, lumen, paper, readable, sandstone, simplex, spacelab, united, yeti)
#'     highlight: kate      # Kolorowanie składni (haddock, kate, espresso, breezedark)
#'     toc: true            # Spis treści
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: hide    
#'     number_sections: false # Numeruje nagłówki (lepsza nawigacja)
#' ---


knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)


df <- read.csv("dating_app.csv")

# wymagane pakiety
library(tm)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(ggthemes)
library(textdata)


#' # Analiza sentymentu

### Analiza sentymentu ----



tokenizacja <- function(df, aplikacja){
  
  text <- paste(df[df$app == aplikacja, 2], collapse = " ")
  
  docs <- VCorpus(VectorSource(text))
  tdm <- TermDocumentMatrix(docs)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m), decreasing=TRUE)
  
  
  tokeny <- data.frame(Review = names(v), freq = v, stringsAsFactors = F)
  tokeny_data <- as_tibble(tokeny)
  
  
  # Tokenizacja tekstu przy użyciu pakietu tidytext
  # library(tidytext)
  
  # Użycie unnest_tokens()
  tidy_tokeny <- tokeny_data %>%
    unnest_tokens(word, Review) # Tworzenie (word) z (Review) 
  
  # unnest_tokens() wykonuje czyszczenie
  # Usuwa interpunkcję i białe znaki, zamienia tekst na małe litery itp.
  
  return(tidy_tokeny)
  
}


analiza_sentymentu <- function(df, aplikacja, słownik){
  
  tidy_tokeny <- tokenizacja(df, aplikacja)
  
  
  if(słownik == "Loughran"){
    ## Analiza sentymentu przy użyciu słownika Loughran
    
    # Użycie inner_join()
    tidy_tokeny %>%
      inner_join(get_sentiments("loughran"), relationship = "many-to-many")
    # Liczba słów drastycznie się zmniejszyła,
    # ponieważ inner_join zachował tylko te słowa,
    # które występowały w słowniku
    
    
    # Zliczanie sentymentu
    sentiment_review <- tidy_tokeny %>%
      inner_join(get_sentiments("loughran"), relationship = "many-to-many")
    
    sentiment_review %>%
      count(sentiment)
    
    # Zliczanie, które słowa są najczęstsze
    # dla danego sentymentu
    sentiment_review %>%
      count(word, sentiment) %>%
      arrange(desc(n))
    
    
    # Filtrowanie analizy sentymentu
    # i pozostawienie tylko słów
    # o sentymencie pozytywnym lub negatywnym
    
    sentiment_review2 <- sentiment_review %>%
      filter(sentiment %in% c("positive", "negative"))
    
    
    word_counts <- sentiment_review2 %>%
      count(word, sentiment) %>%
      group_by(sentiment) %>%
      top_n(10, n) %>%
      ungroup() %>%
      mutate(
        word2 = fct_reorder(word, n)
      )
    
    # Wizualizacja sentymentu
    plot <- ggplot(word_counts, aes(x=word2, y=n, fill=sentiment)) + 
      geom_col(show.legend=FALSE) +
      facet_wrap(~sentiment, scales="free") +
      coord_flip() +
      labs(x = "Słowa", y = "Liczba") +
      theme_gdocs() + 
      ggtitle("Liczba słów wg sentymentu (Loughran)") +
      scale_fill_manual(values = c("firebrick", "darkolivegreen4"))
    return(plot)
  }
  
  if(słownik == "NRC"){
    ## Analiza sentymentu przy użyciu słownika NRC
    
    
    # Zliczanie sentymentu
    sentiment_review_nrc <- tidy_tokeny %>%
      inner_join(get_sentiments("nrc"), relationship = "many-to-many")
    
    sentiment_review_nrc %>%
      count(sentiment)
    
    # Zliczanie, które słowa są najczęstsze
    # dla danego sentymentu
    sentiment_review_nrc %>%
      count(word, sentiment) %>%
      arrange(desc(n))
    
    
    # Filtrowanie analizy sentymentu
    # i pozostawienie tylko słów
    # o sentymencie pozytywnym lub negatywnym
    
    sentiment_review_nrc2 <- sentiment_review_nrc %>%
      filter(sentiment %in% c("positive", "negative"))
    
    
    word_counts_nrc2 <- sentiment_review_nrc2 %>%
      count(word, sentiment) %>%
      group_by(sentiment) %>%
      top_n(10, n) %>%
      ungroup() %>%
      mutate(
        word2 = fct_reorder(word, n)
      )
    
    # Wizualizacja sentymentu
    plot <- ggplot(word_counts_nrc2, aes(x=word2, y=n, fill=sentiment)) + 
      geom_col(show.legend=FALSE) +
      facet_wrap(~sentiment, scales="free") +
      coord_flip() +
      labs(x = "Słowa", y = "Liczba") +
      theme_gdocs() + 
      ggtitle("Liczba słów wg sentymentu (NRC)")
    
    return(plot)
  }
  
  if(słownik == "Bing"){
    ## Analiza sentymentu przy użyciu słownika Bing
    
    
    # Zliczanie sentymentu
    sentiment_review_bing <- tidy_tokeny %>%
      inner_join(get_sentiments("bing"))
    
    sentiment_review_bing %>%
      count(sentiment)
    
    # Zliczanie, które słowa są najczęstsze
    # dla danego sentymentu
    sentiment_review_bing %>%
      count(word, sentiment) %>%
      arrange(desc(n))
    
    
    # Filtrowanie analizy sentymentu
    # i pozostawienie tylko słów
    # o sentymencie pozytywnym lub negatywnym
    
    sentiment_review_bing2 <- sentiment_review_bing %>%
      filter(sentiment %in% c("positive", "negative"))
    
    
    word_counts_bing2 <- sentiment_review_bing2 %>%
      count(word, sentiment) %>%
      group_by(sentiment) %>%
      top_n(10, n) %>%
      ungroup() %>%
      mutate(
        word2 = fct_reorder(word, n)
      )
    
    # Wizualizacja sentymentu
    plot <- ggplot(word_counts_bing2, aes(x=word2, y=n, fill=sentiment)) + 
      geom_col(show.legend=FALSE) +
      facet_wrap(~sentiment, scales="free") +
      coord_flip() +
      labs(x = "Słowa", y = "Liczba") +
      theme_gdocs() + 
      ggtitle("Liczba słów wg sentymentu (Bing)") +
      scale_fill_manual(values = c("dodgerblue4", "goldenrod1"))
    
    return(plot)
  }
  
  
  if(słownik == "Afinn"){
    ## Analiza sentymentu przy użyciu słownika Afinn
    
    
    # Zliczanie sentymentu
    sentiment_review_afinn <- tidy_tokeny %>%
      inner_join(get_sentiments("afinn"))
    
    sentiment_review_afinn %>%
      count(value)
    
    # Zliczanie, które słowa są najczęstsze
    # dla danego sentymentu
    sentiment_review_afinn %>%
      count(word, value) %>%
      arrange(desc(n))
    
    
    # Silnie pozytywne lub silnie negatywne słowa:
    # filtrowanie analizy sentymentu
    # i pozostawienie tylko słów o wartości w zakresie od -5 do 5
    
    sentiment_review_afinn3 <- sentiment_review_afinn %>%
      filter(value %in% c("3", "-3" , "4", "-4", "5", "-5"))
    
    
    word_counts_afinn3 <- sentiment_review_afinn3 %>%
      count(word, value) %>%
      group_by(value) %>%
      top_n(10, n) %>%
      ungroup() %>%
      mutate(
        word2 = fct_reorder(word, n)
      )
    
    # Wizualizacja sentymentu
    plot <- ggplot(word_counts_afinn3, aes(x=word2, y=n, fill=value)) + 
      geom_col(show.legend=FALSE) +
      facet_wrap(~value, scales="free") +
      coord_flip() +
      labs(x = "Słowa", y = "Liczba") +
      theme_gdocs() + 
      ggtitle("Liczba słów wg sentymentu (AFINN)")
    
    return(plot)
  }
  
}


#' ## Analiza sentymentu dla Tindera
analiza_sentymentu(df, "Tinder", "Loughran")
analiza_sentymentu(df, "Tinder", "NRC")
analiza_sentymentu(df, "Tinder", "Bing")
analiza_sentymentu(df, "Tinder", "Afinn")
      

#' ## Analiza sentymentu dla Hinge
analiza_sentymentu(df, "Hinge", "Loughran")
analiza_sentymentu(df, "Hinge", "NRC")
analiza_sentymentu(df, "Hinge", "Bing")
analiza_sentymentu(df, "Hinge", "Afinn")


#' ## Analiza sentymentu dla Bumble
analiza_sentymentu(df, "Bumble", "Loughran")
analiza_sentymentu(df, "Bumble", "NRC")
analiza_sentymentu(df, "Bumble", "Bing")
analiza_sentymentu(df, "Bumble", "Afinn")


#' ## Analiza sentymentu dla Boo
analiza_sentymentu(df, "Boo", "Loughran")
analiza_sentymentu(df, "Boo", "NRC")
analiza_sentymentu(df, "Boo", "Bing")
analiza_sentymentu(df, "Boo", "Afinn")



#' # Analiza występowania słów za pomocą TF-IDF

### Analiza występowania słów za pomocą TF-IDF ----


oczyszczanie <- function(df, aplikacja){
  df <- head(df[df$app == aplikacja,], 1000)
  
  
  corpus <- VCorpus(VectorSource(df$content))
  # Normalizacja i usunięcie zbędnych znaków 
  
  # Zapewnienie kodowania w całym korpusie
  corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))
  
  
  # Funkcja do zamiany znaków na spację
  toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))
  
  
  # Usuń zbędne znaki lub pozostałości url, html itp.
  
  # symbol @
  corpus <- tm_map(corpus, toSpace, "@")
  
  # symbol @ ze słowem (zazw. nazwa użytkownika)
  corpus <- tm_map(corpus, toSpace, "@\\w+")
  
  # linia pionowa
  corpus <- tm_map(corpus, toSpace, "\\|")
  
  # tabulatory
  corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")
  
  # CAŁY adres URL:
  corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")
  
  # http i https
  corpus <- tm_map(corpus, toSpace, "http\\w*")
  
  # tylko ukośnik odwrotny (np. po http)
  corpus <- tm_map(corpus, toSpace, "/")
  
  # pozostałość po re-tweecie
  corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")
  
  # inne pozostałości
  corpus <- tm_map(corpus, toSpace, "www")
  corpus <- tm_map(corpus, toSpace, "~")
  corpus <- tm_map(corpus, toSpace, "â€“")
  
  
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  
  # usunięcie zbędnych nazw własnych
  corpus <- tm_map(corpus, removeWords, c("app", "dating"))
  
  corpus <- tm_map(corpus, stripWhitespace)
  return (corpus)
}

analiza_tmidf <- function(corpus, aplikacja){
  
  corpus <- oczyszczanie(corpus, aplikacja)
  #' # Tokenizacja
  # Tokenizacja
  
  
  
  #' #Macierz częstości TDM

  tdm <- TermDocumentMatrix(corpus)
  tdm_m <- as.matrix(tdm)
  
  
  
  #' #Zliczanie częstości słów
  # (Word Frequency Count)
  
  
  # Zlicz same częstości słów w macierzach
  v <- sort(rowSums(tdm_m), decreasing = TRUE)
  tdm_df <- data.frame(word = names(v), freq = v)
  
  
  #Eksploracyjna analiza danych

  # (Exploratory Data Analysis, EDA)
  
  
  # Chmura słów (globalna)
  wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 30, 
            colors = brewer.pal(8, "Dark2"))
  
  

  
  #Macierz częstości TDM z TF-IDF

  
  tdm_tfidf <- TermDocumentMatrix(corpus,
                                  control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
  
  
  
  tdm_tfidf_m <- as.matrix(tdm_tfidf)
  
  
  #Zliczanie częstości słów
  # (Word Frequency Count)
  
  
  # Zlicz same częstości słów w macierzach
  v_tfidf <- sort(rowSums(tdm_tfidf_m), decreasing = TRUE)
  tdm_tfidf_df <- data.frame(word = names(v_tfidf), freq = v_tfidf)
  
  
  
  # Eksploracyjna analiza danych
  # (Exploratory Data Analysis, EDA)
  
  
  # Chmura słów (globalna)
  wordcloud(words = tdm_tfidf_df$word, freq = tdm_tfidf_df$freq, min.freq = 100, 
            colors = brewer.pal(8, "Dark2"))
  
  
  
}

#' ## Chmury słów, przed i po przeprowadzeniu analizy TF-IDF 
#' ### Tinder
analiza_tmidf(df, "Tinder")

#' ### Hinge
analiza_tmidf(df, "Hinge")

#' ### Bumble
analiza_tmidf(df, "Bumble")

#' ### Boo
analiza_tmidf(df, "Boo")


