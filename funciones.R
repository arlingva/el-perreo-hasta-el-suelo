
###-----------------------------------------------
library(textclean)
clean_text <- function(text){
  text <- tolower(text) 
  text <- str_replace_all(text,"\\$\\w*", "")  
  text <- str_replace_all(text,"^RT[\\S]+", "") 
  text <- str_replace_all(text,"http[\\S]*", "")
  text <- str_replace_all(text,"@[\\S]*", "") 
  text <- str_replace_all(text,"#", "") 
  #text <- str_replace_all(text,"[¿?¡!,()‑-.]", "") 
  text <- chartr("áéíóú", "aeiou", text) 
  text <- str_replace_all(text,"[[:punct:]]", " ") 
  text <- str_replace_all(text,"[[:digit:]]", " ") 
  text <- str_replace_all(text,"[\\n]+", " ") 
  text <- str_replace_all(text,"[\\r]+", " ") 
  text <- str_replace_all(text,"[\\s]+", " ") 
  text <- str_replace_all(text,"^[\\s]+", "")  
  text <- str_replace_all(text,"[\\s]+^", "")  
  return(text)
}



###-----------------------------------------------
library(corpus)
tokenize_text <- function(text, stemming = FALSE, 
                           exclude_stopwords = TRUE,
                           nmin = 1
                           ){
  #text <- clean_text(text)
  text <- str_split(text, " ")[[1]]
  if(stemming == TRUE){text <- text_tokens(text, stemmer = "es")}
  if(exclude_stopwords == TRUE){text <- text[text %notin% stopwords]}
  text <- text[text != ""] 
  text <- text[str_length(text) > nmin] 
  text <- unlist(text)
  return(text)
}


###-----------------------------------------------
#df <- df.regueton
procesamiento <- function(df, genero, stemming = FALSE, 
                          exclude_stopwords = TRUE, nmin = 1){
  n <- df$letra %>%
    length()

  # Limpieza del texto
  clean.letra <- df$letra %>%
    clean_text()

  ## Número de palabras únicas previo a eliminar stopwords.
  num.token0 <- c()
  for(i in 1:n){
    str_split(clean.letra, " ")[[i]] %>%
      unique() %>%
      length() -> num.token0[i]
  }

  ## Limpieza de datos
  ### Limpieza de datos sin stemming, excluyendo stopwords.
  
  clean.letra.v1 <- list()
  
  #for(i in 1:n){clean.letra.v1[[i]] <- tokenize_text(clean.letra[1])}
  clean.letra.v1 <- lapply(clean.letra, tokenize_text,
                           stemming, exclude_stopwords, nmin)

  ## Número de palabras únicas después de eliminar stopwords.
  
  num.token <- c()
  for(i in 1:n){
    clean.letra.v1[[i]] %>% 
      unique() %>% 
      length() -> num.token[i]
  }

  ## Lista de palabras
  clean.letra.df <- unlist(clean.letra.v1) %>%
    as.data.frame()
  
  colnames(clean.letra.df) <- "word"
  
  clean.letra.df <- clean.letra.df %>%
    group_by(word) %>%
    summarise(freq = n()) %>%
    arrange(desc(freq)) %>%
    filter(!is.na(word)) %>%
    mutate(genero = genero)
  
  df <- cbind(df, num.token0, num.token)

  return(list(df, clean.letra.df))  
}

###-----------------------------------------------
procesamiento2 <- function(df, genero, stemming = FALSE, 
                          exclude_stopwords = TRUE, nmin = 1){
  n <- df$letra %>%
    length()
  
  # Limpieza del texto
  clean.letra <- df$letra %>%
    clean_text()
  
  ## Número de palabras únicas previo a eliminar stopwords.
  num.token0 <- c()
  for(i in 1:n){
    str_split(clean.letra, " ")[[i]] %>%
      unique() %>%
      length() -> num.token0[i]
  }
  
  ## Limpieza de datos
  ### Limpieza de datos sin stemming, excluyendo stopwords.
  
  clean.letra.v1 <- list()
  
  #for(i in 1:n){clean.letra.v1[[i]] <- tokenize_text(clean.letra[1])}
  clean.letra.v1 <- lapply(clean.letra, tokenize_text,
                           stemming, exclude_stopwords, nmin)
  
  ## Número de palabras únicas después de eliminar stopwords.
  
  num.token <- c()
  for(i in 1:n){
    clean.letra.v1[[i]] %>% 
      unique() %>% 
      length() -> num.token[i]
  }
  
  ## Lista de palabras
  clean.letra.df <- unlist(clean.letra.v1) %>%
    as.data.frame()
  
  colnames(clean.letra.df) <- "word"
  
  clean.letra.df <- clean.letra.df %>%
    group_by(word) %>%
    summarise(freq = n()) %>%
    arrange(desc(freq)) %>%
    filter(!is.na(word)) %>%
    mutate(genero = genero)
  
  df <- cbind(df, num.token0, num.token)
  
  return(list(df, clean.letra.df))  
}



###-----------------------------------------------

top.token <- function(df, genero, n){
  df %>%
    filter(genero == genero) %>%
    select(cancion, num.token) %>%
    arrange(desc(num.token)) %>%
    head(n)
}

tail.token <- function(df, genero, n){
  df %>%
    select(cancion, num.token) %>%
    arrange(num.token) %>%
    head(n)
}

###-----------------------------------------------
library(plotly)
barplotly <- function(df, genero, n){
  #df <- df(df, genero, n) %>% tail.token()
  x <- df$cancion
  y <- df$num.token
  text <- df$cancion
  data <- data.frame(x=reorder(df$cancion, -df$num.token), y, text)
  
  #ggplot(data,aes(x= reorder(cat,-num),num))+geom_bar(stat ="identity")
  
  fig <- plot_ly(data, x = ~x, y = ~y, type = 'bar', text = y,
                 marker = list(color = 'rgb(158,202,225)',
                               line = list(color = 'rgb(8,48,107)',
                                           width = 1.5)))
 
  
  fig
}

###-----------------------------------------------
library(forcats)

ggbarplot <- function(df){
  df %>%
    mutate(cancion = fct_reorder(cancion, num.token)) %>%
    ggplot(aes(x=cancion, y=num.token)) +
    geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
    coord_flip() +
    xlab("") +
    theme_bw()
}

# Reorder following the value of another column:

