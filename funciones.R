
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

#df <- proyecto_canciones
procesamiento <- function(df, stemming = FALSE, 
                          exclude_stopwords = TRUE, nmin = 1){
  n <- df$id %>%
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
    filter(!is.na(word))
  
  df <- cbind(df, num.token0, num.token)

  return(list(df, clean.letra.df))  
}

