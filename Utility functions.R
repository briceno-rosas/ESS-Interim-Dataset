
mysummary <- function(x){
   a<- c(min = min(x),
         max = max(x),
         mean = mean(x),
         sd = sd(x),
         Q1 = as.numeric(quantile(x, .25)),
         Q2 = median(x),
         Q3 = as.numeric(quantile(x, .75)),
         n = length(x))
   a[1:7] <- round(a[1:7], 4)
   a
}

catcountries <- function(x, dev = "and"){
  l <- length(x)
  
  
  if(l == 1){
    x
  } else if(l == 2){
    paste(x, collapse = paste("", dev, ""))
  } else{
    paste(paste0(x[1:(l-1)], collapse = ", "), dev, x[l])
  }
}

catsources <- function(x, dev = "and"){
  l <- length(x)
  
  
  if(l == 1){
    x
  } else if(l == 2){
    x[2] <- gsub("Based on ", "", x[2])
    x <- paste(x, collapse = paste("", dev, ""))
  } else{
    x[1:(l-1)] <- lapply(x[1:(l-1)], FUN = function(x) gsub("[.]$", "", x))
    x[2:l] <- gsub("Based on ", "", x[2:l])
    x <- paste(paste0(x[1:(l-1)], collapse = ", "), dev, x[l])
  }
  x <- gsub(paste("[.]", dev), paste("", dev), x)
  x
}

catitems <- function(x){
  l <- nrow(x)
  x$itemlabel <- with(x, paste(toupper(x$item), paste0("'", x$label, "'")))
  if(l == 1){
    x$itemlabel
  } else if(l == 2){
    paste(x$itemlabel, collapse = " and ")
  } else{
    paste(paste0(x$itemlabel[1:(l-1)], collapse = ", "), "and", x$itemlabel[l])
  }
}

catnumeric <- function(x, unit = "round"){
  l <- length(x)
  if(l == 1){
    paste(firstup(unit), x)
  } else if(l == 2){
    paste(paste0(firstup(unit), "s"), paste(x, collapse = " and "))
  } else{
    paste(paste0(firstup(unit), "s"), paste(paste0(x[1:(l-1)], collapse = ", "), "and", x[l]))
  }
}

previousrounds <- function(roundsset, rounds, qualifier = "only"){
  prev <- ifelse(length(roundsset) == 0, "in none of the previous rounds",
                 ifelse(setequal(rounds, roundsset), "in all previous rounds",
                        ifelse(setequal(head(rounds[order(-rounds)], length(roundsset)), roundsset), paste("since Round", min(roundsset)),
                        ifelse(length(roundsset) <= 3, ifelse(!is.null(qualifier), paste(qualifier, "in", catnumeric(roundsset)),
                                                                       paste("in", catnumeric(roundsset))),
                               ifelse(length(roundsset)/length(rounds) <= .5,
                                             paste("in", catnumeric(roundsset)),
                                             paste("in all previous rounds, except", catnumeric(setdiff(rounds, roundsset))))))))
  prev
}

weekinmonth <- function(date){
  timing <- ifelse(lubridate::day(date) <= 7, "the beginning of", 
                   ifelse(lubridate::day(date) <= 14, "the second week of", 
                          ifelse(lubridate::day(date) <= 21, "the third week of", "the end of")))
  timing
}

datedescr <- function(date){
  paste(weekinmonth(date), lubridate::month(date, label = T, abbr = F), lubridate::year(date))
}


firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

firstdown <- function(x) {
  substr(x, 1, 1) <- tolower(substr(x, 1, 1))
  x
}

fullstop <- function(x) {
  x <- paste0(x, ".")
  x
}

comma <- function(x) {
  x <- paste0(x, ",")
  x
}

brackets <- function(x) {
  x <- paste0("(", x, ")")
  x
}

durationapprox <- function(x, qualifier = "only"){
  a <- ifelse(x == 1, ifelse(!is.null(qualifier), paste(qualifier, "1 day"), "1 day"),
              ifelse(x < 7, ifelse(!is.null(qualifier), paste(qualifier, x, "days"), paste(x, "days")),
                     ifelse(round(x/7) == 1, "1 week",
                            ifelse(x < 25, paste("about", round(x/7), "weeks"),
                                   ifelse(x/30 <= 1.33, "about 1 month",
                                          ifelse(x/30 < 1.66, "about 1 and a half month",
                                                 ifelse((x/30) %% 1 < .33 | (x/30) %% 1 > .66, paste("about", round(x/30), "months"),
                                                        paste("about", floor(x/30), "and a half months"))))))))
  a
}

sumrow <- function(dat){
  func <- function(z) if (is.numeric(z)) sum(z) else ''
  sumrow <- as.data.frame(lapply(dat, func))
  sumrow
}

propapprox <- function(x, onlycutoff = NA){
  a <- ifelse(x < .01, "less than 1%", 
              ifelse(is.na(onlycutoff) | (!is.na(onlycutoff) & x > onlycutoff), as.character(formattable::percent(x, digits = 0)),
                     paste("only", as.character(formattable::percent(x, digits = 0)))))
  a
}

is_outlier <- function(x) {
  return(x < quantile(x, 0.25, na.rm = TRUE) - 1.5 * IQR(x, na.rm = TRUE) | x > quantile(x, 0.75, na.rm = TRUE) + 1.5 * IQR(x, na.rm = TRUE))
}

percentmatchDF_intwer <- function(df, id, intwer){ # returns a dataframe with the percentage of shared answers for every observation combination in a three variable format
  # inputs: a dataset, the IDs of the interviews (id) and the interviewers (intwer) either in df$id format or previously extracted
  # it is useful to subset the data before to include only substantial answers and extract the IDs so they do not count in the share of common answers
  
  df <- t(df)
  cols <- ncol(df)
  rows <- nrow(df)
  id1 <- as.integer(id)
  
  pmatch <- matrix(nrow = cols, ncol = cols)
  
  for (c in 1:cols){
    comp <- df == df[,c]
    pmatch[c,] <- colSums(comp, na.rm=T)/rows
  }
  
  pmatch[upper.tri(pmatch)] <- NA
  diag(pmatch) <- NA
  
  
  id2 <- as.integer(rep(id[1], cols))
  match <- pmatch[,1]
  pmatchdf <- as.data.frame(cbind(id1, id2, match))
  
  for(c in 2:cols){
    id2 <- as.integer(rep(id[c], cols))
    match <- pmatch[,c]
    pmatchdf_temp <- as.data.frame(cbind(id1, id2, match))
    
    pmatchdf <- rbind(pmatchdf, pmatchdf_temp)
  }
  
  pmatchdf$intwer1 <- rep(intwer, each = cols)
  pmatchdf$intwer2 <- rep(intwer, cols)
  
  pmatchdf <- subset(pmatchdf, is.na(pmatchdf$match) == FALSE)
  
  return(pmatchdf)
}

#FUNCTION 4: SINGLE COUNTRY DATASET FUCTION

CatPCA_scntry <- function(ess,varnames,keptvars,mnint){
  
  #convert vars so can be used in dplyr  
  dpvarn<-enquo(varnames)
  dpkept<-enquo(keptvars)
  
  
  #datapreparation, get all counts needed for later subsetting
  esub<-Main %>% select(!!dpkept, !!dpvarn)
  esub<-esub %>% group_by(intnum) %>% dplyr::mutate(n_int= n())
  
  esub$nmis<-rowSums(is.na(esub[varnames]))
  
  esub[varnames]<-sapply(esub[varnames], function(x) as.numeric(as.character(x)))
  
  esub$sd<-rowSds(as.matrix(esub[varnames]), na.rm = TRUE)
  
  
  #calculate scores
  esub$scores<-NA
  
  esub<-as.data.frame(esub)
  
  esub$scores[complete.cases(esub[varnames])]<-as.vector(princals(esub[complete.cases(esub[,varnames]),varnames], ndim=1)$objectscores*-1)
  
  
  #calculate table for pcat
  pcatable<-esub%>%filter(n_int>mnint & !is.na(scores))%>%group_by(intnum) %>% summarise(m_intscore=mean(scores), sd_intscore= sd(scores),min_intscore=min(scores), max_intscore=max(scores))
  
  
  n_int<-esub %>% group_by(intnum) %>% dplyr::summarise(n_int=n())
  completetable<-full_join(pcatable, n_int, by= "intnum")
  
  return(as.data.frame(completetable))
}

