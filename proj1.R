## Group 25
## Frans : s2591760
## Nathan : s2524152
## Daiki : s2547603

## Contribution to this project
## Frans : 
## Nathan : 
## Daiki : 

## 3: 
setwd("C:/Users/maruw/ASEAN-Group-25") # setting the working directory
a <- scan("4300-0.txt",what="character",skip=73,nlines=32858-73) # scanning the text file
a <- gsub("_(","",a,fixed=TRUE) # 

## 4: the function to separate each punctuation mark
split_punct <- function(text, punct) {
  ie <- grep(punct, text, fixed=TRUE)  ## detecting the words in text containing punct
  vect <- rep("",times = length(ie)+length(text))  ## empty character output vector 
  pos_punct <- ie+1:length(ie)  ## detecting the position of punct in the new vector
  vect[pos_punct] <- substr(text[ie],nchar(text[ie]),nchar(text[ie]))  ## putting punct in the new vector
  text <- gsub(punct,"",text,fixed = TRUE)  ## removing punct from words in text
  vect[-pos_punct] <- text ## putting the remaining words in the new vector
  return(vect)
}

## 5: separate the punctuation marks
a <- split_punct(a,",") 
a <- split_punct(a,".")
a <- split_punct(a,";")
a <- split_punct(a,"!")
a <- split_punct(a,":")
a <- split_punct(a,"?")

## 6: create the vector "b"
a_low <- tolower(a)
a_uni <- unique(a_low)
a_match <- match(a_low, a_uni)
a_tab <-tabulate(a_match)
a_order <- sort(a_tab, decreasing=TRUE)
threshold <- a_order[1000]
b <- a_uni[a_tab >= threshold]

## 7: create the matrices "T" and "P"
#create vector of indices matching the full text to the most common words 
ia <- match(a_low, b)
#create three-column matrix of indices where 
#each row is a triplet of adjacent words
pt <- cbind(ia[1:(length(ia)-2)],ia[2:(length(ia)-1)],ia[3:length(ia)])
#remove rows that contain NA
T <- pt[!is.na(rowSums(pt,na.rm=FALSE)),]

#create a two-column matrix of indices where 
#each row is a pair of adjacent-words 
pt2 <- cbind(ia[1:(length(ia)-1)],ia[2:length(ia)])
#remove rows that contain NA
P <- pt2[!is.na(rowSums(pt2,na.rm=FALSE)),]

# Simulate first word
dict <- match(a_low,b)
dict <- na.omit(dict)
sim <- NULL
sim <- sample(dict,size=1)

# Simulate second word
#extract the sub-matrix from T who rows all have k[i] and k[j] in column 1 and 2
k_i <- P[which(P[,1] == sim),]
sim <- cbind(sim, sample(k_i[,2], size = 1))


## 8: simulate 50-words sections
generate <- function(list) {
  col_T <- T[which((T[,1] == list[1]) & (T[,2] == list[2])),]
  if (length(col_T) != 0){
    next_index <- sample(col_T[,3], size = 1)
  }
  else {
    input <- list[2]
    col_P <- P[which(P[,1] == input),]
    if (length(col_P != 0)){
      next_index <- sample(col_P[,2], size = 1)
    }
    else{
      next_word <- sample(b,size=1)
      next_index <- match(next_word,b) ##### change?????
    }
  }
  return(next_index)
}

for (p in 1:48){
  next_word <- generate(sim[p:p+1])
  sim <- cbind(sim, next_word)
}

sim <- as.numeric(sim)
sim_text <- b[sim]

## 9:

alt <- sample(dict, size = 50, replace = TRUE)

alt_text <- b[alt]

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}

<<<<<<< HEAD
## 10:
capital <- firstup(a) # all a with first letter capitalized
ii <- na.omit(match(capital,a)) #indices of capital letters in a
capz <- a[ii] # all first letter capital word in a
capz_uni <- unique(capz) # unique capital words
capz_unib <- firstup(b[na.omit(match(tolower(capz_uni),b))]) # unique capital letters in b
low_capz <- tolower(capz_unib)

count_capital <- NULL
for (k in 1:length(capz_unib)){
  entry <- length(which(a == capz_unib[k]))
  count_capital <- cbind(count_capital,entry)
} # count_capital is number of capital word appearance in a with the same order as capz_unib

count_low <- NULL
for (k in 1:length(low_capz)){
  entry <- length(which(a == low_capz[k]))
  count_low <- cbind(count_low,entry)
} # count_low is number of capital word appear as lower case

probability <- count_capital/(count_capital + count_low)
freq_cap <- capz_unib[which(probability > 0.5)] # words that appear as capital >50%
low_freq_cap <- tolower(freq_cap)
b_update <- b
b_update[match(low_freq_cap,b_update)] <- freq_cap

dict2 <- match(a_low,b_update)
dict2 <- na.omit(dict2)
sim2 <- NULL
sim2 <- sample(dict,size=1)
k_i2 <- P[which(P[,1] == sim2),]
sim2 <- cbind(sim2, sample(k_i2[,2], size = 1))
generate2 <- function(list) {
  col_T <- T[which((T[,1] == list[1]) & (T[,2] == list[2])),]
  if (length(col_T) != 0){
    next_index <- sample(col_T[,3], size = 1)
  }
  else {
    input <- list[2]
    col_P <- P[which(P[,1] == input),]
    if (length(col_P != 0)){
      next_index <- sample(col_P[,2], size = 1)
    }
    else{
      next_word <- sample(b_update,size=1)
      next_index <- match(next_word,dict2)
    }
  }
  return(next_index)
}

for (p in 1:48){
  next_word <- generate(sim[p:p+1])
  sim2 <- cbind(sim2, next_word)
}

sim2 <- as.numeric(sim2)
sim_text2 <- b[sim2]


