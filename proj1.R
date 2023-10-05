## Group 25
## Frans : s2591760
## Nathan : s2524152
## Daiki : s2547603

## Contribution to this project
## Frans : 
## Nathan : 
## Daiki : 

###########################################################################

# 3: Read the text file
setwd("C:/Users/maruw/ASEAN-Group-25") ## set the working directory
a <- scan("4300-0.txt", what="character", skip=73, nlines=32858-73) ## read the text file
a <- gsub("_(", "", a, fixed=TRUE) ## remove "_("

###########################################################################

# 4: Create the function to separate each punctuation mark
split_punct <- function(text, punct) {
  ie <- grep(punct, text, fixed=TRUE)  ## detect the words containing the punctuation mark
  vect <- rep("",times = length(ie) + length(text))  ## vector to store the punctuation marks 
  pos_punct <- ie + 1:length(ie)  ## where should the punctuation mark go in the new vector
  vect[pos_punct] <- substr(text[ie], nchar(text[ie]), nchar(text[ie]))  ## insert the punctuation marks in the new vector
  text <- gsub(punct, "", text, fixed = TRUE)  ## remove the punctuation mark from the text
  vect[-pos_punct] <- text ## insert the remaining words in the new vector
  return(vect) ## return a value
}

###########################################################################

# 5: Separate the punctuation marks
a <- split_punct(a, ",") ## separate ","
a <- split_punct(a, ".") ## separate "."
a <- split_punct(a, ";") ## separate ";"
a <- split_punct(a, "!") ## separate "!"
a <- split_punct(a, ":") ## separate ":"
a <- split_punct(a, "?") ## separate "?"

###########################################################################

# 6: Create the vector of top 1008 unique words called "b"
a_low <- tolower(a) ## replace capital letters with lower case letters
a_uni <- unique(a_low) ## vector of unique words
a_match <- match(a_low, a_uni) ## detect which element in a_uni is element in a_low
a_tab <-tabulate(a_match) ## count up the number of occurances of each unique word
a_order <- sort(a_tab, decreasing=TRUE) ## sort a_tab in descending order
threshold <- a_order[1000] ## gain the threshold number of occurances 
b <- a_uni[a_tab >= threshold] ## create the vector b

###########################################################################

# 7: Create the matrices "T" and "P"
ia <- match(a_low, b) ## vector of indices matching the full text to b
pt <- cbind(ia[1:(length(ia)-2)],ia[2:(length(ia)-1)],ia[3:length(ia)]) 
## create three-column matrix of indices where each row is a triplet of adjacent words
T <- pt[!is.na(rowSums(pt,na.rm=FALSE)),] ## remove rows containing NA

pt2 <- cbind(ia[1:(length(ia)-1)],ia[2:length(ia)])
## create a two-column matrix of indices where each row is a pair of adjacent words 
P <- pt2[!is.na(rowSums(pt2,na.rm=FALSE)),] ## remove rows that contain NA

###########################################################################

# 8: Simulate 50-word sections
#### simulate first word
dict <- match(a_low, b) ######### same as ia??
dict <- na.omit(dict) ## remove NA
sim <- NULL ## empty vector
sim <- sample(dict,size=1) ## randomly choose first word from dict

#### simulate second word
k_i <- P[which(P[,1] == sim),] 
## extract all rows containing sim in the first column
sim <- cbind(sim, sample(k_i[,2], size = 1)) 
## randomly choose the number from k_i and connect it to sim

#### function to generate the next word
generate <- function(list) {
  col_T <- T[which((T[,1] == list[1]) & (T[,2] == list[2])),] ## 
  if (length(col_T) != 0){
    next_index <- sample(col_T[,3], size = 1)
  } ##
  else {
    input <- list[2] ## 
    col_P <- P[which(P[,1] == input),] ## 
    if (length(col_P != 0)){
      next_index <- sample(col_P[,2], size = 1)
    } ## 
    else{
      next_word <- sample(b,size=1)
      next_index <- match(next_word,b) ##### change?????
    } ## 
  } ## 
  return(next_index) ## return a value
}

#### generate 50 words
for (p in 1:48){
  next_word <- generate(sim[p:p+1]) ## generate the next word
  sim <- cbind(sim, next_word) ## connect the word to sim
}
sim <- as.numeric(sim) ## 
sim_text <- b[sim] ## change indices to words

###########################################################################

# 9: Simulate 50-word sections where the word probabilities are based on the common word frequencies
alt <- sample(dict, size = 50, replace = TRUE) ## 
alt_text <- b[alt] ## 
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1)) ## 
  return(x) ## return a value
}

###########################################################################

# 10: Modified version to consider the words that most often start with a capital letter
capital <- firstup(a) ## all a with first letter capitalized
ii <- na.omit(match(capital,a)) ## indices of capital letters in a
capz <- a[ii] ## all first letter capital word in a
capz_uni <- unique(capz) ## unique capital words
capz_unib <- firstup(b[na.omit(match(tolower(capz_uni),b))]) ## unique capital letters in b
low_capz <- tolower(capz_unib) ## replace capital letters with lower case letters

#### count_capital is the number of capital word appearance in a with the same order as capz_unib
count_capital <- NULL ## empty vector
for (k in 1:length(capz_unib)){
  entry <- length(which(a == capz_unib[k]))
  count_capital <- cbind(count_capital,entry)
} 

#### count_low is the number of capital word appear as lower case
count_low <- NULL ## empth vector
for (k in 1:length(low_capz)){
  entry <- length(which(a == low_capz[k])) ##
  count_low <- cbind(count_low,entry) ## 
} 

probability <- count_capital/(count_capital + count_low) ## calculate the probability of 
freq_cap <- capz_unib[which(probability > 0.5)] ## words that appear as capital >50%
low_freq_cap <- tolower(freq_cap) ## 
b_update <- b ## 
b_update[match(low_freq_cap,b_update)] <- freq_cap ## 

dict2 <- match(a_low,b_update) ## 
dict2 <- na.omit(dict2) ## 
sim2 <- NULL ## empty vector
sim2 <- sample(dict,size=1) ## 
k_i2 <- P[which(P[,1] == sim2),] ## 
sim2 <- cbind(sim2, sample(k_i2[,2], size = 1)) ## 
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
} ## 

for (p in 1:48){
  next_word <- generate(sim[p:p+1])
  sim2 <- cbind(sim2, next_word)
} ##
sim2 <- as.numeric(sim2) ## 
sim_text2 <- b[sim2] ## 


