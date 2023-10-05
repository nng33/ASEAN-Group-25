## Group 25
## Frans : s2591760
## Nathan : s2524152
## Daiki : s2547603

## Contribution to this project
## All group members Collaborated together to create the simulation of the 50-word sections from the model
## All group members reviewed the entire code and wrote the comments together 
## Frans (35%): Modified the code that replaces words that most often start with a capital letter in the main text
## Daiki (32%): Handled the data pre-processing part of the task
## Nathan (33%): Responsible for creating the matrices of common word triplets and pairs

###########################################################################


# 3: Read the text file

setwd("C:/Users/maruw/ASEAN-Group-25") ## set the working directory
a <- scan("4300-0.txt", what="character", skip=73, nlines=32858-73) ## read the text file
a <- gsub("_(", "", a, fixed=TRUE) ## remove "_("


###########################################################################


# 4: Create a function to separate each punctuation mark

split_punct <- function(text, punct) {
  ## obtain indices of the words containing the punctuation mark
  index_punct <- grep(punct, text, fixed=TRUE)  
  
  ## create an empty vector to store the punctuation marks 
  vect <- rep("",times = length(index_punct) + length(text))  
  
  ## obtain position of the punctuation mark in the new vector
  pos_punct <- index_punct + 1:length(index_punct)  
  
  ## insert punctuation marks in the new vector
  vect[pos_punct] <- substr(text[index_punct], nchar(text[index_punct]), nchar(text[index_punct]))  
  
  ## remove punctuation marks from the main text
  text <- gsub(punct, "", text, fixed = TRUE)  
  
  ## insert the remaining words in the new vector
  vect[-pos_punct] <- text 
  
  ## return the new vector
  return(vect) 
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


# 6: Create a vector of top (approx.) 1000 unique words called "b"

a_low <- tolower(a) ## replace capital letters with lower case letters

a_uni <- unique(a_low) ## vector of unique words

a_match <- match(a_low, a_uni) ## detect which element of a_uni is element of a_low

a_tab <-tabulate(a_match) ## count up the number of occurrences of each unique word

a_order <- sort(a_tab, decreasing=TRUE) ## sort a_tab in descending order

threshold <- a_order[1000] ## gain the number of occurrences threshold for the top 1000 words

b <- a_uni[a_tab >= threshold] ## store all words that has occurrences higher than the threshold into b


###########################################################################


# 7: Create the matrices of common word triplets "T" and pairs "P"

ia <- match(a_low, b) ## vector of indices matching the full text to b

## create three-column matrix of indices where each row is a triplet of adjacent words
## first column is ia, second column is ia shifted up by 1, and third column is ia shifted up by 2
## this implies that the last two rows of ia will be unusable
pre_T <- cbind(ia[1:(length(ia)-2)], ia[2:(length(ia)-1)], ia[3:length(ia)]) 

## remove rows of NA from pt to get the final matrix of triplets of adjacent words
T <- pre_T[!is.na(rowSums(pre_T, na.rm=FALSE)),] 

## create a two-column matrix of indices where each row is a pair of adjacent words 
## first column is ia, second column is ia shifted up by 1
## this implies that the last row of ia will be unusable
pre_P <- cbind(ia[1:(length(ia)-1)], ia[2:length(ia)])

## remove rows that contain NA to get the final matrix of pairs of adjacent words
P <- pre_P[!is.na(rowSums(pre_P, na.rm=FALSE)),]


###########################################################################


# 8: Simulate 50-word sections

#### simulate first word

## remove NA from ia 
ia_mod <- na.omit(ia) 

## randomly choose first word from the text that exists in b (i.e, sample ia_mod)
sim <- sample(ia_mod, size=1)


#### simulate second word

## generate2 will simulate words based on P
## If the first word exists in the first column of P,
## randomly choose a word from the second column of P corresponding to the rows of ia_mod

generate2 <- function(list,ia_mod,P){
  ## extract all rows containing sim in the first column
  ## if there are elements in col_P that exist, we sample the word based on common word pairs
  ## otherwise sample the word based on common word frequencies
  col_P <- P[which(P[,1] == list[1]),]
  if (length(col_P != 0)){
    next_index <- sample(col_P[,2], size = 1)
  }
  else{
    next_index <- sample(ia_mod, size = 1)
  }
  return(next_index)
}

next_index <- generate2(sim,ia_mod,P) ## use the above function to simulate the second index
sim <- cbind(sim, next_index) ## column bind the previous index to sim

## generate3 will simulate words based on P and T
## If the first word exists in the first column of T and second word exists in the second column of T,
## randomly choose a word from the third column of T corresponding to the rows of ia_mod

generate3 <- function(list,ia_mod,P,T) {
  ## extract all rows containing sim in the first column
  ## if there are elements in col_T that exist, we sample the word based on common word pairs
  ## otherwise sample the word based on common word frequencies
  col_T <- T[which((T[,1] == list[1]) & (T[,2] == list[2])),]  
  if (length(col_T) != 0){
    next_index <- sample(col_T[,3], size = 1)
  }
  else {
    next_index <- generate2(list,ia_mod,P)
  }
  return(next_index) 
}


#### generate 50 words

for (p in 1:48){
  next_index <- generate3(sim[p:p+1], ia_mod, P, T) ## generate the next word using the previous two indices
  sim <- cbind(sim, next_index) ## column bind the previous index to sim
}

sim_text <- b[sim] ## match the indices of the generated words (based on word pairs) to b and assign to new variable
cat(sim_text) ## print the corresponding sample of words


###########################################################################


# 9: Simulate 50-word sections where the word probabilities are based on the common word frequencies

sim2 <- sample(ia_mod, size = 50, replace = TRUE) ## sample 50 word indices from ia_mod with replacement
sim2_text <- b[sim2] ## match the indices of the generated words (based on common word frequencies) to b and assign to new variable
cat(sim2_text) ## print the corresponding sample of words


###########################################################################


# 10: Modified version to consider the words that most often start with a capital letter

capitalise <- function(x) {
  ## extract the first letter and capitalize it and replace it 
  substr(x, 1, 1) <- toupper(substr(x, 1, 1)) 
  return(x) 
}


capital <- capitalise(a) ## use the above function to capitalize all the first letters of each word in a
capz_index <- na.omit(match(capital,a)) ## indices of capital letters in a excluding the NAs
capz <- a[capz_index] ## match the indices of all capital letters in a
capz_uni <- unique(capz) ## obtain the unique capital words
capz_unib <- capitalise(b[na.omit(match(tolower(capz_uni),b))]) ## obtain the unique capital letters in b excluding the NAs
low_capz <- tolower(capz_unib) ## replace capital letters with lower case letters

## count_capital is the number of capital words in a with the same order as capz_unib
count_capital <- NULL ## create an empty vector
for (k in 1:length(capz_unib)){
  entry <- length(which(a == capz_unib[k])) ## obtain the frequency of each word that starts with a capital letter
  count_capital <- cbind(count_capital,entry) ## column bind the result to count_capital
} 

## count_low is the count of capz_unib words that begins with a lowercase letter in a
count_low <- NULL ## create an empty vector
for (k in 1:length(low_capz)){
  entry <- length(which(a == low_capz[k])) ## obtain the frequency of each word that starts with a lowercase letter
  count_low <- cbind(count_low,entry) ## column bind the result to count_low
} 

probability <- count_capital/(count_capital + count_low) ## calculate the occurrences of capitalized and non-capitalized words
freq_cap <- capz_unib[which(probability > 0.5)] ## obtain the index of words that are most often start with a capital letter (more than 50% of the time)
low_freq_cap <- tolower(freq_cap) ## convert the capitalized words to lowercase words 
b_update <- b ## clone b into a new variable
b_update[match(low_freq_cap,b_update)] <- freq_cap ## replace all words that most often start with a capital letter with its capitalized version

ia2 <- match(a,b_update) ## vector of indices matching the full text of b_update
ia2_mod <- na.omit(ia2) ## remove NA from ia2

sim3 <- sample(ia2_mod,size=1) ## randomly choose first word from the text that exists in b_update (i.e, sample ia2_mod)
next_index <- generate2(sim3,ia2_mod,P) ## simulate the second index 
sim3 <- cbind(sim3, next_index) ## column bind the previous index to sim3

for (p in 1:48){
  next_index <- generate3(sim3[p:p+1],ia2_mod,P,T) ## generate the next index using the previous two words
  sim3 <- cbind(sim3, next_index) ## column bind the previous index to sim3
} 

sim3_text <- b_update[sim3] ## match the indices of the generated words to b_update and assign to new variable
cat(sim3_text) ## print the corresponding sample of words

