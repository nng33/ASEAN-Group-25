## Group 25
## Frans : s2591760
## Daiki : s2547603
## Nathan : s2524152


## Contribution to this project
## All group members Collaborated together to create the simulation of the 50-word sections from the model
## All group members reviewed the entire code and wrote the comments together 
## Frans (35%): Modified the code that replaces words that most often start with a capital letter in the main text
## Daiki (32%): Handled the data pre-processing part of the task
## Nathan (33%): Responsible for creating the matrices of common word triplets and pairs


#######################################################################################################################


## This program simulates 50-word sections according to the Ulysses text. 
## The model's vocabulary is the top m = (approx.) 1000 most common words stored in a vector b.
## The dependency between each word is accounted for up until the two words preceding them.
## Triplets and pairs of adjacent words matrices, T and P, are created and used to generate each word
## with the correct (conditional) probability.

## For comparison, another 50-word sections is simulated by 50 repeated independent sampling of the
## top m common words based on common word frequencies (i.e., ignoring dependency between words).

## Finally, the model's dictionary is updated such that the words that most often (> 50%) start with 
## a capital letter also start with a capital letter in the simulated text. Another 50-word sections 
## is then simulated with the updated vocabulary.


#######################################################################################################################


# 3: Read the text file

# setwd("C:/Users/maruw/ASEAN-Group-25") ## set the working directory
a <- scan("4300-0.txt", what="character", skip=73, nlines=32858-73) ## read the text file
a <- gsub("_(", "", a, fixed=TRUE) ## remove "_("


#######################################################################################################################


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


#######################################################################################################################


# 5: Separate the punctuation marks

a <- split_punct(a, ",") ## separate ","
a <- split_punct(a, ".") ## separate "."
a <- split_punct(a, ";") ## separate ";"
a <- split_punct(a, "!") ## separate "!"
a <- split_punct(a, ":") ## separate ":"
a <- split_punct(a, "?") ## separate "?"


#######################################################################################################################


# 6: Create a vector of top m most common words called "b"

## replace all capital letters with lower case letters
a_low <- tolower(a) 

## create a vector of unique words in a_low
a_uni <- unique(a_low) 

## detect which element of a_uni is element of a_low
a_match <- match(a_low, a_uni) 

## count up the number of occurrences of each unique word
a_tab <-tabulate(a_match) 

## sort a_tab in descending order
a_order <- sort(a_tab, decreasing=TRUE) 

## consider the top m = 1000 words
m = 1000 

## gain the number of occurrences threshold for the top m words
threshold <- a_order[m] 

## store all words that has occurrences higher than the threshold into a vector b
b <- a_uni[a_tab >= threshold] 


#######################################################################################################################


# 7: Create the matrices of common word triplets "T" and pairs "P"

ia <- match(a_low, b) ## vector of indices of b matched to a_low

## create three-column matrix of indices where each row is a triplet of adjacent words
## first column is ia, second column is ia shifted up by 1, and third column is ia shifted up by 2
## this implies that the last two rows of ia will be unusable
pre_T <- cbind(ia[1:(length(ia)-2)], ia[2:(length(ia)-1)], ia[3:length(ia)]) 

## remove rows of NA from pre_T to get the final matrix of triplets of adjacent words
T <- pre_T[!is.na(rowSums(pre_T, na.rm=FALSE)),] 

## create a two-column matrix of indices where each row is a pair of adjacent words 
## first column is ia, second column is ia shifted up by 1
## this implies that the last row of ia will be unusable
pre_P <- cbind(ia[1:(length(ia)-1)], ia[2:length(ia)])

## remove rows that contain NA to get the final matrix of pairs of adjacent words
P <- pre_P[!is.na(rowSums(pre_P, na.rm=FALSE)),]


#######################################################################################################################


# 8: Simulate 50-word sections

#### simulate first word

## ia with NA entries removed
ia_mod <- na.omit(ia) 

## randomly choose first word index from the text that exists in b based on common word frequencies
sim <- sample(ia_mod, size=1)


#### simulate second word

## the function generate2() takes 3 inputs: 
## (i) a vector of simulated indices with length 1 or 2, v
## (ii) a vector of indices of the words that exists in b, ia_mod 
## (iii) the pair of adjacent words matrix, P
## and outputs the next simulated index depending on the index preceding it, next_index.

generate2 <- function(v, ia_mod, P){
  ## col_P contains all rows of P whose first column is the last entry of v
  col_P <- P[which(P[,1] == v[length(v)]),]
  
  ## if col_P is not empty, randomly sample the second column of col_P 
  ## otherwise sample the word based on common word frequencies
  if (length(col_P != 0)){
    next_index <- sample(col_P[,2], size = 1)
  }
  else{
    next_index <- sample(ia_mod, size = 1)
  }
  
  return(next_index)
}

## simulate the second index
next_index <- generate2(sim, ia_mod, P) 

## column bind the simulated index to sim
sim <- cbind(sim, next_index) 

## The function generate3() takes 4 inputs:
## (i) a vector of simulated index of with length 2, v
## (ii) a vector of indices of the words that exists in b, ia_mod 
## (iii) the pair of adjacent words matrix, P
## (iv) the triplet of adjacent words matrix, T
## and outputs the next simulated index depending on the two indices preceding it, next_index.

generate3 <- function(v, ia_mod, P, T) {
  ## col_T contains all rows of T whose first and second column is the first and second entry of v, respectively
  col_T <- T[which((T[,1] == v[1]) & (T[,2] == v[2])),]  
  
  ## if col_T is not empty, randomly sample the third column of col_T
  ## otherwise, generate the next index based on the last entry of v
  if (length(col_T) != 0){
    next_index <- sample(col_T[,3], size = 1)
  }
  else {
    next_index <- generate2(v, ia_mod, P)
  }
  return(next_index) 
}


### simulate the rest 48 words

for (p in 1:48){
  ## generate the next index based on the two index preceding it
  next_index <- generate3(sim[p:p+1], ia_mod, P, T)
  
  ## column bind the generated index to sim
  sim <- cbind(sim, next_index)
}

## match the simulated indices to b
sim_text <- b[sim] 

## print the simulated 50-word sections
cat(sim_text) 

#######################################################################################################################


# 9: Simulate 50-word sections where the word probabilities are based on the common word frequencies

## sample 50 word indices from ia_mod
sim2 <- sample(ia_mod, size = 50, replace = TRUE) 

## match the simulated indices to b
sim2_text <- b[sim2] 

## print the simulated text
cat(sim2_text) 

### compare the output of step 8 and 9
## The text generated based on T tends to be more meaningful than the text simulated by independent sampling 
## based on common word frequencies because all words of the former one are related to each other;
## on the other hand, the latter is simply 50 repeated independent sampling.


#######################################################################################################################


# 10: Create a modified version of b such that words that most often start with a capital letter 
# also start with capital letter in the simulated text


## The function capitalise() takes 1 input: a vector of one word, x
## and outputs the same word with its first letter capitalised

capitalise <- function(x) {
  ## replace the first letter of x by its capitalised version
  substr(x, 1, 1) <- toupper(substr(x, 1, 1)) 
  return(x) 
}

## capitalise all the first letters of each word in a
capital <- capitalise(a)

## obtain indices of words starting with a capital letter in a without NA entries
capz_index <- na.omit(match(capital, a)) 

## obtain the words starting with a capital letter in a
capz <- a[capz_index] 

## obtain all the unique capital words
capz_uni <- unique(capz) 

## obtain the unique capital letters that are included in b with the NA entries removed
capz_unib <- capitalise(b[na.omit(match(tolower(capz_uni), b))]) 

## replace capital letters with lower case letters
low_capz <- tolower(capz_unib) 

## obtain the frequency of words starting in capital letters in the same order as capz_unib
count_capital <- NULL
for (k in 1:length(capz_unib)){
  entry <- length(which(a == capz_unib[k])) 
  count_capital <- cbind(count_capital, entry) 
} 

## obtain the frequency of words in capz_unib that start with a lower case letter
count_low <- NULL
for (k in 1:length(low_capz)){
  entry <- length(which(a == low_capz[k])) 
  count_low <- cbind(count_low, entry) 
} 

## calculate the probability of each word starting with a capital letter
probability <- count_capital/(count_capital + count_low) 

## obtain the words that start with a capital letter more than 50% of the time
freq_cap <- capz_unib[which(probability > 0.5)] 

## convert the words in freq_cap to lowercase words 
low_freq_cap <- tolower(freq_cap) 

## clone b into b_update
b_update <- b 

## replace all the words in b_update that most often start with a capital letter with its capitalized version
b_update[match(low_freq_cap, b_update)] <- freq_cap 


### simulate 50-word sections with updated b 

## vector of indices matching the full text of b_update
ia2 <- match(a, b_update)

## remove NA from ia2
ia2_mod <- na.omit(ia2) 

## simulate the first index by randomly selecting a word from the text that exists in b_update
sim3 <- sample(ia2_mod, size=1) 

## simulate the second index 
next_index <- generate2(sim3, ia2_mod,P) 

## column bind the second index to sim3
sim3 <- cbind(sim3, next_index) 

## simulate the rest 48 indices
for (p in 1:48){
  ## generate the next index using the previous two words
  next_index <- generate3(sim3[p:p+1], ia2_mod, P, T) 
  
  ## column bind the previous index to sim3
  sim3 <- cbind(sim3, next_index) 
} 

## match the indices of the generated words to b_update
sim3_text <- b_update[sim3] 

## print the corresponding sample of words
cat(sim3_text) 

