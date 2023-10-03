## Group 25
## Frans : s2591760
## Nathan : s2524152
## Daiki : s2547603

## Contribution to this project

setwd("M:/ASEAN-Group-25") ## comment out of submitted
a <- scan("4300-0.txt",what="character",skip=73,nlines=32858-73)
a <- gsub("_(","",a,fixed=TRUE)

#Question 4
split_punct <- function(text, punct) {
  ie <- grep(punct, text, fixed=TRUE)  ## detecting the words in text containing punct
  vect <- rep("",times = length(ie)+length(text))  ## empty character output vector 
  pos_punct <- ie+1:length(ie)  ## detecting the position of punct in the new vector
  vect[pos_punct] <- substr(text[ie],nchar(text[ie]),nchar(text[ie]))  ## putting punct in the new vector
  text <- gsub(punct,"",text,fixed = TRUE)  ## removing punct from words in text
  vect[-pos_punct] <- text ## putting the remaining words in the new vector
  return(vect)
}

#Question 5
a <- split_punct(a,",") 
a <- split_punct(a,".")
a <- split_punct(a,";")
a <- split_punct(a,"!")
a <- split_punct(a,":")
a <- split_punct(a,"?")
a

#Question 6
a_low <- tolower(a)
a_uni <- unique(a_low)
a_match <- match(a_low, a_uni)
a_tab <-tabulate(a_match)
a_order <- sort(a_tab, decreasing=TRUE)
threshold <- a_order[1000]
b <- a_uni[a_tab >= threshold]

#Question 7
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

###################################################################################

# Simulate first word
dict <- match(a_low,b)
dict <- na.omit(dict)
sim <- NULL
sim <- sample(dict,size=1)

# Simulate second word
#extract the sub-matrix from T who rows all have k[i] and k[j] in column 1 and 2
k_i <- P[which(P[,1] == sim),]
sim <- cbind(sim, sample(k_i[,2], size = 1))


##################################################################################

# create 50
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
      next_index <- match(next_word,b)
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

# No. 9

alt <- NULL
for (k in 1:50){
  new <- sample(dict, size = 1)
  alt <- cbind(alt, new)
}

alt_text <- b[alt]

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}

firstup(a)


