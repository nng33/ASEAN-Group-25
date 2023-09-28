## Group 25
## Frans : s2591760
## Nathan : s2524152
## Daiki : s2547603

## Contribution to this project

setwd("C:/Users/maruw/ASEAN-Group-25") ## comment out of submitted
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
ia <- match(a_low, b)
pt <- cbind(ia[1:(length(ia)-2)],ia[2:(length(ia)-1)],ia[3:length(ia)])
T <- pt[!is.na(rowSums(pt,na.rm=FALSE)),]

pt2 <- cbind(ia[1:(length(ia)-1)],ia[2:length(ia)])
P <- pt2[!is.na(rowSums(pt2,na.rm=FALSE)),]

#select 2 words from vector b k[i] and k[j] 

#extract the sub-matrix from T who rows all have k[i] and k[j] in column 1 and 2
#pick 1 element at random from the third column of this extract sub-matrix
#using the sample function 


cat(sample(b[], size = 50, prob = )) ##final function





