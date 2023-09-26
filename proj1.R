## Group 25
## Frans : 
## Nathan : s2524152
## Daiki : s2547603

## Contribution to this project

setwd("C:/Users/maruw/ASEAN-Group-25") ## comment out of submitted
a <- scan("4300-0.txt",what="character",skip=73,nlines=32858-73)
a <- gsub("_(","",a,fixed=TRUE)

split_punct <- function(text, punct) {
  ie <- grep(punct, text, fixed=TRUE)  ## detecting the words in text containing punct
  vect <- rep("",times = length(ie)+length(text))  ## empty character output vector 
  pos_punct <- ie+1:length(ie)  ## detecting the position of punct in the new vector
  vect[pos_punct] <- substr(text[ie],nchar(text[ie]),nchar(text[ie]))  ## putting punct in the new vector
  text <- gsub(punct,"",text)  ## removing punct from words in text
  vect[-pos_punct] <- text ## putting the remaining words in the new vector
  return(vect)
}

daiki <- c("An","omnishambles,","in","a","headless","chicken","factory")
split_punct(daiki,",")
