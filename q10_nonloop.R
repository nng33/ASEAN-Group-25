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

# Example vector of words
words_vector <- c("London", "london", "London", "LONDON", "paris", "London", "london",
                  "paris", "Paris", "Paris", "Africa", "africa")

# Convert all words to lowercase for case-insensitive comparison
lowercase_words <- tolower(a)

# Function that converts all the first letters of each word into a capital
firstup <- function(x) {
        substr(x, 1, 1) <- toupper(substr(x, 1, 1))
        return(x)
}
# All uppercase words
uppercase_words <- firstup(a)

# Create a table of counts for both capital and non-capitalized words
counts <- table(Counts = a[a == uppercase_words | a == lowercase_words])

# Calculate the ratio for each unique word using the sapply function
# Within the sapply function, apply a newly created function that takes the ratio
word_ratios <- sapply(unique(a), function(word) {
        counts[word] / (counts[word] + counts[tolower(word)])
})

result_df <- data.frame(word = names(word_ratios), ratio = word_ratios, row.names = NULL)
freq_cap <- word_ratios[which(word_ratios > 0.5)]
# Code to update b required
