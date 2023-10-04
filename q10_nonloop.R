# Example vector of words
words_vector <- c("London", "london", "London", "LONDON", "paris", "London", "london",
                  "paris", "Paris", "Paris", "Africa", "africa")

# Convert all words to lowercase for case-insensitive comparison
lowercase_words <- tolower(words_vector)

# Function that converts all the first letters of each word into a capital
firstup <- function(x) {
        substr(x, 1, 1) <- toupper(substr(x, 1, 1))
        return(x)
}
# All uppercase words
uppercase_words <- firstup(words_vector)

# Create a table of counts for both capital and non-capitalized words
counts <- table(Counts = words_vector[words_vector == uppercase_words | 
                                words_vector == lowercase_words])

# Calculate the ratio for each unique word using the sapply function
# Within the sapply function, apply a newly created function that takes the ratio
word_ratios <- sapply(unique(words_vector), function(word) {
        counts[word] / (counts[word] + counts[tolower(word)])
})


