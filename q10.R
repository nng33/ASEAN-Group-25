capital <- firstup(a) # all a with first letter capitalized
ii <- na.omit(match(capital,a)) # indices of capital letters in a
capz <- a[ii] # all first letter capital word in a
capz_uni <- unique(capz) # unique capital words
capz_unib <- firstup(b[na.omit(match(tolower(capz_uni),b))]) # unique capital letters in b
low_capz <- tolower(capz_unib)

# count is number of capital word appearance in a with the same order as capz_unib
count_capital <- NULL
for (k in 1:length(capz_unib)){
    entry <- length(which(a == capz_unib[k]))
    count_capital <- cbind(count_capital,entry)
}

# count_low us number of capital word appear as lower case 
count_low <- NULL
for (k in 1:length(low_capz)){
    entry <- length(which(a == low_capz[k]))
    count_low <- cbind(count_low,entry)
}

probability <- count_capital / (count_capital + count_low)

freq_cap <- capz_unib[which(probability > 0.5)] # words that appear as capital > 50%
low_freq_cap <- tolower(freq_cap)

b_update <- b
b_update[match(low_freq_cap,b_update)] <- freq_cap







