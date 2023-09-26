## Frans : 
## Nathan : s2524152
## Daiki Maruyama: s2547603

## Contribution to this project

setwd("C:/Users/maruw/ASEAN-Group-25") ## comment out of submitted
a <- scan("4300-0.txt",what="character",skip=73,nlines=32858-73)
a <- gsub("_(","",a,fixed=TRUE)
