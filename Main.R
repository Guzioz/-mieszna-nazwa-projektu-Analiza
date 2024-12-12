library(RCurl)
x <- getURL('https://raw.githubusercontent.com/Guzioz/Smieszna-nazwa-projektu-Analiza/refs/heads/main/czynniki.csv')
czynniki <- read.csv(text = x)