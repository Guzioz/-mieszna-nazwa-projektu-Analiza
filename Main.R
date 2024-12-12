library(RCurl)
x <- getURL('https://raw.githubusercontent.com/Guzioz/Smieszna-nazwa-projektu-Analiza/refs/heads/main/czynniki.csv')
czynniki <- read.csv(text = x)
summary(czynniki)
str(czynniki)
LICZBA_NA <- data.frame(liczba_brakow = colSums(is.na(czynniki)))
LICZBA_NA
WARTOSCI_BEZ_NA <- data.frame(liczba_unikatowych_wartosci = sapply(czynniki, function(x) length(unique(na.omit(x)))))
WARTOSCI_BEZ_NA
typ_danych <- data.frame(typ = sapply(czynniki, class))
typ_danych