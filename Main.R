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
# Usuwamy każdy examine score NA, jako że będzie nam fałszowało ostateczny wynik
czynniki2 <- subset(czynniki, !is.na(Exam_Score))
LICZBA_NA2 <- data.frame(liczba_brakow = colSums(is.na(czynniki2)))
LICZBA_NA2
typ_danych <- data.frame(typ = sapply(czynniki, class))
typ_danych
#Liczba unikalnych wartości w każdej z tych kolumn
columns_to_check <- c("Hours_Studied", "Attendance", "Parental_Involvement", "Access_to_Resources", 
                      "Extracurricular_Activities", "Sleep_Hours", "Previous_Scores", 
                      "Motivation_Level", "Internet_Access", "Tutoring_Sessions", "Family_Income", 
                      "Teacher_Quality", "School_Type", "Peer_Influence", "Physical_Activity", 
                      "Learning_Disabilities", "Parental_Education_Level", "Distance_from_Home", 
                      "Gender", "Exam_Score")
unique_values_all <- lapply(czynniki2[columns_to_check], unique)

# Wyświetlenie liczby unikalnych wartości dla każdej kolumny
print(unique_values_all)