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

#Mapa brakujących wartości
install.packages("naniar")
library(naniar)
vis_miss(czynniki2, cluster = TRUE, sort_miss = TRUE)


#Czyszczenie danych
median_sleep <- median(czynniki2$Sleep_Hours, na.rm = TRUE)
replace_with_random <- function() {
  return(median_sleep + sample(c(-1, 0, 1), 1))
}
czynniki2$Sleep_Hours <- ifelse(
  is.na(czynniki2$Sleep_Hours),
  replace_with_random(),
  czynniki2$Sleep_Hours
)
LICZBA_NA2 <- data.frame(liczba_brakow = colSums(is.na(czynniki2)))
LICZBA_NA2
czynniki3 <- subset(czynniki2, !is.na(Family_Income))
LICZBA_NA2 <- data.frame(liczba_brakow = colSums(is.na(czynniki3)))
LICZBA_NA2
median_sleep <- median(czynniki3$Sleep_Hours, na.rm = TRUE)
replace_with_random <- function() {
  return(median_sleep + sample(c(-1, 0, 1), 1))
}
czynniki3$Sleep_Hours <- ifelse(
  is.na(czynniki2$Sleep_Hours),
  replace_with_random(),
  czynniki3$Sleep_Hours
)
czynniki3 <- subset(czynniki3, !is.na(Exam_Score))

#Standaryzowanie danych

czynniki3$Parental_Involvement <- as.numeric(factor(
  czynniki3$Parental_Involvement,
  levels = c("Low", "Medium", "High"),
  labels = c(1, 2, 3)
))
czynniki3$Access_to_Resources <- as.numeric(factor(
  czynniki3$Access_to_Resources,
  levels = c("High", "Medium", "Low"),
  labels = c(1, 2, 3)
))
czynniki3$Extracurricular_Activities <- ifelse(
  czynniki3$Extracurricular_Activities == "Yes", 1, 0
)
czynniki3$Motivation_Level <- as.numeric(factor(
  czynniki3$Motivation_Level,
  levels = c("Low", "Medium", "High"),
  labels = c(1, 2, 3)
))
czynniki3$Internet_Access <- ifelse(
  czynniki3$Internet_Access == "Yes", 1, 0
)
czynniki3$Family_Income <- as.numeric(factor(
  czynniki3$Family_Income,
  levels = c("Low", "Medium", "High"),
  labels = c(1, 2, 3)
))
czynniki3$Teacher_Quality <- as.numeric(factor(
  czynniki3$Teacher_Quality,
  levels = c("Low", "Medium", "High"),
  labels = c(1, 2, 3)
))
czynniki3$Distance_from_Home <- ifelse(czynniki3$Distance_from_Home == "Near", 0,
                                       ifelse(czynniki3$Distance_from_Home == "Moderate", 1,
                                              ifelse(czynniki3$Distance_from_Home == "Far", 2, NA)))
czynniki3$Parental_Education_Level <- ifelse(czynniki3$Parental_Education_Level == "High School", 0,
                                             ifelse(czynniki3$Parental_Education_Level == "College", 1,
                                                    ifelse(czynniki3$Parental_Education_Level == "Postgraduate", 2, NA)))
czynniki3$Learning_Disabilities <- ifelse

czynniki3$Peer_Influence <- ifelse(czynniki3$Peer_Influence == "Negative", 0,
                                   ifelse(czynniki3$Peer_Influence == "Neutral", 1,
                                          ifelse(czynniki3$Peer_Influence == "Positive", 2, NA)))
czynniki3$Gender <- ifelse(
  czynniki3$Gender == "Male", 1, 0
)
Wyja

czynniki3$School_Type <- ifelse(czynniki3$School_Type == "Public", 0,
                                ifelse(czynniki3$School_Type == "Private", 1, NA))