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

# Usuwamy wiersze, w których 'ExamScore' jest większe niż 100
data_cleaned <- subset(czynniki2, Exam_Score <= 100)

#Mapa brakujących wartości
install.packages("naniar")
library(naniar)
vis_miss(czynniki2, cluster = TRUE, sort_miss = TRUE)

#Cyszczenie danych Sleep_Hours
install.packages("mice")
library(mice)
# Załaduj pakiet mice
library(mice)

# Upewnijmy się, że zmienne kategoryczne mają odpowiedni typ i poziomy
czynniki2$Family_Income <- factor(czynniki2$Family_Income, levels = c("Low", "Medium", "High"))
czynniki2$Teacher_Quality <- factor(czynniki2$Teacher_Quality, levels = c("Low", "Medium", "High"))
czynniki2$Parental_Education_Level <- factor(czynniki2$Parental_Education_Level, levels = c("High School", "College", "Postgraduate"))
czynniki2$Distance_from_Home <- factor(czynniki2$Distance_from_Home, levels = c("Near", "Moderate", "Far"))
czynniki2$Motivation_Level <- factor(czynniki2$Motivation_Level, levels = c("Low", "Medium", "High"))
czynniki2$Internet_Access <- factor(czynniki2$Internet_Access, levels = c("Yes", "No"))
czynniki2$Learning_Disabilities <- factor(czynniki2$Learning_Disabilities, levels = c("No", "Yes"))
czynniki2$School_Type <- factor(czynniki2$School_Type, levels = c("Public", "Private"))
czynniki2$Extracurricular_Activities <- factor(czynniki2$Extracurricular_Activities, levels = c("No", "Yes"))
czynniki2$Peer_Influence <- factor(czynniki2$Peer_Influence, levels = c("Positive", "Negative", "Neutral"))
czynniki2$Gender <- factor(czynniki2$Gender, levels = c("Male", "Female"))

# Sprawdzenie typu zmiennych po zmianach
str(czynniki2)

# Definiowanie metod imputacji
method <- make.method(czynniki2)

# Ustalamy metodę imputacji dla zmiennych numerycznych (np. Sleep_Hours, Previous_Scores) jako 'pmm'
method[c("Sleep_Hours", "Previous_Scores", "Tutoring_Sessions", "Physical_Activity", "Exam_Score")] <- "pmm"

# Ustalamy metodę imputacji dla zmiennych kategorycznych:
# 'logreg' dla zmiennych kategorycznych z dwoma poziomami (np. Gender, Internet_Access)
method[c("Internet_Access", "Gender", "Extracurricular_Activities")] <- "logreg"

# 'polyreg' dla zmiennych kategorycznych z więcej niż dwoma poziomami (np. Family_Income, Teacher_Quality, Parental_Education_Level, Distance_from_Home)
method[c("Family_Income", "Teacher_Quality", "Parental_Education_Level", "Distance_from_Home", 
         "Motivation_Level", "Learning_Disabilities", "School_Type", "Peer_Influence")] <- "polyreg"

# Sprawdzenie przypisanych metod
print(method)

# Imputacja danych
czynnikisleep <- mice(czynniki2, m = 5, method = method, seed = 500)

# Pobranie pierwszego zestawu danych z imputacjami
czynniki3 <- complete(czynnikisleep, 1)

# Sprawdzamy wynik imputacji dla zmiennych
head(czynniki3)
LICZBA_NA <- data.frame(liczba_brakow = colSums(is.na(czynniki3)))
LICZBA_NA

# Zainstaluj pakiet gridExtra, jeśli jeszcze go nie masz
install.packages("gridExtra")

# Załaduj pakiet gridExtra
library(gridExtra)
library(ggplot2)
# Rysowanie boxplotów dla zmiennej Exam_Score
b1 <- ggplot(czynniki3, aes(y = Exam_Score)) +
  geom_boxplot() +
  labs(title = "Exam_Score")

# Rysowanie histogramów dla zmiennej Exam_Score
h1 <- ggplot(czynniki3) +
  aes(x = Exam_Score) +
  geom_histogram(bins = 30, fill = "#0c4c8a") +
  theme_minimal()

# Wykorzystanie grid.arrange do rozmieszczenia wykresów
grid.arrange(b1, h1, nrow = 1)

# Załaduj wymagane biblioteki
library(ggplot2)

# Lista zmiennych kategorycznych bez Gender
categorical_vars <- c("Family_Income", "Teacher_Quality", "Parental_Education_Level", 
                      "Distance_from_Home", "Motivation_Level", "School_Type", 
                      "Peer_Influence")

# Kolory dla Gender: 0 = różowy (Female), 1 = niebieski (Male)
gender_colors <- c("0" = "pink", "1" = "blue")

# Kolory dla pozostałych zmiennych: 0 = szary, 1 = zielony, 2 = żółty, 3 = czerwony
fill_colors <- c("0" = "gray", "1" = "green", "2" = "yellow", "3" = "red")

# Wykres dla Gender
gender_plot <- ggplot(czynniki3, aes(x = Gender, fill = as.factor(Gender))) +
  geom_bar() +
  scale_fill_manual(values = gender_colors) +
  labs(title = "Bar Plot of Gender", x = "Gender", y = "Count") +
  theme_minimal()
print(gender_plot)

# Tworzenie wykresów słupkowych dla każdej zmiennej kategorycznej z odpowiednimi kolorami
for (var in categorical_vars) {
  plot <- ggplot(czynniki3, aes_string(x = var, fill = as.factor(czynniki3[[var]]))) +
    geom_bar() +
    scale_fill_manual(values = fill_colors) +
    labs(title = paste("Bar Plot of", var), x = var, y = "Count") +
    theme_minimal()
  
  print(plot)
}


#Wykresy pudełkowe dla facktorów
for (i in seq_along(categorical_vars)) {
  var <- categorical_vars[i]
  plot <- ggplot(czynniki3, aes_string(x = var, y = "Exam_Score", fill = var)) +  # Użycie zmiennej kategorycznej do kolorowania
    geom_boxplot(color = "black") +
    labs(title = paste("Boxplot of Exam_Score by", var), x = var, y = "Exam Score") +
    scale_fill_manual(values = rainbow(length(unique(czynniki3[[var]])))) +  # Użycie różnych kolorów dla każdej kategorii
    theme_minimal()
  
  print(plot)
}

library(ggplot2)

# Kolory: 0 = niebieski, 1 = błękitny
fill_colors <- c("0" = "blue", "1" = "lightblue")

# Funkcja do rysowania wykresów kołowych
create_pie_chart <- function(data, var, fill_colors) {
  ggplot(data, aes(x = "", fill = as.factor(data[[var]]))) +
    geom_bar(width = 1, color = "white") +
    coord_polar(theta = "y") +
    scale_fill_manual(values = fill_colors) +
    labs(title = paste("Pie Chart of", var), fill = "Categories") +
    theme_void() +
    theme(legend.position = "top")
}

# Wykresy kołowe dla zmiennych: Internet_Access, Learning_Disabilities, Extracurricular_Activities
pie_internet <- create_pie_chart(czynniki3, "Internet_Access", fill_colors)
pie_learning_disabilities <- create_pie_chart(czynniki3, "Learning_Disabilities", fill_colors)
pie_extracurricular_activities <- create_pie_chart(czynniki3, "Extracurricular_Activities", fill_colors)

# Wyświetlamy wykresy
print(pie_internet)
print(pie_learning_disabilities)
print(pie_extracurricular_activities)

# Biblioteki
library(ggplot2)

# Lista zmiennych liczbowych
numeric_vars <- c("Hours_Studied", "Attendance", "Previous_Scores", "Sleep_Hours", 
                  "Tutoring_Sessions", "Physical_Activity")

# Histogramy oraz scatter ploty dla każdej zmiennej liczbowej
for (var in numeric_vars) {
  
  # Histogram
  hist_plot <- ggplot(czynniki3, aes_string(x = var)) +
    geom_histogram(bins = 30, fill = "#0c4c8a", color = "black") +
    labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
    theme_minimal()
  
  # Scatter Plot zależny od Exam_Score
  scatter_plot <- ggplot(czynniki3, aes_string(x = var, y = "Exam_Score")) +
    geom_point(color = "#0c4c8a") +
    labs(title = paste("Scatter Plot of Exam_Score vs", var), x = var, y = "Exam Score") +
    theme_minimal()
  
  # Wyświetlanie wykresów 
  print(hist_plot)  # Wyświetla histogram
  print(scatter_plot)  # Wyświetla scatter plot
}

#Statystyki opisowe
# Pakiet
install.packages("moments")
library(moments)

# Sprawdźmy dane wejściowe
str(czynniki3$Exam_Score)  # Sprawdzenie struktury danych

# Obliczanie poszczególnych statystyk( testowe bo inaczej nie działało)
Min <- min(czynniki3$Exam_Score, na.rm = TRUE)
Max <- max(czynniki3$Exam_Score, na.rm = TRUE)
Kwartyl_dolny <- quantile(czynniki3$Exam_Score, 0.25, na.rm = TRUE)
Mediana <- median(czynniki3$Exam_Score, na.rm = TRUE)
Kwartyl_gorny <- quantile(czynniki3$Exam_Score, 0.75, na.rm = TRUE)
Srednia <- mean(czynniki3$Exam_Score, na.rm = TRUE)
Odch_std <- sd(czynniki3$Exam_Score, na.rm = TRUE)
IQR_value <- IQR(czynniki3$Exam_Score, na.rm = TRUE)
Skosnosc <- skewness(czynniki3$Exam_Score, na.rm = TRUE)
Kurtoza_value <- kurtosis(czynniki3$Exam_Score, na.rm = TRUE)

# Wyniki wstępne
print(c(Min, Max, Kwartyl_dolny, Mediana, Kwartyl_gorny, Srednia, Odch_std, IQR_value, Skosnosc, Kurtoza_value))

# Obliczanie procentowych odchyleń
Odch_std_pct <- (Odch_std / Srednia) * 100
Odch_cwiartkowe_pct <- (IQR_value / Mediana) * 100
Odchylenie_cwiartkowe <- IQR_value / 2

# Wyniki obliczeń
print(c(Odch_std_pct, Odch_cwiartkowe_pct, Odchylenie_cwiartkowe))
# Ramki danych z obliczonymi statystykami
statystyki <- data.frame(
  Min = round(Min, 3),
  Max = round(Max, 3),
  "Kwartyl dolny" = round(Kwartyl_dolny, 3),
  Mediana = round(Mediana, 3),
  "Kwartyl górny" = round(Kwartyl_gorny, 3),
  Średnia = round(Srednia, 3),
  "Odch. std." = round(Odch_std, 3),
  IQR = round(IQR_value, 3),
  "Skośność" = round(Skosnosc, 3),
  "Kurtoza" = round(Kurtoza_value, 3),
  "Odch. std. w %" = round(Odch_std_pct, 3),
  "Odch. ćwiartkowe w %" = round(Odch_cwiartkowe_pct, 3),
  "Odchylenie ćwiartkowe" = round(Odchylenie_cwiartkowe, 3)
)

#Tabela
print(statystyki)

czynniki_kor <- czynniki3
#Standaryzowanie danych
czynniki_kor$Parental_Involvement <- as.numeric(factor(
  czynniki_kor$Parental_Involvement,
  levels = c("Low", "Medium", "High"),
  labels = c(1, 2, 3)
))
czynniki_kor$Access_to_Resources <- as.numeric(factor(
  czynniki_kor$Access_to_Resources,
  levels = c("High", "Medium", "Low"),
  labels = c(1, 2, 3)
))
czynniki_kor$Extracurricular_Activities <- ifelse(
  czynniki_kor$Extracurricular_Activities == "Yes", 1, 0
)
czynniki_kor$Motivation_Level <- as.numeric(factor(
  czynniki_kor$Motivation_Level,
  levels = c("Low", "Medium", "High"),
  labels = c(1, 2, 3)
))
czynniki_kor$Internet_Access <- ifelse(
  czynniki_kor$Internet_Access == "Yes", 1, 0
)
czynniki_kor$Family_Income <- as.numeric(factor(
  czynniki_kor$Family_Income,
  levels = c("Low", "Medium", "High"),
  labels = c(1, 2, 3)
))
czynniki_kor$Teacher_Quality <- as.numeric(factor(
  czynniki_kor$Teacher_Quality,
  levels = c("Low", "Medium", "High"),
  labels = c(1, 2, 3)
))
czynniki_kor$Distance_from_Home <- ifelse(czynniki_kor$Distance_from_Home == "Near", 0,
                                          ifelse(czynniki_kor$Distance_from_Home == "Moderate", 1,
                                                 ifelse(czynniki_kor$Distance_from_Home == "Far", 2, NA)))
czynniki_kor$Parental_Education_Level <- ifelse(czynniki_kor$Parental_Education_Level == "High School", 0,
                                                ifelse(czynniki_kor$Parental_Education_Level == "College", 1,
                                                       ifelse(czynniki_kor$Parental_Education_Level == "Postgraduate", 2, NA)))
czynniki_kor$Learning_Disabilities <- ifelse(czynniki_kor$Learning_Disabilities == "Yes", 1, 0)

czynniki_kor$Peer_Influence <- ifelse(czynniki_kor$Peer_Influence == "Negative", 0,
                                      ifelse(czynniki_kor$Peer_Influence == "Neutral", 1,
                                             ifelse(czynniki_kor$Peer_Influence == "Positive", 2, NA)))
czynniki_kor$Gender <- ifelse(czynniki_kor$Gender == "Male", 1, 0)

czynniki_kor$School_Type <- ifelse(czynniki_kor$School_Type == "Public", 0,
                                   ifelse(czynniki_kor$School_Type == "Private", 1, NA))

# Korelacja dla wszystkich zmiennych numerycznych w czynniki3
correlations <- data.frame(cor(czynniki_kor))

#Wyniki korelacji
print(correlations)

# Pakiet
library(corrplot)

# Macierz korelacji dla danych numerycznych
cor_matrix <- cor(czynniki_kor, use = "complete.obs")

# Wizualizacja macierzy korelacji
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)        

install.packages("ggstatsplot")
library(ggstatsplot)

# 5. Wykresy korelacyjne ggscatterstats

# Korelacja Exam_Score vs Hours_Studied
ggscatterstats(
  data = czynniki3,
  x = Hours_Studied,
  y = Exam_Score,
  title = "Korelacja: Wynik egzaminu a liczba godzin nauki",
  xlab = "Liczba godzin nauki",
  ylab = "Wynik egzaminu",
  ggtheme = ggplot2::theme_minimal()
)

# Korelacja Exam_Score vs Previous_Scores
ggscatterstats(
  data = czynniki3,
  x = Previous_Scores,
  y = Exam_Score,
  title = "Korelacja: Wynik egzaminu a wcześniejsze wyniki",
  xlab = "Poprzednie wyniki",
  ylab = "Wynik egzaminu",
  ggtheme = ggplot2::theme_minimal()
)
#Tworzymy model regresji liniowej z Physical_Activity i Sleep_Hours jako zmiennymi objaśniającymi
model1 <- lm(Exam_Score ~ Physical_Activity + Sleep_Hours, data = czynniki3)

plot1 <- ggcoefstats(
  model1,
  title = "Wpływ aktywności fizycznej i snu na wynik egzaminu",
  xlab = "Współczynniki regresji",
  ggtheme = ggplot2::theme_minimal()
)

# Rysowanie wykresu
print(plot1)

ggscatterstats(
  data = data,
  x = Attendance,
  y = Exam_Score,
  title = "Korelacja: Wynik egzaminu a frekwencja",
  xlab = "Frekwencja (%)",
  ylab = "Wynik egzaminu",
  ggtheme = ggplot2::theme_minimal()
)


ggbetweenstats(
  data = czynniki3,
  x = Gender,
  y = Exam_Score,
  title = "Porównanie wyników egzaminu wg płci",
  xlab = "Płeć",
  ylab = "Wynik egzaminu",
  ggtheme = ggplot2::theme_minimal()
)

ggbetweenstats(
  data = czynniki3,
  x = Access_to_Resources,
  y = Exam_Score,
  title = "Porównanie wyników egzaminu wg dostępu do zasobów edukacyjnych",
  xlab = "Zasoby edukacyjne",
  ylab = "Wynik egzaminu",
  ggtheme = ggplot2::theme_minimal()
)

ggbetweenstats(
  data = czynniki3,
  x = Parental_Involvement,
  y = Exam_Score,
  title = "Porównanie wyników egzaminu wg poziomu zaagnażowania rodziców",
  xlab = "Poziom zaangażowania rodziców",
  ylab = "Wynik egzaminu",
  ggtheme = ggplot2::theme_minimal()
)

