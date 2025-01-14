file_path <- "C:/Users/fifi1/OneDrive/Pulpit/Zarządzanie big data/R/Projekt/laluz.csv"

# Wczytanie pliku
data <- read.csv(file_path, sep = ";")
data <- data[, -5]
data <- na.omit(data)
View(data)
head(data)
dim(data)
colnames(data)
summary(data)
unique(data$product_type)
unique(data$sprayed)
sapply(data, function(x) sum(is.na(x)))
# Wczytanie pliku

# Filtrowanie danych
regular_bulb__without_spray_filtered_data <- subset(data, product_type == "regular bulb" & sprayed == "none")
doubled_bulb_without_spray_filtered_data <- subset(data, product_type == "DoubleBulb" & sprayed == "none")

regular_bulb__without_spray_mean_time <- mean(regular_bulb__without_spray_filtered_data$time_in_hours, na.rm = TRUE)
doubled_bulb__without_spray_mean_time <- mean(doubled_bulb_without_spray_filtered_data$time_in_hours, na.rm = TRUE)


# TEST F aby sprawdzić czy wariancje są równe czy różne żeby potem do testu wrzucić 
var_test_result <- var.test(
  regular_bulb__without_spray_filtered_data$time_in_hours,
  doubled_bulb_without_spray_filtered_data$time_in_hours
)

# Wynik
print(var_test_result)

# Test sprawdzający rozkład normalny - shapiro wilka -- 0.05

# w pierwszym zadaniu dodajemy dwa testy i usuwamy test t-studenta. Dodajemy
# test shapiro wilka żeby sprawdzić czy mają rozkład normalny i zdecydować
# którego tesut użyć 
# Dopisać hipotezy zerowa itp. 
# dodać test manna whitneya bo jeden ze zbiorów jest nie normalny 
# dodać wykresy do shapiro wilka

# Test normalności - Shapiro-Wilk
shapiro_test_regular_bulb <- shapiro.test(regular_bulb__without_spray_filtered_data$time_in_hours)
shapiro_test_doubled_bulb <- shapiro.test(doubled_bulb_without_spray_filtered_data$time_in_hours)

# Wyświetlanie wyników testów
print(shapiro_test_regular_bulb)
print(shapiro_test_doubled_bulb)

# Wykres Q-Q dla obu zbiorów danych
par(mfrow=c(1,2))  # Ustawienie wykresów obok siebie
qqnorm(regular_bulb__without_spray_filtered_data$time_in_hours, main = "Q-Q plot: Regular Bulb")
qqline(regular_bulb__without_spray_filtered_data$time_in_hours)

qqnorm(doubled_bulb_without_spray_filtered_data$time_in_hours, main = "Q-Q plot: Doubled Bulb")
qqline(doubled_bulb_without_spray_filtered_data$time_in_hours)


# Wyświetlenie wyniku
print(regular_bulb__without_spray_mean_time)
print(doubled_bulb__without_spray_mean_time)

# Test t-Studenta dla dwóch grup
t_test_result <- t.test(
  regular_bulb__without_spray_filtered_data$time_in_hours,
  doubled_bulb_without_spray_filtered_data$time_in_hours,
  var.equal = FALSE,  # Założenie homogeniczności wariancji
)

# test manna whitneya
# Test Manna-Whitneya
mann_whitney_result <- wilcox.test(regular_bulb__without_spray_filtered_data$time_in_hours, 
                                   doubled_bulb_without_spray_filtered_data$time_in_hours)

# Wyświetlanie wyników testu
print(mann_whitney_result)

# Wyświetlenie wyników testu
print(t_test_result)



bulbs_with_spray <- subset(data, sprayed == "CoatItYourself")
nrow(bulbs_with_spray)  # Liczba wierszy w podzbiorze

total_time_spray <- sum(bulbs_with_spray$time_in_hours, na.rm = TRUE)
print(total_time_spray)

# 1 butelka to 735924 psiknięcia 100 / 735924

print(100 / 735924 * 100) # 100 - cena za butelkę, 735924 liczba psiknięć, 100 - żeby wyszły zł

print0.0001358836 * 100 

# 0.01358836 -> tyle groszy za psik 0,014 zaokrąglone




## Cena jednej godziny użytkowania żarówki regularnej be

regular_bulb_with_spray <- subset(data, product_type == 'regular bulb' & sprayed == "CoatItYourself");

sum_regular_bulb_with_spray_time <- sum(regular_bulb_with_spray$time_in_hours, na.rm = TRUE)
sum_regular_bulb_with_spray_price <- sum(regular_bulb_with_spray$price_in_PLN, na.rm = TRUE)



price_per_hour_for_regular_bulb_with_spray <- sum_regular_bulb_with_spray_price / sum_regular_bulb_with_spray_time;
print(price_per_hour_for_regular_bulb_with_spray)

# 0.003039603 - cena użytkowania godziny żarówki w zł ~ 30gr  + 1.4gr



## Cena jednej godziny użytkowania żarówki regularnej bez spraya

regular_bulb_without_spray <- subset(data, product_type == 'regular bulb' & sprayed == "none");

sum_regular_bulb_without_spray_time <- sum(regular_bulb_without_spray$time_in_hours, na.rm = TRUE)
sum_regular_bulb_without_spray_price <- sum(regular_bulb_without_spray$price_in_PLN, na.rm = TRUE)

price_per_hour_for_regular_bulb_without_spray <- sum_regular_bulb_without_spray_price / sum_regular_bulb_without_spray_time;
print(price_per_hour_for_regular_bulb_without_spray)

# 0.003442773 - cena użytkowania godziny żarówki w zł ~ 34gr



## Cena jednej godziny użytkowania żarówki double bez spraya

double_bulb_without_spray <- subset(data, product_type == 'DoubleBulb' & sprayed == "none");

sum_double_bulb_without_spray_time <- sum(double_bulb_without_spray$time_in_hours, na.rm = TRUE)
sum_double_bulb_without_spray_price <- sum(double_bulb_without_spray$price_in_PLN, na.rm = TRUE)

price_per_hour_for_double_bulb_without_spray <- sum_double_bulb_without_spray_price / sum_double_bulb_without_spray_time;
print(price_per_hour_for_double_bulb_without_spray)

# 0.004478454 - cena użytkowania godziny żarówki w zł ~ 44gr



## Cena jednej godziny użytkowania żarówki double z sprayem

double_bulb_with_spray <- subset(data, product_type == 'DoubleBulb' & sprayed == "CoatItYourself");

sum_double_bulb_with_spray_time <- sum(double_bulb_with_spray$time_in_hours, na.rm = TRUE)
sum_double_bulb_with_spray_price <- sum(double_bulb_with_spray$price_in_PLN, na.rm = TRUE)

price_per_hour_for_double_bulb_with_spray <- sum_double_bulb_with_spray_price / sum_double_bulb_with_spray_time;
print(price_per_hour_for_double_bulb_with_spray)

# 0.004619901 - cena użytkowania godziny żarówki w zł ~ 46gr + 1.4gr

# Dodać wykres słupkowy do danych





# 

# krótszy sposób
#mean_time_regular_bulb <- mean(regular_bulb_without_spray$time_in_hours)
#mean_price_regular_bulb <- mean(regular_bulb_without_spray$price_in_PLN)
#print(mean_price_regular_bulb / mean_time_regular_bulb)





## Badamy na ile lat starczy dana żarówka, nie uwzględniamy lat przystępnych

# Regularna bez spraya 
mean_time_regular_bulb_without_spray <- mean(regular_bulb_without_spray$time_in_hours);
regular_bulb_without_spray_day_count <- mean_time_regular_bulb_without_spray / 4;
regular_bulb_without_spray_year_count <- regular_bulb_without_spray_day_count / 365;
print(regular_bulb_without_spray_year_count)
# lat  1.981916

# Regularna ze sprayem 
mean_time_regular_bulb_with_spray <- mean(regular_bulb_with_spray$time_in_hours);
regular_bulb_with_spray_day_count <- mean_time_regular_bulb_with_spray / 4;
regular_bulb_with_spray_year_count <- regular_bulb_with_spray_day_count / 365;
print(regular_bulb_with_spray_year_count)
# 2.233281 lat

# double bez spraya 
mean_time_double_bulb_without_spray <- mean(double_bulb_without_spray$time_in_hours);
double_bulb_without_spray_day_count <- mean_time_double_bulb_without_spray / 4;
double_bulb_without_spray_year_count <- double_bulb_without_spray_day_count / 365;
print(double_bulb_without_spray_year_count)
# lat  3.058663

# double ze sprayem 
mean_time_double_bulb_with_spray <- mean(double_bulb_with_spray$time_in_hours);
double_bulb_with_spray_day_count <- mean_time_double_bulb_with_spray / 4;
double_bulb_with_spray_year_count <- double_bulb_with_spray_day_count / 365;
print(double_bulb_with_spray_year_count)
# lat  2.962882

# dodać wykres słupkowy do danych


##  Policz, na jak długo starczy w tym modelowym gospodarstwie domowym jedno opakowanie sprayu.
# Zakłądamy że mamy 10 żarówek w gospodarstwie i żarówki średnio świecą się 4h dziennie, co ooznacza że dziennie musimy użyć sprayu 40 razy
bulb_count_in_house <- 20;
bulb_light_per_day <- 4;
spray_count_day <- total_time_spray / bulb_light_per_day / bulb_count_in_house;
spray_count_years <- total_time_spray / bulb_light_per_day / bulb_count_in_house / 365;

print(spray_count_day) # 9199.057
print(spray_count_years) # 25.2029

# Zastanowic sie nad wykresem który będzie przedstawiał rozkład z różną ilością żarówek np.




# dla producenta bardziej opłąca się sprzedaż żarówek double bez sprayu, ze wzfględu na najwyższą cenę i najdłuższy czas pracy przez co uzytkownicy będą zadowoleni
# Spray musiałby być mniejszy / lub starczało na mniejszą ilość użyć bo 25 lat dużo 
# Bez sensu jest to żeby uzytkownik psikał co godzinę więc przydałoby się wydfłużyc czas działania np. do psiknięcia raz dziennie
# Raz psiknąć na całą żywotność żarówki

