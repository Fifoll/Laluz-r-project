file_path <- "C:/Users/fifi1/OneDrive/Pulpit/Zarządzanie big data/R/Projekt/laluz.csv"

# Wczytanie pliku
data <- read.csv(file_path, sep = ";")
data <- data[, -5]
data <- na.omit(data)
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


# Wyświetlenie wyniku
print(regular_bulb__without_spray_mean_time)
print(doubled_bulb__without_spray_mean_time)

# Test t-Studenta dla dwóch grup
t_test_result <- t.test(
  regular_bulb__without_spray_filtered_data$time_in_hours,
  doubled_bulb_without_spray_filtered_data$time_in_hours,
  var.equal = FALSE,  # Założenie homogeniczności wariancji
)

# Wyświetlenie wyników testu
print(t_test_result)

