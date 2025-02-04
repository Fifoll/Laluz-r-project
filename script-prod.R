file_path <- "https://mimuw.edu.pl/~szym/laluz.csv"

data <- read.csv(file_path, sep = ";")
data <- data[, -5]
data <- na.omit(data)
head(data)
#View(data)


dim(data) # zlicza wiersze i kolumny
colnames(data) # listuje nagłówki kolumn
summary(data) # podsumowanie głównych statystyk kolumn
unique(data$product_type) # unikalne wartości w kolumnie product_type
unique(data$sprayed) # unikalne wartości w kolumnie sprayed
#View(data)

bulb_labels <- c("Żarówka podwójna", "Żarówka pojedyncza")
bulb_color <- c("#63C13270", "#FF000070")


if (!requireNamespace("ggplot2")) {
  install.packages("ggplot2")
}

library(ggplot2) #Wczytanie pakietu ggplot2

ggplot(data, aes(x = time_in_hours, y = price_in_PLN, color = product_type, shape = product_type)) +
  geom_point(size = 4, position = position_jitter(width = 0.3, height = 0.3)) +  # Dodanie scatter
  scale_color_manual(values = bulb_color, name = "Typ żarówki", labels = bulb_labels) +
  scale_shape_manual(values = c("DoubleBulb" = 17, "regular bulb" = 17), name = "Typ żarówki", labels = bulb_labels) +
  labs(
    x = "Czas w godzinach",
    y = "Cena w złotówkach",
    title = "Dane uzyskane w badaniu"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(margin = margin(b = 20), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20)),
  )


z_score_time_in_hours <- scale(data$time_in_hours)
outlier_time_in_hours <- which(abs(z_score_time_in_hours) > 3)

z_score_price_in_pln <- scale(data$price_in_PLN)
outlier_price_in_pln <- which(abs(z_score_price_in_pln) > 3)


outliner_data <- c(outlier_time_in_hours, outlier_price_in_pln)
data <- subset(data, !(1:nrow(data) %in% outliner_data))



# W pierszej kolejności należało wyfiltrować odpowiednio dane na potrzeby testu
# Pierwsza zmienna przechowuje dane żarówek regularnych bez użytego spraya
# Druga zmienna przechowuje dane żarówek double bez użytego spraya
regular_bulb_without_spray_filtered_data <- subset(data, product_type == "regular bulb" & sprayed == "none")
doubled_bulb_without_spray_filtered_data <- subset(data, product_type == "DoubleBulb" & sprayed == "none")

# Następnie wykonaliśmy testy
shapiro_test_regular_bulb <- shapiro.test(regular_bulb__without_spray_filtered_data$time_in_hours)
shapiro_test_doubled_bulb <- shapiro.test(doubled_bulb_without_spray_filtered_data$time_in_hours)
print(shapiro_test_regular_bulb)
print(shapiro_test_doubled_bulb)


mann_whitney_result <- wilcox.test(regular_bulb__without_spray_filtered_data$time_in_hours, 
                                   doubled_bulb_without_spray_filtered_data$time_in_hours)

# Wyświetlanie wyników testu
print(mann_whitney_result)

regular_bulb_without_spray_mean_time <- mean(regular_bulb_without_spray_filtered_data$time_in_hours, na.rm = TRUE) #na.rm = TRUE - usuwa puste wartości
doubled_bulb_without_spray_mean_time <- mean(doubled_bulb_without_spray_filtered_data$time_in_hours, na.rm = TRUE)
print(regular_bulb__without_spray_mean_time)
print(doubled_bulb__without_spray_mean_time)


# Podane dane
#regular_bulb_without_spray_mean_time <- 800  # Zamień na właściwą wartość, jeśli jest inna
#doubled_bulb_without_spray_mean_time <- 1200 # Zamień na właściwą wartość, jeśli jest inna

# Przygotowanie danych do wykresu
# Dane
#bulb_types <- c("Pojedyncza żarówka", "Podwójna żarówka")
mean_times <- c(doubled_bulb_without_spray_mean_time, regular_bulb_without_spray_mean_time)

# Tworzenie wykresu słupkowego
bar_positions <- barplot(
  mean_times,
  names.arg = bulb_labels,
  col = bulb_color,
  main = "Średni czas działania żarówek (bez sprayu)",
  xlab = "Typ żarówki",
  ylab = "Średni czas działania (godziny)",
  xlim = c(0, 5), # Ustawienie zakresu os X
  ylim = c(0, 5000), # Ustawienie zakresu osi Y do 5000
  space = 1,  # Zmniejszenie szerokości słupków (odstęp pomiędzy słupkami)
  cex.names = 0.8
)

# Dodanie punktów dla regular_bulb
points(
  jitter(rep(bar_positions[1], length(doubled_bulb_without_spray_filtered_data$time_in_hour))), # Jitter dla losowego rozrzutu
  doubled_bulb_without_spray_filtered_data$time_in_hour,
  col = "red",
  pch = 16
)

# Dodanie punktów dla doubled_bulb
points(
  jitter(rep(bar_positions[2], length(regular_bulb_without_spray_filtered_data$time_in_hour))), # Jitter dla losowego rozrzutu
  regular_bulb_without_spray_filtered_data$time_in_hour,
  col = "blue",
  pch = 16
)



regular_bulb_with_spray_filtered_data <- subset(data, product_type == "regular bulb" & sprayed == "CoatItYourself")
doubled_bulb_with_spray_filtered_data <- subset(data, product_type == "DoubleBulb" & sprayed == "CoatItYourself")


# Następnie wykonaliśmy testy
shapiro_test_regular_bulb_with_spray <- shapiro.test(regular_bulb_with_spray_filtered_data$time_in_hours)
shapiro_test_doubled_bulb_with_spray <- shapiro.test(doubled_bulb_with_spray_filtered_data$time_in_hours)

print(shapiro_test_regular_bulb_with_spray)
print(shapiro_test_regular_bulb)

print(shapiro_test_doubled_bulb_with_spray)
print(shapiro_test_doubled_bulb)

wilcox_regular_bulb <- wilcox.test(regular_bulb_without_spray_filtered_data$time_in_hours, regular_bulb_with_spray_filtered_data$time_in_hours)
print(wilcox_regular_bulb)

wilcox_doubled_bulb <- wilcox.test(doubled_bulb_without_spray_filtered_data$time_in_hours, doubled_bulb_with_spray_filtered_data$time_in_hours)
print(wilcox_doubled_bulb)


# Cena za jeden psik preparatm

bulbs_with_spray <- subset(data, sprayed == "CoatItYourself")

total_time_spray <- sum(bulbs_with_spray$time_in_hours, na.rm = TRUE)

spray_price <- 100; # cena sprayu

one_shot_spray_price <- spray_price / total_time_spray;

print(one_shot_spray_price)





# Cena za godzinę użytkowania 

# regular bulb without spray
mean_time_regular_bulb_without_spray <- mean(regular_bulb_without_spray_filtered_data$time_in_hours)
mean_price_regular_bulb_without_spray <- mean(regular_bulb_without_spray_filtered_data$price_in_PLN)
price_per_hour_regular_bulb_without_spray <- mean_price_regular_bulb_without_spray / mean_time_regular_bulb_without_spray;
print(price_per_hour_regular_bulb_without_spray)
# ~34,4 gr.

# regular bulb with spray
mean_time_regular_bulb_with_spray <- mean(regular_bulb_with_spray_filtered_data$time_in_hours)
mean_price_regular_bulb_with_spray <- mean(regular_bulb_with_spray_filtered_data$price_in_PLN)
price_per_hour_regular_bulb_with_spray <- mean_price_regular_bulb_with_spray / mean_time_regular_bulb_with_spray + one_shot_spray_price;
print(price_per_hour_regular_bulb_with_spray)
# ~ 31,7gr

# double bulb without spray
mean_time_doubled_bulb_without_spray <- mean(doubled_bulb_without_spray_filtered_data$time_in_hours)
mean_price_doubled_bulb_without_spray <- mean(doubled_bulb_without_spray_filtered_data$price_in_PLN)
price_per_hour_doubled_bulb_without_spray <- mean_price_doubled_bulb_without_spray / mean_time_doubled_bulb_without_spray;
print(price_per_hour_doubled_bulb_without_spray)
# ~47,7gr

# double bulb with spray
mean_time_doubled_bulb_with_spray <- mean(doubled_bulb_with_spray_filtered_data$time_in_hours)
mean_price_doubled_bulb_with_spray <- mean(doubled_bulb_with_spray_filtered_data$price_in_PLN)
price_per_hour_doubled_bulb_with_spray <- mean_price_doubled_bulb_with_spray / mean_time_doubled_bulb_with_spray + one_shot_spray_price;
print(price_per_hour_doubled_bulb_with_spray)
# ~48,1






# Średni czas działania żarówki w latach

light_hours_per_day <- 4;
day_number_in_year <- 365;
# Regularna bez spraya 
mean_time_regular_bulb_without_spray <- mean(regular_bulb_without_spray_filtered_data$time_in_hours);
regular_bulb_without_spray_day_count <- mean_time_regular_bulb_without_spray / light_hours_per_day;
regular_bulb_without_spray_year_count <- regular_bulb_without_spray_day_count / day_number_in_year;
print(regular_bulb_without_spray_year_count)

# Regularna ze sprayem 
mean_time_regular_bulb_with_spray <- mean(regular_bulb_with_spray_filtered_data$time_in_hours);
regular_bulb_with_spray_day_count <- mean_time_regular_bulb_with_spray / light_hours_per_day;
regular_bulb_with_spray_year_count <- regular_bulb_with_spray_day_count / day_number_in_year;
print(regular_bulb_with_spray_year_count)

# double bez spraya 
mean_time_doubled_bulb_without_spray <- mean(doubled_bulb_without_spray_filtered_data$time_in_hours);
doubled_bulb_without_spray_day_count <- mean_time_doubled_bulb_without_spray / light_hours_per_day;
doubled_bulb_without_spray_year_count <- doubled_bulb_without_spray_day_count / day_number_in_year;
print(doubled_bulb_without_spray_year_count)

# double ze sprayem 
mean_time_doubled_bulb_with_spray <- mean(doubled_bulb_with_spray_filtered_data$time_in_hours);
doubled_bulb_with_spray_day_count <- mean_time_doubled_bulb_with_spray / light_hours_per_day;
doubled_bulb_with_spray_year_count <- doubled_bulb_with_spray_day_count / day_number_in_year;
print(doubled_bulb_with_spray_year_count)


# na ile lat starcza spray
bulb_count_in_house <- 10;
bulb_light_per_day <- 4;
spray_count_day <- total_time_spray / bulb_light_per_day / bulb_count_in_house;
spray_count_years <- total_time_spray / bulb_light_per_day / bulb_count_in_house / 365;

print(spray_count_day)
print(spray_count_years)

