# Wczytywanie danych i bibliotek
library(tidyverse)
library(lubridate)
data <- read.csv("horror_movies.csv")

# Zamiana typu zmiennej release date z character na date
data <- data %>%
  mutate(
    release_date = ifelse(nchar(release_date) > 4, format(dmy(release_date), format = "%Y-%m-%d"), NA)
  )

# Zamaina pozostalych zmiennych na odpowiednie w zaleznosci od konteksu
data <- type_convert(data, col_types = "ccDffdcccccc")

# Parsowanie movie_run_time, w celu operatywnosci ta zmienna
data$movie_run_time <- as.integer(parse_number(data$movie_run_time))
data <- data %>%
  rename(movie_run_time_min = movie_run_time) %>%
  mutate(movie_run_time_min = as.integer(movie_run_time_min))

# Wyciagniecie informacji o walucie z budget
data <- data %>%
  mutate(Currency = str_remove_all(budget, "[0-9]|\\s|,"))

# Wyciagniecie informacji o kraju krecenia filmu z filming_locations
data <- data %>%
  mutate(filming_country = str_split(filming_locations, ",\\s*") %>%
    sapply(function(x) tail(x, 1)))


# Z informacji na temat gatunku stworzenie kolumny dla każdego gatunku z osobna
data <- data %>%
  mutate(genres = str_remove_all(genres, "\\s")) %>% 
  separate_rows(genres, sep = "\\|") %>%
  pivot_wider(names_from = genres, values_from = genres, values_fn = length, values_fill = 0) %>%
  select(-`NA`) %>%
  mutate_at(vars(14:35), ~ ifelse(. > 1, 1, .)) %>%
  mutate(across(14:35, as.logical))

# Z informacji na temat jezyka stworzenie kolumny dla każdego jezyka z osobna
data <- data %>%
  separate_rows(language, sep = "\\|") %>%
  pivot_wider(names_from = language, values_from = language, values_fn = length, values_fill = 0) %>%
  select(-c(`None`, `NA`)) %>%
  mutate(across(English:`Japanese Sign Language`, as.logical))

# Przestawienie filming_country obok filming_locations
data <- data %>%
  relocate(filming_country, .after = filming_locations)

# Stworzenie kolumny director, i przeniesienie jej obok plot
data <- data %>%
  mutate(director = str_extract(plot, ".*?(?= With)")) %>%
  mutate(director = ifelse(is.na(director), plot, director)) %>%
  mutate(director = str_extract(director, "(?<=Directed by ).*")) %>%
  mutate(director = str_remove(director, "\\.$")) %>%
  relocate(director, .after = plot)

# Zapisanie wyczyszczonych danych do csv i rds, do rds po to, żeby wczytać sformatowane wcześcniej dane
write_csv(data, "tidied_data.csv")
write_rds(data, "tidied_data.rds")
