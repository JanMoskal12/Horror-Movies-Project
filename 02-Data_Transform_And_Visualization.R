library(tidyverse)
library(lubridate)
library(ggthemes)
library(gganimate)

data <- read_rds("tidied_data.rds")


# Najpopularniejszy gatunek filmowy ---------------------------------------

most_popular_genre <- data %>%
  select(Drama:`Reality-TV`) %>%
  colSums() %>%
  sort(decreasing = TRUE)

most_popular_genre <- as.data.frame(most_popular_genre)
most_popular_genre <- rownames_to_column(most_popular_genre, var = "Genre")

most_popular_genre %>%
  rename(Count = most_popular_genre) %>%
  ggplot(aes(x = reorder(Genre, Count), y = Count, fill = Genre)) +
  geom_col() +
  coord_flip() +
  labs(title = "Most popular movie genre", x = "Genre", y = "Count") +
  theme(legend.position = "none") +
  scale_color_colorblind()

ggsave("most_popular_genre.jpg")


# Najpopularniejszy dostępny język ----------------------------------------

most_popular_language <- data %>%
  select(36:111) %>%
  colSums() %>%
  sort(decreasing = TRUE)

most_popular_language <- as.data.frame(most_popular_language)
most_popular_language <- rownames_to_column(most_popular_language, var = "Language")

most_popular_language %>%
  rename(Count = most_popular_language) %>%
  filter(Count >= 10) %>%
  ggplot(aes(x = reorder(Language, Count), y = Count, fill = Language)) +
  geom_col() +
  coord_flip() +
  labs(title = "Most popular language", x = "Language", y = "Count") +
  theme(legend.position = "none") +
  scale_color_colorblind()

ggsave("most_popular_language.jpg")


# Średni czas i mediana czasu filmów ---------------------------------

mean(data$movie_run_time_min, na.rm = TRUE)
median(data$movie_run_time_min, na.rm = TRUE)

 data %>% 
  select(title, movie_run_time_min) %>% 
  filter(movie_run_time_min >= 45) %>% 
  group_by(movie_run_time_min) %>% 
  count() %>% 
  ggplot(aes(x = movie_run_time_min, y = n)) +
  geom_line(color = "red") +
  geom_point(color = "darkred") +
  labs(
    title = "Movie run time distribution",
       x = "Movie run time (min)",
       y = "Count"
  ) +
  theme(
    plot.title = element_text(size = 20),
      axis.text = element_text(size = 10)
  ) +
  transition_reveal(movie_run_time_min) +
  enter_fade() +
  exit_fade()

# Najczęstsze kraje wydawania filmów --------------------------------------

data %>%
  group_by(release_country) %>%
  count() %>%
  arrange(desc(n)) %>%
  filter(n >= 30) %>%
  ggplot(aes(x = reorder(release_country, n), y = n, fill = release_country)) +
  geom_col() +
  coord_flip() +
  labs(title = "Most common releasing countries", x = "Release country", y = "Count", fill = "Countries")


# Podział filmów na lata -------------------------------------------------------------------------

 data %>% 
  mutate(release_year = str_extract(release_date, "^.{4}")) %>% 
  mutate(release_year = parse_double(release_year)) %>% 
  group_by(release_year) %>% 
  drop_na(release_year) %>% 
  count() %>% 
  ggplot(aes(x = as.factor(release_year), y = n, fill = as.factor(release_year))) +
  geom_bar(stat = "identity") +
   theme(legend.position = "none",
         plot.title = element_text(size = 15)) +
   labs(title = "Distribution of movies by years",
        x = "Year", 
        y = "Count") +
   scale_fill_brewer(palette = "Set2")
 

# Kraje tworzące najwięcej filmów dla każdego roku-------------------------------------------------------------------------
release_country_count <- data %>% 
   count(release_country, sort = TRUE)
 x <- release_country_count[1:10, 1]
 x <- pull(x)
 x <- as.character(x)
 
 data %>% 
   mutate(release_year = str_extract(release_date, "^.{4}")) %>% 
   mutate(release_year = parse_double(release_year)) %>% 
   select(title, release_year, release_country) %>% 
   group_by(release_year) %>% 
   filter(release_country %in% x) %>% 
   ggplot(aes(x = (release_country), fill = release_country)) +
   geom_bar() +
   coord_flip() +
   transition_states(
     states = release_year,
     transition_length = 4
   ) + 
   labs(title = "Year : {closest_state}")
  