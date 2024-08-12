# Load the required libraries

library(gridExtra)
library(ggplot2)
library(patchwork)
library(tidyr)
library(reshape2)
library(dplyr)

# Read the data
data <- read.csv("spotify_songs.csv")

# ================ EXPLORATORY DATA ANLYSIS ===================





#----------- PLOT BW ALL FACTORS VS. TRACK_POPULARITY ----------- 

# function to creating scatter plots
create_scatter_plot <- function(data, x_var) {
  ggplot(data, aes_string(x = x_var, y = "track_popularity")) +
    geom_point(alpha = 0.1) +
    labs(x = x_var, y = "Popularity") +
    theme_minimal()
}
# List of all the x-variables
x_vars <- c("danceability", "energy", "loudness", "speechiness", "acousticness",
            "instrumentalness", "valence", "tempo", "duration_ms")
# Creates a list of plots
plots <- lapply(x_vars, create_scatter_plot, data = data)
# Arranges plots in a grid
plot_grid <- wrap_plots(plots, ncol = 3)
plot_grid





# ------------ JITTER PLOT FOR IDENTIFYING CHAR. OF POULAR SONGS ----------------


feature_names <- names(data)[c(12,13,15,17:23)]

songs <- data %>% 
  arrange(desc(track_popularity)) %>%
  head(n = 500) %>%
  pivot_longer(cols = feature_names) 

songs %>%
  ggplot(aes(x = name, y = value)) +
  geom_jitter(aes(color = playlist_genre)) +
  facet_wrap(~name, ncol = 3, scales = 'free') +
  labs(title = 'Audio Feature Pattern Frequency Plots', x = '', y = '') +
  theme(axis.text.y = element_blank())






# ---------------- COORELATION PLOT TO IDENTIFY RELNSHIP BW FACTORS ---------------- 

# Calculating the correlation matrix
cor_matrix <- cor(data[, c("danceability", "energy", "loudness","key", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "duration_ms","tempo", "track_popularity")])
# Converting the correlation matrix to a tidy format
cor_matrix_tidy <- melt(cor_matrix)
# Creating the correlation plot
ggplot(cor_matrix_tidy, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  labs(x = "", y = "", title = "Correlation Plot") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name = "Correlation")



# ---------------- BOX PLOT BW GENRE & POPULARITY  ---------------- 

ggplot(data, aes(x = playlist_genre, y = track_popularity, fill = playlist_genre)) +
  geom_boxplot() +
  xlab("Genre") +
  ylab("Popularity") +
  ggtitle("Song Popularity by Genre") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 75, by = 5), limits = c(0, 75))  # Set limits from 0 to 75




#  -----------  PLOT FOR NO. OF SONGS IN EVERY GENRE -----------
data %>%
  count(playlist_genre) %>%
  ggplot() +
  geom_col(aes(x = playlist_genre, y = n, fill = playlist_genre)) +
  coord_polar() +
  theme(axis.text.x = element_text(hjust = 1), axis.text.y = element_text(hjust = 1)) +
  ggtitle("Number of songs in every genre") +
  xlab("Song Genre") +
  ylab("Number of songs")











