setwd("C:/Users/HP/Desktop/Project_script")
#load Dataset
spotify_songs<-read.csv("songs_normalize.csv")

View(spotify_songs)
dim(spotify_songs)

# Step 1: Check for missing values before
print(colSums(is.na(spotify_songs)))

# Step 2: Replace NA values in 'popularity' column with the median value
spotify_songs$popularity[is.na(spotify_songs$popularity)] <- median(spotify_songs$popularity, na.rm = TRUE)

# Step 3: Check for missing values again
print(colSums(is.na(spotify_songs)))

#Deleting duplicate records
View(spotify_songs)
print(dim(spotify_songs))
spotify_songs <- unique(spotify_songs)
View(spotify_songs)

dim(spotify_songs)
View(spotify_songs)

# Data type conversion from milliseconds to minutes
spotify_songs <- spotify_songs %>% mutate(duration_min = (duration_ms / 1000)/60, year) 
View(spotify_songs)

#Removing irrelevant columns
View(spotify_songs)
View(spotify_songs)
dim(spotify_songs)
spotify_songs<-spotify_songs[,-c(3,9,11,14)]
View(spotify_songs)
dim(spotify_songs)


# Analysis & Visualizations
 ggplot(spotify_songs, aes(x = popularity)) + 
  geom_histogram(bins = 15, fill = "blue", color = "black") + 
  labs(title = "Popularity Distribution by Song", 
       x = "Popularity", y = "Count") + theme_minimal()

# top 10 songs by popularity
spotify_songs %>% 
  select(song, popularity, genre) %>%  # Replace with the correct column name
  group_by(song) %>% 
  arrange(desc(popularity))

#explicit content positively, negatively, or neutral impact on popularity
positively, negatively, or neutral impact on popularity
popularity_explicit_content <- spotify_songs %>%  
  ggplot(aes(x = popularity, fill = explicit)) + 
  geom_histogram(binwidth = 5) +  # Adjust binwidth if necessary
  ggtitle("Popularity Change with Explicit Content") +
  labs(x = "Popularity", y = "Count") + 
  theme_minimal()

# Print the plot to display it
print(popularity_explicit_content)


summary(spotify_songs$popularity)
spotify_songs$popularity

# Create a popularity_rating column
spotify_songs$popularity_rating <- as.factor(
  ifelse(spotify_songs$popularity <= 60, "Low - Under 60",  
         ifelse(spotify_songs$popularity <= 80, "Med - 60-79", 
                "High - 80 above"))
)

# Display the popularity_rating column
print(spotify_songs$popularity_rating)

# Plot bar graph
ggplot(spotify_songs, aes(x = popularity_rating)) + 
  geom_bar() + 
  labs(title = "Popularity of Songs Rated High, Med, Low") + 
  theme_minimal() 

# Plot pie chart
ggplot(spotify_songs, aes(x = "", fill = popularity_rating)) + 
  geom_bar(width = 1) + 
  coord_polar(theta = "y") + 
  labs(title = "Proportion of Songs Rated High, Med, Low") + 
  theme_void()  # Optional: to have a clean look

# 15 Artists with the most releases of the songs from the year 1998–2020

Artist_Popular <- spotify_songs %>% count(artist, sort = TRUE, name = "Count") 
Artist_Fil <- Artist_Popular %>% filter(Count >= 15) 
par(mar = c(12, 5, 4, 2)+ 0.1) 
barplot(Artist_Fil$Count,  
        ylab = "Number of songs", 
        col = "#80C4F5", 
         names.arg= Artist_Fil$artist, 
        width= 0.01, 
        ylim = c(0,20), 
       las = 2) 

# Create a new dataframe with duration in minutes
song_duration <- transmute(spotify_songs, duration_min = (duration_ms / 1000) / 60, year)

# Plot the duration of songs over the years
ggplot(song_duration, aes(x = year, y = duration_min)) + 
  geom_point() + 
  geom_smooth() + 
  labs(title = "Duration of Songs Over Years from 2000-2020", x = "Year", y = "Duration in Minutes") + 
  theme_minimal()

# song's popularity and valency correlate with the duration
ggplot(spotify_songs, aes(x=valence, y=popularity, color=duration_min)) + 
geom_point(size=2) +  
ggtitle("Relationship between song popularity and valency") 

#Statistical summary of valence
summary(spotify_songs$valence)  
min(spotify_songs$valence) 
max(spotify_songs$valence) 
mean(spotify_songs$valence) 
median(spotify_songs$valence) 
sd(spotify_songs$valence) 

#Statistical summary of duration_min
summary(spotify_songs$duration_min) 
min(spotify_songs$duration_min) 
max(spotify_songs$duration_min) 
median(spotify_songs$duration_min) 
mean(spotify_songs$duration_min) 
sd(spotify_songs$duration_min)

#Statistical summary of tempo:
summary(spotify_songs$tempo)
min(spotify_songs$tempo)
max(spotify_songs$tempo)
median(spotify_songs$tempo)
mean(spotify_songs$tempo)
sd(spotify_songs$tempo)

#script for categorization of popularity 
View(spotify_songs$popularity_rating)
f1 <- function() 
{ 
spotify_songs$popularity_rating <- as.factor( 
    ifelse(spotify_songs$popularity<=60, "Low - Under 60",  
            ifelse(spotify_songs$popularity<=80, "Med - 60-79", 
                  ifelse(spotify_songs$popularity>80,'High - 80 above',"High - 80 above")))) 
  print("script executed")} 
f1() 
spotify_songs$popularity_rating

song_duration<- transmute(spotify_songs, duration_min = 
(duration_ms / 1000)/60 , year) 
View(song_duration)