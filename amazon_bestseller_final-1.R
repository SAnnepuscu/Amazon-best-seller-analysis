library(tidyverse)
library(ggplot2)
library(reshape2)
library(corrplot)
library(ggcorrplot)
library(dplyr)
library(caTools)
library(caret)

df <- read.csv('/Users/siva_sai_chandra/public_projects/amazon_bestsellers/bestsellers with categories.csv')


# Data Cleaning/Exploration
head(df)

# feature names
colnames(df) 

# check for na values
sum(is.na(df))

# check for duplicates
sum(duplicated(df))

# summary statistics
summary(df)


# Find various categories
unique(df$Genre)





# EDA

# histogram of user ratings
summary(df$User.Rating)
ggplot(df, aes(x = `User.Rating`)) + 
  geom_histogram(binwidth = 0.1, fill = "lightblue", color = "black") +
  xlab("User Rating") +
  ylab("Count") +
  labs(title = 'Histogram of User Ratings')


# histogram of book prices (what price point do most people buy books)
summary(df$Price)
ggplot(df, aes(x = Price)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  labs(x = "Price", y = "Count", title = "Histogram of Book Prices (Most Popular Price Point)")


# boxplot of user ratings by genre
df %>% group_by(Genre) %>%
  summarize(min_rating = min(`User.Rating`),
            max_rating = max(`User.Rating`),
            median_rating = median(`User.Rating`),
            mean_rating = mean(`User.Rating`),
            sd_rating = sd(`User.Rating`),
            n_books = n())

ggplot(df, aes(x = Genre, y = `User.Rating`, fill = Genre)) + 
  geom_boxplot() +
  xlab("Genre") +
  ylab("User Rating") +
  labs(title = 'Boxplot of User Ratings by Genre')



# aggregate sum of reviews by genre (Genre with the most sales/# of reviews)
df_genre_reviews <- aggregate(df$Reviews, by = list(df$Genre), FUN = sum)
names(df_genre_reviews) <- c("Genre", "Reviews")
# create a pie chart of the aggregate sum of reviews by genre
pie_chart <- ggplot(df_genre_reviews, aes(x="", y=Reviews, fill=Genre)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar(theta="y") +
  labs(title="Aggregate Sum of Reviews by Genre") +
  theme_void()
# add the percent on the pie chart
pie_chart + geom_text(aes(label=paste0(round(Reviews/sum(Reviews)*100),"%")), position=position_stack(vjust=0.5))





# select the top 10 most expensive books and their number of reviews
df <- df[!duplicated(df$Name) | df$Name != "Publication Manual of the American Psychological Association, 6th Edition", ]

top_expensive <- df %>% 
  arrange(desc(Price)) %>% 
  head(10) %>% 
  select(Name, Reviews, Price)

# create a new column with the order you want to use for plotting
top_expensive$plot_order <- 1:nrow(top_expensive)

# create a bar chart of the most expensive books and their number of reviews
ggplot(top_expensive, aes(x = plot_order, y = Reviews, fill = Price)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  scale_x_continuous(breaks = top_expensive$plot_order, labels = top_expensive$Name) +
  coord_flip() +
  labs(title = "Top 10 Most Expensive Books and Number of Reviews", x = "Book Title", y = "Number of Reviews", fill = "Price")
# The most expensive books don't necessarily sell the most copies


# create a correlation matrix for a subset of variables in df
subset_df <- df[, c("User.Rating", "Reviews", "Price", "Year")]
corr_matrix <- cor(subset_df)
# print the correlation matrix
print(corr_matrix)
# create a correlation matrix plot for the numeric columns
corrplot(cor(subset_df), method = "color", type = "upper", tl.col = "black")
# Insight: very little correlation between the features
# our regression show us that user.rating is not correlated with number of reviews



# remove any non-numeric characters and convert to integer
df$Year <- as.integer(format(as.Date(gsub("[^0-9]", "", df$Year), "%Y"), "%Y"))


# filter out rows with non-numeric values in 'Reviews' column
df_numeric <- df[!is.na(df$Reviews) & is.numeric(df$Reviews), ]


# calculate mean number of reviews per year by genre
df_mean <- df %>% 
  group_by(Year, Genre) %>% 
  summarise(mean_reviews = mean(Reviews, na.rm = TRUE))

# create line plot of mean reviews per year, colored by genre
df %>%
  group_by(Genre, Year) %>%
  summarize(mean_reviews = mean(Reviews)) %>%
  group_by(Genre) %>%
  summarize(min_rating = min(mean_reviews),
            max_rating = max(mean_reviews),
            median_rating = median(mean_reviews),
            mean_rating = mean(mean_reviews),
            sd_rating = sd(mean_reviews),
            n_books = n())

ggplot(df_mean, aes(x = Year, y = mean_reviews, color = Genre)) +
  geom_line() +
  labs(x = "Year", y = "Mean Reviews", color = "Genre") +
  ggtitle("Mean Number of Reviews per Year by Genre")
# Fiction generally outperforms nonfiction throughout the years

# Group the data by year and genre
df_grouped <- df %>% group_by(Year, Genre) %>% summarise(mean_reviews = mean(Reviews))

# Pivot the data to create a table with years as rows and genres as columns
df_pivot <- df_grouped %>% pivot_wider(names_from = Genre, values_from = mean_reviews, values_fill = 0)

# Plot the grouped bar plot
df_pivot %>% ggplot(aes(x = Year)) + 
  geom_bar(aes(y = Fiction, fill = "Fiction"), stat = "identity") + 
  geom_bar(aes(y = `Non Fiction`, fill = "Non-Fiction"), stat = "identity") + 
  labs(title = "Average Book Reviews by Year and Genre", x = "Year", y = "Average Reviews") + 
  scale_fill_manual(name = "", values = c("Fiction" = "steelblue", "Non-Fiction" = "skyblue")) + 
  guides(fill = guide_legend(reverse = TRUE))




# Create scatterplot with linear regression line of Reviews vs User Rating
ggplot(df, aes(x = `User.Rating`, y = Reviews)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "User Rating", y = "Number of Reviews") +
  ggtitle("Correlation Between User Rating and Number of Reviews") +
  theme_bw()

# Fit a linear regression model for User.Rating vs Reviews
model = lm(User.Rating ~ Reviews, data = df)
summary(model)

# Regression analysis shows that there is not much correlation between the two

# Group the data by author and calculate the mean user rating for each author (Author with most consistent book sales)
author_reviews <- df %>%
  group_by(Author) %>%
  summarize(mean_reviews = mean(Reviews, na.rm = TRUE)) %>%
  arrange(desc(mean_reviews))

# Select the top 10 authors with the highest mean rating
top_authors <- head(author_reviews, 10)

# Create a bar chart of the top authors by mean rating
ggplot(top_authors, aes(x = reorder(Author, -mean_reviews), y = mean_reviews)) +
  geom_bar(stat = "identity", fill = "#00BFC4") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Top Authors by Average # of Reviews") +
  xlab("Author") + ylab("Average Reviews")



# Group the data by author and calculate the mean user rating and mean price for each author
author_reviews <- df %>%
  group_by(Author) %>%
  summarize(mean_reviews = mean(Reviews, na.rm = TRUE), mean_price = mean(Price, na.rm = TRUE)) %>%
  arrange(desc(mean_reviews))

# Select the top 10 authors with the highest mean rating
top_authors <- head(author_reviews, 10)

# Create a bar chart of the top authors by mean rating and mean price gradient
colfunc <- colorRampPalette(c("grey", "lightblue", "blue"))

ggplot(top_authors, aes(x = reorder(Author, -mean_reviews), y = mean_reviews, fill = mean_price)) +
  geom_bar(stat = "identity") +
  scale_fill_gradientn(colors = colfunc(10), na.value = "grey50") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Top Authors by Average # of Reviews and Mean Price Gradient") +
  xlab("Author") + ylab("Average Reviews")












# NLP Top Words in Titles
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyverse)
library(SnowballC)
library(tm)
#library(wordStem)
library(dplyr)
library(stringr)
library(tm)

# Create a new dataframe with title and reviews columns
df_title <- df %>% select(Name, Reviews)

# Preprocess the book titles by removing stop words and stemming the remaining words
preprocess_text <- function(text) {
  words <- str_split(tolower(text), "\\s+")[[1]]
  words <- words[!words %in% stopwords('english')]
  words <- wordStem(words, language = 'english')
  return(paste(words, collapse = " "))
}

df_title$Title_Processed <- sapply(df_title$Name, preprocess_text)

# Use DocumentTermMatrix to convert the preprocessed text into a bag-of-words representation
corpus <- Corpus(VectorSource(df_title$Title_Processed))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stemDocument)

# Remove empty documents from the corpus
corpus <- corpus[!(corpus == "")]

# create the document-term matrix (filtered by words that occur frequently)
dtm <- DocumentTermMatrix(corpus, control = list(minDocFreq = 0.1))

X <- as.data.frame(as.matrix(dtm))

# Train a linear regression model on the bag-of-words features and the number of reviews
model <- lm(Reviews ~ ., data = cbind(df_title['Reviews'], X))

# Get the coefficients of the trained model and map them back to the vocabulary
coefficients <- data.frame(Coefficient = coef(model)[-1])
rownames(coefficients) <- colnames(X)
coefficients <- coefficients %>%
  mutate(word = rownames(coefficients)) %>%
  arrange(desc(Coefficient)) %>%
  select(word, Coefficient)

# Summarize Coefficients and P-Value of Model for Significance
summary(model)
# all the top words are statistically significant as p<2e-16


# Print the top 10 words or phrases with the highest coefficients
head(coefficients, 10)

# Plot the top 10 words or phrases with the highest coefficients
library(ggplot2)
library(tidyr)
class(coefficients)
top_words <- data.frame(Word = rownames(head(coefficients, 30)), Coefficient = head(coefficients, 30)[,1], row.names = NULL)

top_words <- head(coefficients, 30) %>% rownames_to_column("Word")
#top_words <- top_words %>% gather(key = "Category", value = "Value", -Word)



top_words <- coefficients %>% 
  rownames_to_column("Word") %>% 
  mutate(Sign = ifelse(Coefficient > 0, "Positive", "Negative")) %>% 
  arrange(desc(abs(Coefficient))) %>% 
  head(30)

# Scale the Coefficient column of top_words to be between -1 and 1
top_words$Coefficient_scaled <- scale(top_words$Coefficient, center = TRUE, scale = max(abs(top_words$Coefficient)))

ggplot(top_words, aes(x = reorder(Word, Coefficient_scaled), y = Coefficient_scaled, fill = Sign)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Positive" = "green", "Negative" = "red")) +
  coord_flip() +
  labs(title = "Top 30 Words with the Highest Coefficients", x = "Word or Phrase", y = "Coefficient")













# K-means Analysis

library(tidyverse)
library(ggplot2)
library(scales)
library(gridExtra)

X <- df %>% select(`User.Rating`, Reviews, Price)



# elbow plot to find ideal k-clusters

# Apply K-means clustering for different values of k
wcss <- vector(mode = "numeric", length = 10)
for (i in 1:10) {
  kmeans <- kmeans(X, centers = i, nstart = 25, iter.max = 500, algorithm = "Lloyd")
  wcss[i] <- kmeans$tot.withinss
}

# Plot the WCSS values against the number of clusters (k)
ggplot(data.frame(k = 1:10, wcss = wcss), aes(x = k, y = wcss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  labs(title = "Elbow Plot for K-Means Clustering", x = "Number of Clusters (k)", y = "Within-Cluster Sum of Squares (WCSS)")


# ideal number of clusters is 3 clusters according to the elbow plot using the WCSS (within-cluster sum squares) method




# apply K-means clustering
k <- 3 # number of clusters to create
kmeans <- kmeans(X, centers = k, nstart = 25, iter.max = 500, algorithm = "Lloyd")

# visualize the clustering results
plot_cluster <- ggplot(X, aes(x = `User.Rating`, y = Reviews, color = factor(kmeans$cluster))) +
  geom_point() +
  labs(title = "K-Means Clustering Results", x = "User Rating", y = "Number of Reviews") +
  scale_color_discrete(name = "Cluster")
plot_cluster


# create a function to format mean values
format_mean <- function(x) {
  if (is.numeric(x)) {
    comma_format()(round(x, 2))
  } else {
    x
  }
}

# create a dataframe with the cluster centers and their counts
centers_df <- data.frame(t(kmeans$centers))
names(centers_df) <- c("Mean User Rating", "Mean Reviews", "Mean Price")
centers_df$Count <- as.vector(table(kmeans$cluster))

# display the cluster centers dataframe
centers_df

# filter the dataframe to include only books in cluster 1
cluster_1 <- df[kmeans$cluster == 1, ]

# extract the titles, authors, and genres of the books in cluster 1
titles <- cluster_1$Name
authors <- cluster_1$Author
genres <- cluster_1$Genre

# predict the cluster labels for each book
labels <- kmeans$cluster

# filter the dataframe to include only books from cluster 1
cluster_1 <- df[labels == 1, c("Name", "Author", "Genre")]
cluster_2 <- df[labels == 2, c("Name", "Author", "Genre")]
cluster_3 <- df[labels == 3, c("Name", "Author", "Genre")]

# visualize the genre distribution of books in each cluster
plot1 <- cluster_1 %>% 
  count(Genre) %>% 
  ggplot(aes(x = Genre, y = n)) +
  geom_col(fill = "skyblue", color = "black") +
  labs(title = "Genre Distribution of Books in Cluster 1", x = "Genre", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

plot2 <- cluster_2 %>% 
  count(Genre) %>% 
  ggplot(aes(x = Genre, y = n)) +
  geom_col(fill = "skyblue", color = "black") +
  labs(title = "Genre Distribution of Books in Cluster 2", x = "Genre", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

plot3 <- cluster_3 %>% 
  count(Genre) %>% 
  ggplot(aes(x = Genre, y = n)) +
  geom_col(fill = "skyblue", color = "black") +
  labs(title = "Genre Distribution of Books in Cluster 3", x = "Genre", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.05, hjust = 1))

# arrange plots in grid

grid.arrange(plot1, plot2, plot3, ncol = 3)
cluster_1                                   
cluster_2
cluster_3

# summary of clusters
kmeans$centers
table(kmeans$cluster)


# create grid for cluster plots
par(mfrow = c(1,3))
# Create a bar plot for User Rating
barplot(kmeans$centers[, 'User.Rating'], main = 'K-Means Centers for User Rating', 
        xlab = 'Cluster', ylab = 'User Rating', ylim = c(0, 5), col = '#F8766D')

# Create a bar plot for Reviews
barplot(kmeans$centers[, 'Reviews'], main = 'K-Means Centers for Reviews', 
        xlab = 'Cluster', ylab = 'Reviews', ylim = c(0, 61000), col = '#00BA38')


# Create a bar plot for Price
barplot(kmeans$centers[, 'Price'], main = 'K-Means Centers for Price', 
        xlab = 'Cluster', ylab = 'Price', ylim = c(0, 20), col = '#619CFF')

#Based on this finding, we can tell the publisher that high user ratings do not necessarily translate 
#to high sales, and that it may be more important to target books that have a broader appeal and are 
#more likely to receive a large number of reviews and sales, even if they do not have the highest user
#ratings. However, it may also be worth considering ways to increase the visibility and marketing of 
#books in the high rating cluster to boost their sales potential.







# GLM Variable Imporance


# preprocess the data
df$Fiction <- ifelse(df$Genre == "Fiction", 1, 0)

# define bestseller as rating of 4.5 or higher and at least 10,000 reviews
df$Bestseller <- ifelse(df$User.Rating >= 4.5 & df$Reviews >= 10000, 1, 0)

# split the data into training and testing sets
set.seed(123)
split <- sample.split(df$Bestseller, SplitRatio = 0.7)
train <- subset(df, split == TRUE)
test <- subset(df, split == FALSE)

# train a logistic regression model
model <- glm(Bestseller ~ User.Rating + Reviews + Price + Year + Fiction, data = train, family = "binomial")

# make predictions on the test set
predictions <- predict(model, newdata = test, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# evaluate the performance of the model
confusionMatrix(table(predicted_classes, test$Bestseller))
# accuracy score of 0.9018 and p-value of 3.802e-14 which is statistically significant

# plot variable importance
importance_df <- data.frame(varImp(model))
ggplot(importance_df, aes(x = reorder(rownames(importance_df), -Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  coord_flip() +
  labs(title = "Variable Importance for Bestseller Prediction Model", x = "", y = "Overall Importance")



# Extra visualizations for glm


# combine bestseller and non-bestseller data
model$Bestseller <- factor(model$Bestseller)
combined_data <- rbind(train, test)


# create scatter plots for user ratings, reviews, and genre
p3 <- ggplot(model, aes(x = User.Rating, y = Fiction, color = Bestseller)) + 
  geom_point() + 
  labs(x = "User Rating", y = "Fiction") +
  ggtitle("Relationship Between User Rating and Price for Bestsellers vs Non-Bestsellers")

p4 <- ggplot(model, aes(x = Reviews, y = Fiction, color = Bestseller)) + 
  geom_point() + 
  labs(x = "Reviews", y = "Fiction") +
  ggtitle("Relationship Between Reviews and Price for Bestsellers vs Non-Bestsellers")

# display the plots
gridExtra::grid.arrange(p3, p4, ncol = 2)



