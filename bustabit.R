setwd('C:/Users/oscar/Documents/R projects/Crypto')

packs <- c('tidyverse','factoextra','GGally')

for (p in packs) {
  if(!require(p, character.only = T)){
    install.packages(p)
    library(p, character.only = T)
  }
}

bustabit <- read_csv('bustabit.csv')

# Look at the first five rows of the data
head(bustabit, 5)

# Find the highest multiplier (BustedAt value) achieved in a game
bustabit %>%
  arrange(desc(BustedAt)) %>%
  slice(1)

# Create the new feature variables 
bustabit_features <- bustabit %>% 
  mutate(CashedOut = ifelse(is.na(CashedOut), BustedAt + .01, CashedOut),
         Profit = ifelse(is.na(Profit), 0, Profit),
         Losses = ifelse(Profit == 0, -Bet, 0),
         GameWon = ifelse(Profit == 0, 0, 1),
         GameLost = ifelse(Losses==0, 0, 1))

# Look at the first five rows of the features data
head(bustabit_features,5)

# Group by players to create per-player summary statistics
bustabit_clus <- bustabit_features %>%
  group_by(Username) %>%
  summarize(AverageCashedOut = mean(CashedOut), 
            AverageBet = mean(Bet),
            TotalProfit = sum(Profit),
            TotalLosses = sum(Losses), 
            GamesWon = sum(GameWon),
            GamesLost = sum(GameLost))

# View the first five rows of the data
head(bustabit_clus, n = 5)

# Create the mean-sd standardization function
mean_sd_standard <- function(x) {
  (x-mean(x))/sd(x)
}

# Apply the function to each numeric variable in the clustering set
bustabit_standardized <- bustabit_clus %>%
  mutate_if(is.numeric, mean_sd_standard)

# Summarize our standardized data
summary(bustabit_standardized)

# Run the fviz_nbclust function with our selected data and method "wss"
elbow_method <- bustabit_standardized %>% select_if(is.numeric) %>% 
  fviz_nbclust(FUNcluster = kmeans, method='wss')

# View the plot
elbow_method

# Choose 20190101 as our random seed
set.seed(20190101)

# Cluster the players using kmeans with five clusters
cluster_solution <- bustabit_standardized %>% select_if(is.numeric) %>% 
  kmeans(5)

# Store the cluster assignments back into the clustering data frame object
bustabit_clus$cluster <- as.factor(cluster_solution$cluster)

# Look at the distribution of cluster assignments
table(bustabit_clus$cluster)

# Group by the cluster assignment and calculate averages
bustabit_clus_avg <- bustabit_clus %>%
  group_by(cluster) %>%
  summarize_if(is.numeric, mean)

# View the resulting table
bustabit_clus_avg

# Create the min-max scaling function
min_max_standard <- function(x) {
  (x-min(x))/(max(x)-min(x))
}

# Apply this function to each numeric variable in the bustabit_clus_avg object
bustabit_avg_minmax <- bustabit_clus_avg %>%
  mutate_if(is.numeric, min_max_standard)

# Create a parallel coordinate plot of the values
ggparcoord(data=bustabit_avg_minmax, columns = 2:ncol(bustabit_avg_minmax), 
           groupColumn = 'cluster', scale = "globalminmax", order = "skewness")

# Calculate the principal components of the standardized data
my_pc <- as.data.frame(prcomp(bustabit_standardized[,2:ncol(bustabit_standardized)])$x)

# Store the cluster assignments in the new data frame
my_pc$cluster <- bustabit_clus$cluster

# Use ggplot() to plot PC2 vs PC1, and color by the cluster assignment
p1 <- ggplot(my_pc, aes(PC2,PC1, color=cluster)) + geom_point()

# View the resulting plot
p1

# Assign cluster names to clusters 1 through 5 in order
cluster_names <- c(
  "Risky Commoners",
  "High Rollers",
  "Risk Takers",
  "Cautious Commoners",
  "Strategic Addicts"
)

# Append the cluster names to the cluster means table
bustabit_clus_avg_named <- bustabit_clus_avg %>%
  cbind(Name = cluster_names)

# View the cluster means table with your appended cluster names
bustabit_clus_avg_named