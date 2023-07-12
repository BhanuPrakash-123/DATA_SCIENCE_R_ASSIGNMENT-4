# QUESTION NO 1
library(ggplot2)
data(iris)
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  labs(x = "Sepal Length", y = "Petal Length", color = "Species") +
  ggtitle("Scatterplot of Sepal Length and Petal Length by Species")

# CONCLUSION : 
#  3 species - setosa, versicolor, virginica
#  in general, sepal & petal lengths order - setosa < versicolor < virginica
#  slight overlap b/w versicolor & virginica.
________________________________________________________________________
# QUESTION NO 2
data(txhousing, package = "ggplot2")
str(txhousing)
summary(txhousing)
missing_values <- sum(!complete.cases(txhousing))
cat("No of missing values:", missing_values, "\n")

# Scatterplot of sales by date
ggplot(txhousing, aes(x = date, y = sales)) +
  geom_point() +
  labs(x = "Date", y = "Sales") +
  ggtitle("Sales by Date")

# Scatterplot of median housing price by date
ggplot(txhousing, aes(x = date, y = median)) +
  geom_point() +
  labs(x = "Date", y = "Median Housing Price") +
  ggtitle("Median Housing Price by Date")

# Bar plot of sales by month
txhousing$month <- format(txhousing$date, "%b")
ggplot(txhousing, aes(x = month)) +
  geom_bar() +
  labs(x = "Month", y = "Sales") +
  ggtitle("Sales by Month")

# Boxplot of sales by year
txhousing$year <- format(txhousing$date, "%Y")
ggplot(txhousing, aes(x = year, y = sales)) +
  geom_boxplot() +
  labs(x = "Year", y = "Sales") +
  ggtitle("Sales by Year")
_____________________________________________________________________
# QUESTION NO 3
library(dbplyr)
titanic <- read.csv("titanic.csv")
final_Plot <- ggplot(data = titanic, aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth = 10, alpha = 0.7) +
  facet_grid(Sex ~ .) +
  scale_fill_manual(values = c("blue", "red"), labels = c("Died", "Survived")) +
  labs(x = "Fare", y = "Count", title = "Titanic Passenger Survival by Fare and Sex") +
  theme_bw()

____________________________________________________________________
#Question-4
n <- 50
m <- 1e3
A <- matrix(runif(n * m), nrow = n, ncol = m)

# Calculate column norms
column_norms <- sqrt(colSums(A^2))

# Calculate probabilities
p_vec <- column_norms / sum(column_norms)

# Choose a random column index
chosen <- sample.int(m, size = 1, prob = p_vec)
___________________________________________________________________
#Question-5
autoreg_fast <- function(n, rho) {
  out <- numeric(n)
  out[1] <- 0
  
  for (t in 2:n) {
    error <- rnorm(1)
    out[t] <- rho * out[t-1] + error
  }
  
  return(out)
}

____________________________________________________________________
#Question-6
# R function sel_sums
sel_sums <- function(mat) {
  p <- ncol(mat)
  s <- sample(1:p, size = 1)  # Randomly choose s between 1 and p
  
  # Compute column sums for the first s columns
  column_sums <- colSums(mat[, 1:s])
  
  return(column_sums)
}

