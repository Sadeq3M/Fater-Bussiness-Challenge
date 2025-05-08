df1 <- read.csv("D:/FED/Statistical Learning and Data Analysis/FATER Challenge/gravitation_NA.csv")
summary(df1)
df2 <- read.csv("D:/FED/Statistical Learning and Data Analysis/FATER Challenge/shapes_NA.csv")
summary(df2)
df3 <- read.csv("D:/FED/Statistical Learning and Data Analysis/FATER Challenge/socio_demo_NA.csv")
summary(df3)
#Creat new dataframe 
mdf <- merge(merge(df1, df2, by = "microcode", all = TRUE), df3, by = "microcode", all = TRUE)
mdf$daytype<-as.factor(mdf$daytype)
mdf$fasciaoraria<-as.factor(mdf$fasciaoraria)
mdf$datatype<-as.factor(mdf$datatype)
mdf$district<-as.factor(mdf$district)
mdf$province<-as.factor(mdf$province)
mdf$region<-as.factor(mdf$region)
mdf$microcode<-as.factor(mdf$microcode)
summary(mdf)
write.csv(summary(mdf), "D:/FED/Statistical Learning and Data Analysis/Fater/mdf.csv")
str(mdf)
dim(mdf)

# Check for duplicate rows based on all columns
duplicate_rows <- mdf[duplicated(mdf), ]
print(duplicate_rows)

#Analyze pattern of missing values in mdf
library(VIM)
aggr<- aggr(mdf, col=c('black','red'), numbers=TRUE, sortVars=TRUE,labels=names(mdf), cex.axis=.7, gap=1, ylab=c("Barplot of missing data","Patterns"))

#Impute the missing values with mean
mdf$media_annuale<-replace(mdf$media_annuale, is.na(mdf$media_annuale), mean(mdf$media_annuale, na.rm = TRUE))
summary(mdf)
# Or Impute the missing values with median
#mdf$media_annuale<-replace(mdf$media_annuale, is.na(mdf$media_annuale), median(mdf$media_annuale, na.rm = TRUE))

# Or Impute the missing values with MICE
#library(mice)
#mdf$media_annuale<-complete(mice(mdf, method = "cart"))$media_annuale
#mdf$media_annuale<-complete(mice(mdf, method = "pmm"))$media_annuale

#cmdf will be used for future to creat mdf_cleaned
cmdf<-mdf

# standardized_data
# Identify numeric columns
numeric_cols <- sapply(mdf, is.numeric)
# Standardize numeric columns
mdf[numeric_cols] <- scale(mdf[numeric_cols])
#Descriptive Statistical mdf
mmdf<-mdf[,-c(1,3, 4, 5,7,8,9,10)]
summary(mmdf)
#hist
# Set up a layout
n_cols <- ncol(mmdf)
n_rows <- ceiling(n_cols / 2)
par(mfrow = c(n_rows, 2), mar = c(4, 4, 2, 2))  # Adjust the layout if needed

# Create histograms for all features
for (col in names(mmdf)) {
  hist(mmdf[[col]], col = "lightblue", main = paste("Histogram for", col), xlab = "Values", 
       border = "black")
}

# Reset the layout to 1x1
par(mfrow = c(1, 1))

#boxplot
boxplot(mmdf)

# Outlier detection
# Function to remove outliers using IQR method for each column
remove_outliers_df <- function(df, k = 1.5) {
  df_cleaned <- df
  for (col in names(df)) {
    x <- df[[col]]
    outliers <- x < quantile(x, 0.25) - k*IQR(x) | x > quantile(x, 0.75) + k*IQR(x)
    df_cleaned[[col]][outliers] <- NA
  }
  return(na.omit(df_cleaned))
}

# Apply the function to  dataframe
cleaned_mdf <- remove_outliers_df(mmdf)

# Identify rows with outliers in the main dataframe
outlier_rows <- setdiff(1:nrow(mdf), match(rownames(cleaned_mdf), rownames(mdf)))

# Remove corresponding rows from the main dataframe 
mdf_cleaned <- cmdf[-outlier_rows, ]
#Display
summary(mdf_cleaned)
dim(cleaned_mdf)
dim(mdf_cleaned)
dim(mmdf)
dim(mdf)

# Create a boxplot with red line for each column
par(mfrow = c(2, 7))  # Adjust the layout if needed

for (col in names(mmdf)) {
  boxplot(mmdf[[col]], main = col, col = "lightblue", border = "black", 
          notch = TRUE, outline = TRUE)
  
  # Add a red line for the threshold
  threshold <- c(quantile(mmdf[[col]], 0.25) - 1.5*IQR(mmdf[[col]]),
                 quantile(mmdf[[col]], 0.75) + 1.5*IQR(mmdf[[col]]))
  abline(h = threshold, col = "red", lty = 2)
}

par(mfrow = c(1, 1))

#corrolation Analysis for mdf
cordf1=cor(cleaned_mdf,method = "pearson")
round(cordf1,2)
library(corrplot)
corrplot(cordf1, method="number")
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# Hypothesis testing with the matrix of the p-value of the correlation
p.mat <- cor.mtest(cordf1)
p.mat
corrplot(cordf1, type="upper", method="number", order="hclust", p.mat = p.mat, sig.level = 0.05)

#After processing cleaned_mdf (669146*14) include numeric features without microcode and mdf_cleaned (669146*20) include all features
#hist
# Set up a layout
n_cols <- ncol(cleaned_mdf)
n_rows <- ceiling(n_cols / 2)
par(mfrow = c(n_rows, 2), mar = c(4, 4, 2, 2))  # Adjust the layout if needed

# Create histograms for all features
for (col in names(cleaned_mdf)) {
  hist(cleaned_mdf[[col]], col = "lightblue", main = paste("Histogram for", col), xlab = "Values", 
       border = "black")
}

# Reset the layout to 1x1
par(mfrow = c(1, 1))

#boxplot
boxplot(cleaned_mdf)

#mdf_cleaned
summary(mdf_cleaned)




df4 <- read.csv("D:/FED/Statistical Learning and Data Analysis/FATER Challenge/stores_NA.csv")
summary(df4)
str(df4)
df4$Insegna<-as.factor(df4$Insegna)
df4$TipologiaPdV<-as.factor(df4$TipologiaPdV)
df4$Parking<-as.factor(df4$Parking)
df4$Comune<-as.factor(df4$Comune)
df4$Provincia<-as.factor(df4$Provincia)
summary(df4)
#write.csv(summary(df4), "E:/01Italy/Other Italy/Helma/Father Helma/FATER Challenge/Datasets Business Game/SDF.csv")
#Analyze pattern of missing values in df4
library(VIM)
aggr<- aggr(df4, col=c('black','red'), numbers=TRUE, sortVars=TRUE,labels=names(df4), cex.axis=.7, gap=1, ylab=c("Barplot of missing data","Patterns"))
#Remove Provincia column from data
df4<-df4[,-10]
summary(df4)
#or maybe predict Provincia based on Comune that is not necessary

#cmdf will be used for future to creat df4_cleaned
cdf4<-df4

# standardized_data
# Identify numeric columns
numeric_cols <- sapply(df4, is.numeric)
# Standardize numeric columns
df4[numeric_cols] <- scale(df4[numeric_cols])

#Descriptive Statistical
df44<-df4[,-c(2,3,5,6,9)]
summary(df44)
#hist
# Set up a layout
n_cols <- ncol(df44)
n_rows <- ceiling(n_cols / 2)
par(mfrow = c(n_rows, 2), mar = c(4, 4, 2, 2))  # Adjust the layout if needed

# Create histograms for all features
for (col in names(df44)) {
  hist(df44[[col]], col = "lightblue", main = paste("Histogram for", col), xlab = "Values", 
       border = "black")
}

# Reset the layout to 1x1
par(mfrow = c(1, 1))

#boxplot
boxplot(df44)

# Outlier detection
# Function to remove outliers using IQR method for each column
remove_outliers_df <- function(df, k = 1.5) {
  df_cleaned <- df
  for (col in names(df)) {
    x <- df[[col]]
    outliers <- x < quantile(x, 0.25) - k*IQR(x) | x > quantile(x, 0.75) + k*IQR(x)
    df_cleaned[[col]][outliers] <- NA
  }
  return(na.omit(df_cleaned))
}

# Apply the function to  dataframe
cleaned_df4 <- remove_outliers_df(df44)

# Identify rows with outliers in the main dataframe
outlier_rows <- setdiff(1:nrow(df4), match(rownames(cleaned_df4), rownames(df4)))

# Remove corresponding rows from the main dataframe 
df4_cleaned <- cdf4[-outlier_rows, ]
#Display
summary(df4_cleaned)
dim(cleaned_df4)
dim(df4_cleaned)
dim(df44)
dim(df4)

# Create a boxplot with red line for each column
par(mfrow = c(1, 5))  # Adjust the layout if needed

for (col in names(df44)) {
  boxplot(df44[[col]], main = col, col = "lightblue", border = "black", 
          notch = TRUE, outline = TRUE)
  
  # Add a red line for the threshold
  threshold <- c(quantile(df44[[col]], 0.25) - 1.5*IQR(df44[[col]]),
                 quantile(df44[[col]], 0.75) + 1.5*IQR(df44[[col]]))
  abline(h = threshold, col = "red", lty = 2)
}

par(mfrow = c(1, 1))

#corrolation Analysis
cordf11=cor(cleaned_df4,method = "pearson")
round(cordf11,2)
library(corrplot)
corrplot(cordf11, method="number")
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# Hypothesis testing with the matrix of the p-value of the correlation
p.mat <- cor.mtest(cordf11)
p.mat
corrplot(cordf11, type="upper", method="number", order="hclust", p.mat = p.mat, sig.level = 0.05)

#After processing cleaned_df4 (923*5) include numeric features and df4_cleaned (923*10) include all features
#hist
# Set up a layout
n_cols <- ncol(cleaned_df4)
n_rows <- ceiling(n_cols / 2)
par(mfrow = c(n_rows, 2), mar = c(4, 4, 2, 2))  # Adjust the layout if needed

# Create histograms for all features
for (col in names(cleaned_df4)) {
  hist(cleaned_df4[[col]], col = "lightblue", main = paste("Histogram for", col), xlab = "Values", 
       border = "black")
}

# Reset the layout to 1x1
par(mfrow = c(1, 1))

#boxplot
boxplot(cleaned_df4)




#After runing Fater GSS and FAter Store
#mdf_cleaned
summary(mdf_cleaned)

#extract longitude and latitude from the geometry column
# Load the required package
#library(sf)
# Convert your data frame to an sf object
#mdf_sf <- st_as_sf(mdf_cleaned, wkt = "geometry")
# Extract longitude and latitude
#mdf_cleaned$latitude <- st_coordinates(st_centroid(mdf_sf))[,"Y"]
#mdf_cleaned$longitude <- st_coordinates(st_centroid(mdf_sf))[,"X"]
#summary(mdf_cleaned)


#Groupby based on microcode
dfg1<-mdf_cleaned[,-c(9, 10)]
library(dplyr)
grouped_data <- dfg1 %>% 
  group_by(microcode)
summary <- grouped_data %>% 
  summarise(count_X = sum(!is.na(X)),
            mode_daytype = as.factor(names(sort(-table(daytype)))[1]),
            mode_fasciaoraria = as.factor(names(sort(-table(fasciaoraria)))[1]),
            mode_datatype = as.factor(names(sort(-table(datatype)))[1]),
            mean_media_annuale=round(mean(media_annuale)),
            district = as.factor(names(sort(-table(district)))[1]),
            population=mean(population),
            population_m=mean(population_m),
            population_f=mean(population_f),
            population_age_00_04_yr=mean(population_age_00_04_yr),
            population_age_05_14_yr=mean(population_age_05_14_yr),
            population_age_15_34_yr=mean(population_age_15_34_yr),
            population_age_35_44_yr=mean(population_age_35_44_yr),
            population_age_45_54_yr=mean(population_age_45_54_yr),
            population_age_55_64_yr=mean(population_age_55_64_yr),
            population_age_65_up_yr=mean(population_age_65_up_yr),
  )
dfg1<-as.data.frame(summary)
summary(dfg1)
dim(dfg1)

#df4_cleaned
summary(df4_cleaned)
df<-df4_cleaned[,c(4, 10)]
#Modeling-K-means
library(cluster)
library(factoextra)
library(flexclust)
library(NbClust)
fviz_nbclust (df, kmeans,'sil', k.max = 5)
k1 <- kmeans(df, centers = 3)
k1$centers
k1$size
#Cluster plot
fviz_cluster(k1,df)
# create new data set with assigned number clusters
dc<-data.frame(df4_cleaned,k1$cluster)
#sort based on priority
dc$k1.cluster<-as.factor(dc$k1.cluster)
levels(dc$k1.cluster)<-c(2, 1, 3)

#Merge new data set with the grouped by data set from last part on mdf_cleaned
mer<-merge.data.frame(dc, dfg1 , by.x = "Comune", by.y= "district", all = TRUE)
summary(mer)
cmer<-na.omit(mer)
summary(cmer)
head(cmer)

#Group by based on Cod3HD
library(dplyr)
grouped_data <- cmer %>% 
  group_by(Cod3HD)
summary <- grouped_data %>% 
  summarise(Comune = as.factor(names(sort(-table(Comune)))[1]),
            Insegna = as.factor(names(sort(-table(Insegna)))[1]),
            TipologiaPdV = as.factor(names(sort(-table(TipologiaPdV)))[1]),
            MQVEND =mean(MQVEND),
            Parking = as.factor(names(sort(-table(Parking)))[1]),
            Potenziale = mean(Potenziale),
            cluster_number = as.factor(names(sort(-table(k1.cluster)))[1]),
            mode_microcode = as.factor ((names(sort(-table(microcode)))[1])),
            mode_daytype = as.factor(names(sort(-table(mode_daytype)))[1]),
            mode_fasciaoraria = as.factor(names(sort(-table(mode_fasciaoraria)))[1]),
            mode_datatype = as.factor(names(sort(-table(mode_datatype)))[1]),
            mean_media_annuale = round(mean(mean_media_annuale)),
            mean_population = round(mean(population)),
            mean_population_m = round(mean(population_m)),
            mean_population_f = round(mean(population_f)),
            mean_population_age_00_04_yr = round(mean(population_age_00_04_yr)),
            mean_population_age_05_14_yr = round(mean(population_age_05_14_yr)),
            mean_population_age_15_34_yr = round(mean(population_age_15_34_yr)),
            mean_population_age_35_44_yr = round(mean(population_age_35_44_yr)),
            mean_population_age_45_54_yr = round(mean(population_age_45_54_yr)),
            mean_population_age_55_64_yr = round(mean(population_age_55_64_yr)),
            mean_population_age_65_up_yr = round(mean(population_age_65_up_yr)),
  )
cmerg<-as.data.frame(summary)
summary(cmerg)
dim(cmerg)

# Create three new data sets from each cluster
cmerg1<-cmerg[cmerg$cluster_number==1,]
summary(cmerg1)
cmerg2<-cmerg[cmerg$cluster_number==2,]
summary(cmerg2)
cmerg3<-cmerg[cmerg$cluster_number==3,]
summary(cmerg3)

# Rf cmerg1
rfcmerg1<-cmerg1[,c(5,13,17,18,19,20,21,22,23, 7)]
summary(rfcmerg1)

# Splitting the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(nrow(rfcmerg1), 0.8 * nrow(rfcmerg1))  # 80% for training
train_data <- rfcmerg1[train_indices, ]
test_data <- rfcmerg1[-train_indices, ]

# Load necessary libraries
library(randomForest)
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Define the number of folds for cross-validation
num_folds <- 5

# Define the grid of hyperparameters to search through
ntree_values <- c(500, 1000, 5000)
maxnodes_values <- c(5, 10, 50)

# Initialize variables to store the best model and its performance
best_model <- NULL
best_accuracy <- 0
best_rmse <- Inf
accuracy_results <- data.frame() # Initialize dataframe to store accuracy results
rmse_results <- data.frame() # Initialize dataframe to store RMSE results

# Perform grid search with cross-validation
for (ntree in ntree_values) {
  for (maxnodes in maxnodes_values) {
    # Initialize variable to store total accuracy and RMSE across folds
    total_accuracy <- 0
    total_rmse <- 0
    
    # Perform cross-validation
    for (fold in 1:num_folds) {
      # Split the data into training and validation sets for this fold
      fold_indices <- sample(nrow(train_data))  # Randomly shuffle indices
      fold_size <- floor(nrow(train_data) / num_folds)
      fold_start <- (fold - 1) * fold_size + 1
      fold_end <- fold * fold_size
      
      if (fold == num_folds) {
        fold_end <- nrow(train_data)
      }
      
      validation_indices <- fold_indices[fold_start:fold_end]
      training_indices <- setdiff(fold_indices, validation_indices)
      
      fold_train_data <- train_data[training_indices, ]
      fold_validation_data <- train_data[validation_indices, ]
      
      # Fit the Random Forest model for this fold
      rf_model <- randomForest(mean_media_annuale ~ ., 
                               data = fold_train_data, 
                               ntree = ntree, 
                               maxnodes = maxnodes)
      
      # Make predictions on the validation set
      fold_pred <- predict(rf_model, fold_validation_data)
      
      # Compute accuracy for this fold
      fold_accuracy <- mean(round(fold_pred) == fold_validation_data$mean_media_annuale)
      
      # Compute RMSE for this fold
      fold_rmse <- sqrt(mean((fold_pred - fold_validation_data$mean_media_annuale)^2))
      
      # Add accuracy and RMSE of this fold to the totals
      total_accuracy <- total_accuracy + fold_accuracy
      total_rmse <- total_rmse + fold_rmse
    }
    
    # Compute average accuracy and RMSE across folds for this parameter combination
    average_accuracy <- total_accuracy / num_folds
    average_rmse <- total_rmse / num_folds
    
    # Store the accuracy and RMSE results
    accuracy_results <- rbind(accuracy_results, data.frame(ntree = ntree, maxnodes = maxnodes, accuracy = average_accuracy))
    rmse_results <- rbind(rmse_results, data.frame(ntree = ntree, maxnodes = maxnodes, rmse = average_rmse))
    
    # Check if this model is better than the current best model
    if (average_accuracy > best_accuracy) {
      best_accuracy <- average_accuracy
      best_model <- randomForest(mean_media_annuale ~ ., 
                                 data = train_data, 
                                 ntree = ntree, 
                                 maxnodes = maxnodes)
    }
    if (average_rmse < best_rmse) {
      best_rmse <- average_rmse
    }
  }
}

# Print the best model
print(best_model)
print(paste("Max Nodes:", maxnodes))

# Plot accuracy results
accuracy_plot <- ggplot(accuracy_results, aes(x = factor(ntree), y = accuracy, color = factor(maxnodes))) +
  geom_line() +
  geom_point() +
  labs(title = "Accuracy for Different Hyperparameter Combinations",
       x = "Number of Trees",
       y = "Accuracy",
       color = "Max Nodes") +
  theme_minimal()

# Plot RMSE results
rmse_plot <- ggplot(rmse_results, aes(x = factor(ntree), y = rmse, color = factor(maxnodes))) +
  geom_line() +
  geom_point() +
  labs(title = "RMSE for Different Hyperparameter Combinations",
       x = "Number of Trees",
       y = "RMSE",
       color = "Max Nodes") +
  theme_minimal()

# Print the plots
print(accuracy_plot)
print(rmse_plot)

# Make predictions on the test set using the best model
test_pred <- predict(best_model, test_data)

# Evaluate model RMSE on the test set
test_rmse <- sqrt(mean((test_pred - test_data$mean_media_annuale)^2))
print(paste("Test RMSE:", test_rmse))



# Feature Importance
library(ggplot2)
# Create the dataframe for plotting
importance_df <- data.frame(
  Feature = names(train_data[, -ncol(train_data)]),
  Importance = best_model$importance[, 1]
)
importance_df <- importance_df[order(-importance_df$Importance), ]

# Create the bar plot
p <- ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Feature", y = "Importance") +
  ggtitle("Feature Importance Plot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(p)


# Rf cmerg2
rfcmerg2<-cmerg2[,c(5,13,17,18,19,20,21,22,23, 7)]
summary(rfcmerg2)
# Load necessary libraries
library(randomForest)

# Splitting the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(nrow(rfcmerg2), 0.8 * nrow(rfcmerg2))  # 80% for training
train_data <- rfcmerg2[train_indices, ]
test_data <- rfcmerg2[-train_indices, ]

# Load necessary libraries
library(randomForest)
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Define the number of folds for cross-validation
num_folds <- 5

# Define the grid of hyperparameters to search through
ntree_values <- c(500, 1000, 5000)
maxnodes_values <- c(5, 10, 50)

# Initialize variables to store the best model and its performance
best_model <- NULL
best_accuracy <- 0
best_rmse <- Inf
accuracy_results <- data.frame() # Initialize dataframe to store accuracy results
rmse_results <- data.frame() # Initialize dataframe to store RMSE results

# Perform grid search with cross-validation
for (ntree in ntree_values) {
  for (maxnodes in maxnodes_values) {
    # Initialize variable to store total accuracy and RMSE across folds
    total_accuracy <- 0
    total_rmse <- 0
    
    # Perform cross-validation
    for (fold in 1:num_folds) {
      # Split the data into training and validation sets for this fold
      fold_indices <- sample(nrow(train_data))  # Randomly shuffle indices
      fold_size <- floor(nrow(train_data) / num_folds)
      fold_start <- (fold - 1) * fold_size + 1
      fold_end <- fold * fold_size
      
      if (fold == num_folds) {
        fold_end <- nrow(train_data)
      }
      
      validation_indices <- fold_indices[fold_start:fold_end]
      training_indices <- setdiff(fold_indices, validation_indices)
      
      fold_train_data <- train_data[training_indices, ]
      fold_validation_data <- train_data[validation_indices, ]
      
      # Fit the Random Forest model for this fold
      rf_model <- randomForest(mean_media_annuale ~ ., 
                               data = fold_train_data, 
                               ntree = ntree, 
                               maxnodes = maxnodes)
      
      # Make predictions on the validation set
      fold_pred <- predict(rf_model, fold_validation_data)
      
      # Compute accuracy for this fold
      fold_accuracy <- mean(round(fold_pred) == fold_validation_data$mean_media_annuale)
      
      # Compute RMSE for this fold
      fold_rmse <- sqrt(mean((fold_pred - fold_validation_data$mean_media_annuale)^2))
      
      # Add accuracy and RMSE of this fold to the totals
      total_accuracy <- total_accuracy + fold_accuracy
      total_rmse <- total_rmse + fold_rmse
    }
    
    # Compute average accuracy and RMSE across folds for this parameter combination
    average_accuracy <- total_accuracy / num_folds
    average_rmse <- total_rmse / num_folds
    
    # Store the accuracy and RMSE results
    accuracy_results <- rbind(accuracy_results, data.frame(ntree = ntree, maxnodes = maxnodes, accuracy = average_accuracy))
    rmse_results <- rbind(rmse_results, data.frame(ntree = ntree, maxnodes = maxnodes, rmse = average_rmse))
    
    # Check if this model is better than the current best model
    if (average_accuracy > best_accuracy) {
      best_accuracy <- average_accuracy
      best_model <- randomForest(mean_media_annuale ~ ., 
                                 data = train_data, 
                                 ntree = ntree, 
                                 maxnodes = maxnodes)
    }
    if (average_rmse < best_rmse) {
      best_rmse <- average_rmse
    }
  }
}

# Print the best model
print(best_model)
print(paste("Max Nodes:", maxnodes))

# Plot accuracy results
accuracy_plot <- ggplot(accuracy_results, aes(x = factor(ntree), y = accuracy, color = factor(maxnodes))) +
  geom_line() +
  geom_point() +
  labs(title = "Accuracy for Different Hyperparameter Combinations",
       x = "Number of Trees",
       y = "Accuracy",
       color = "Max Nodes") +
  theme_minimal()

# Plot RMSE results
rmse_plot <- ggplot(rmse_results, aes(x = factor(ntree), y = rmse, color = factor(maxnodes))) +
  geom_line() +
  geom_point() +
  labs(title = "RMSE for Different Hyperparameter Combinations",
       x = "Number of Trees",
       y = "RMSE",
       color = "Max Nodes") +
  theme_minimal()

# Print the plots
print(accuracy_plot)
print(rmse_plot)

# Make predictions on the test set using the best model
test_pred <- predict(best_model, test_data)

# Evaluate model RMSE on the test set
test_rmse <- sqrt(mean((test_pred - test_data$mean_media_annuale)^2))
print(paste("Test RMSE:", test_rmse))



# Feature Importance
library(ggplot2)
# Create the dataframe for plotting
importance_df <- data.frame(
  Feature = names(train_data[, -ncol(train_data)]),
  Importance = best_model$importance[, 1]
)
importance_df <- importance_df[order(-importance_df$Importance), ]

# Create the bar plot
p <- ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Feature", y = "Importance") +
  ggtitle("Feature Importance Plot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(p)

# Rf cmerg3
rfcmerg3<-cmerg3[,c(5,13,17,18,19,20,21,22,23, 7)]
summary(rfcmerg3)
# Load necessary libraries
library(randomForest)

# Splitting the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(nrow(rfcmerg3), 0.8 * nrow(rfcmerg3))  # 80% for training
train_data <- rfcmerg3[train_indices, ]
test_data <- rfcmerg3[-train_indices, ]

# Load libraries
library(randomForest)
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Define the number of folds for cross-validation
num_folds <- 5

# Define the grid of hyperparameters to search through
ntree_values <- c(500, 1000, 5000)
maxnodes_values <- c(5, 10, 50)

# Initialize variables to store the best model and its performance
best_model <- NULL
best_accuracy <- 0
best_rmse <- Inf
accuracy_results <- data.frame() # Initialize dataframe to store accuracy results
rmse_results <- data.frame() # Initialize dataframe to store RMSE results

# Perform grid search with cross-validation
for (ntree in ntree_values) {
  for (maxnodes in maxnodes_values) {
    # Initialize variable to store total accuracy and RMSE across folds
    total_accuracy <- 0
    total_rmse <- 0
    
    # Perform cross-validation
    for (fold in 1:num_folds) {
      # Split the data into training and validation sets for this fold
      fold_indices <- sample(nrow(train_data))  # Randomly shuffle indices
      fold_size <- floor(nrow(train_data) / num_folds)
      fold_start <- (fold - 1) * fold_size + 1
      fold_end <- fold * fold_size
      
      if (fold == num_folds) {
        fold_end <- nrow(train_data)
      }
      
      validation_indices <- fold_indices[fold_start:fold_end]
      training_indices <- setdiff(fold_indices, validation_indices)
      
      fold_train_data <- train_data[training_indices, ]
      fold_validation_data <- train_data[validation_indices, ]
      
      # Fit the Random Forest model for this fold
      rf_model <- randomForest(mean_media_annuale ~ ., 
                               data = fold_train_data, 
                               ntree = ntree, 
                               maxnodes = maxnodes)
      
      # Make predictions on the validation set
      fold_pred <- predict(rf_model, fold_validation_data)
      
      # Compute accuracy for this fold
      fold_accuracy <- mean(round(fold_pred) == fold_validation_data$mean_media_annuale)
      
      # Compute RMSE for this fold
      fold_rmse <- sqrt(mean((fold_pred - fold_validation_data$mean_media_annuale)^2))
      
      # Add accuracy and RMSE of this fold to the totals
      total_accuracy <- total_accuracy + fold_accuracy
      total_rmse <- total_rmse + fold_rmse
    }
    
    # Compute average accuracy and RMSE across folds for this parameter combination
    average_accuracy <- total_accuracy / num_folds
    average_rmse <- total_rmse / num_folds
    
    # Store the accuracy and RMSE results
    accuracy_results <- rbind(accuracy_results, data.frame(ntree = ntree, maxnodes = maxnodes, accuracy = average_accuracy))
    rmse_results <- rbind(rmse_results, data.frame(ntree = ntree, maxnodes = maxnodes, rmse = average_rmse))
    
    # Check if this model is better than the current best model
    if (average_accuracy > best_accuracy) {
      best_accuracy <- average_accuracy
      best_model <- randomForest(mean_media_annuale ~ ., 
                                 data = train_data, 
                                 ntree = ntree, 
                                 maxnodes = maxnodes)
    }
    if (average_rmse < best_rmse) {
      best_rmse <- average_rmse
    }
  }
}

# Print the best model
print(best_model)
print(paste("Max Nodes:", maxnodes))

# Plot accuracy results
accuracy_plot <- ggplot(accuracy_results, aes(x = factor(ntree), y = accuracy, color = factor(maxnodes))) +
  geom_line() +
  geom_point() +
  labs(title = "Accuracy for Different Hyperparameter Combinations",
       x = "Number of Trees",
       y = "Accuracy",
       color = "Max Nodes") +
  theme_minimal()

# Plot RMSE results
rmse_plot <- ggplot(rmse_results, aes(x = factor(ntree), y = rmse, color = factor(maxnodes))) +
  geom_line() +
  geom_point() +
  labs(title = "RMSE for Different Hyperparameter Combinations",
       x = "Number of Trees",
       y = "RMSE",
       color = "Max Nodes") +
  theme_minimal()

# Print the plots
print(accuracy_plot)
print(rmse_plot)

# Make predictions on the test set using the best model
test_pred <- predict(best_model, test_data)

# Evaluate model RMSE on the test set
test_rmse <- sqrt(mean((test_pred - test_data$mean_media_annuale)^2))
print(paste("Test RMSE:", test_rmse))

# Feature Importance
library(ggplot2)
# Create the dataframe for plotting
importance_df <- data.frame(
  Feature = names(train_data[, -ncol(train_data)]),
  Importance = best_model$importance[, 1]
)
importance_df <- importance_df[order(-importance_df$Importance), ]

# Create the bar plot
p <- ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Feature", y = "Importance") +
  ggtitle("Feature Importance Plot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(p)



