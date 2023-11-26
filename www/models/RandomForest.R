df_filtered <- data.frame(df)
df_filtered <- df_filtered %>%
       mutate(Profile.Score = Profile.Score / 100)
df_filtered <- df_filtered %>%
       mutate(LTV.Ratio = LTV.Ratio / 100)
df_filtered <- na.omit(df_filtered)

library(randomForest)

numericas <- c('Income', 'Credit.Score', 'Credit.History.Length', 'Loan.Amount',
               'Loan.Tenure', 'LTV.Ratio', 'Age', 'Number.of.Existing.Loans')
categoricas <- c('Employment.Profile', 'Occupation', 'Gender', 
                 'Existing.Customer')

df_filtered[categoricas] <- lapply(df_filtered[categoricas], function(x) as.factor(x))
df_filtered[numericas] <- scale(df_filtered[numericas])

predictors <- c(numericas, categoricas)
target <- "Profile.Score"
set.seed(123)

train_indices <- createDataPartition(df_filtered[[target]], p = 0.65, list = FALSE)
train_data <- df_filtered[train_indices, ]
test_data <- df_filtered[-train_indices, ]

modelRF <- randomForest(train_data[, predictors], train_data[[target]], 
                        ntree = 15, seed = 100)

predictions <- predict(modelRF, test_data[, predictors])
mse <- mean((predictions - test_data[, target])^2)
# 0.002515296

realvalues <- test_data[, target]
mean_real <- mean(realvalues)
total_variance <- sum((realvalues - mean_real)^2)
residual_variance <- sum((realvalues - predictions)^2)
r_squared <- 1 - (residual_variance / total_variance)
# 0.9560031