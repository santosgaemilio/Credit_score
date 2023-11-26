data_cluster <- select(df, Income, Age, Credit.Score, Number.of.Existing.Loans,
                       Loan.Amount)
data_cluster <- na.omit(data_cluster)
data_cluster_scaled <- scale(data_cluster)

set.seed(123) 
kmeans_result <- kmeans(data_cluster_scaled, centers = 4, nstart = 25)

cluster_summary <- df %>%
  group_by(Cluster) %>%
  summarise(
    Income_Mean = mean(Income, na.rm = TRUE),
    Age_Mean = mean(Age, na.rm = TRUE),
    Credit_Score_Mean = mean(Credit.Score, na.rm = TRUE),
    Existing_Loans_Count = mean(Number.of.Existing.Loans, na.rm = TRUE),
    Loan_Amount_Mean = mean(Loan.Amount, na.rm = TRUE),
    Loan_Tenure_Mean = mean(Loan.Tenure, na.rm = TRUE),
    LTV_Ratio_Mean = mean(LTV.Ratio, na.rm = TRUE),
    Count = n()
  )

# ----------------------------------------------------

x <- seq_along(y_test)

# Subset the data for the range [100:150]
subset_start <- 100
subset_end <- 150

# Creating a data frame to hold the subsetted data
data_subset <- data.frame(
  x = x[subset_start:subset_end],
  predictions = predictions[subset_start:subset_end],
  y_test = y_test[subset_start:subset_end]
)

# Create the plot
ggplot(data_subset, aes(x = x)) +
  geom_line(aes(y = predictions), color = "blue", linetype = "solid", shape = 1) + # Prediccion
  geom_line(aes(y = y_test), color = "red", linetype = "dashed", shape = 4) + # Real
  labs(x = "Prediccion", y = "Real", title = "Comparativa del desempeño de modelo en 50 datos") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(subset_start, subset_end, by = 5)) +
  scale_y_continuous() +
  theme(legend.position = "top") +
  scale_shape_manual(values = c(1, 4)) +
  geom_point(aes(y = predictions), color = "blue", shape = 1) +
  geom_point(aes(y = y_test), color = "red", shape = 4) +
  labs(color = "Legend Title") +
  guides(color = guide_legend(override.aes = list(shape = c(1, 4), linetype = c("solid", "dashed"))))


