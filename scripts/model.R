library(tidyverse)
library(sf)
library(caret)

# Load preprocessed data
census_geo <- readRDS("data/census_geo.rds")
cat("Loaded census_geo:\n")
print(head(census_geo))

# Feature engineering: Add tract area and centroid coordinates
census_geo <- census_geo %>%
  mutate(
    Area_km2 = as.numeric(st_area(geometry) / 1e6),  # Convert to km²
    Centroid_X = st_coordinates(st_centroid(geometry))[,1],
    Centroid_Y = st_coordinates(st_centroid(geometry))[,2]
  )
cat("Added features: Area_km2, Centroid_X, Centroid_Y\n")
print(head(census_geo))

# Prepare data for modeling
model_data <- census_geo %>%
  select(Mean_Temp_C, Median_Income, Area_km2, Centroid_X, Centroid_Y) %>%
  st_drop_geometry()  # Remove geometry for modeling
cat("Model data summary:\n")
print(summary(model_data))

# Scale features for Random Forest to balance importance
model_data_scaled <- model_data %>%
  mutate(
    Median_Income = scale(Median_Income),
    Area_km2 = scale(Area_km2),
    Centroid_X = scale(Centroid_X),
    Centroid_Y = scale(Centroid_Y)
  )
cat("Scaled model data summary:\n")
print(summary(model_data_scaled))

# Split data: 80% train, 20% test
set.seed(123)
trainIndex <- createDataPartition(model_data$Mean_Temp_C, p = 0.8, list = FALSE)
train_data <- model_data_scaled[trainIndex, ]
test_data <- model_data_scaled[-trainIndex, ]
cat("Training rows:", nrow(train_data), "\n")
cat("Test rows:", nrow(test_data), "\n")

# Linear Regression
lm_control <- trainControl(method = "cv", number = 5)
lm_model <- train(
  Mean_Temp_C ~ Median_Income + Area_km2 + Centroid_X + Centroid_Y,
  data = train_data,
  method = "lm",
  trControl = lm_control
)
cat("Linear regression results:\n")
print(summary(lm_model))

# Random Forest (full model)
rf_control <- trainControl(method = "cv", number = 5)
rf_model <- train(
  Mean_Temp_C ~ Median_Income + Area_km2 + Centroid_X + Centroid_Y,
  data = train_data,
  method = "rf",
  trControl = rf_control,
  ntree = 100
)
cat("Random Forest results:\n")
print(rf_model)

# Random Forest (Median_Income only)
rf_single <- train(
  Mean_Temp_C ~ Median_Income,
  data = train_data,
  method = "rf",
  trControl = rf_control,
  ntree = 100,
  tuneGrid = data.frame(mtry = 1)  # Fix mtry for single predictor
)
cat("Random Forest (Median_Income only) results:\n")
print(rf_single)

# Predictions and evaluation
lm_pred <- predict(lm_model, test_data)
rf_pred <- predict(rf_model, test_data)
rf_single_pred <- predict(rf_single, test_data)
lm_rmse <- sqrt(mean((lm_pred - test_data$Mean_Temp_C)^2))
rf_rmse <- sqrt(mean((rf_pred - test_data$Mean_Temp_C)^2))
rf_single_rmse <- sqrt(mean((rf_single_pred - test_data$Mean_Temp_C)^2))
lm_r2 <- cor(lm_pred, test_data$Mean_Temp_C)^2
rf_r2 <- cor(rf_pred, test_data$Mean_Temp_C)^2
rf_single_r2 <- cor(rf_single_pred, test_data$Mean_Temp_C)^2
cat("Linear Regression RMSE:", lm_rmse, "\n")
cat("Random Forest RMSE:", rf_rmse, "\n")
cat("Random Forest (Median_Income only) RMSE:", rf_single_rmse, "\n")
cat("Linear Regression R²:", lm_r2, "\n")
cat("Random Forest R²:", rf_r2, "\n")
cat("Random Forest (Median_Income only) R²:", rf_single_r2, "\n")

# Feature importance (full model)
importance <- varImp(rf_model)
cat("Feature importance (full model):\n")
print(importance)

# Save models
saveRDS(lm_model, "models/lm_model.rds")
saveRDS(rf_model, "models/rf_model.rds")
saveRDS(rf_single, "models/rf_single_model.rds")
cat("Models saved to models/\n")

# Visualization: Predicted vs. Actual
pred_data <- data.frame(
  Actual = test_data$Mean_Temp_C,
  LM_Pred = lm_pred,
  RF_Pred = rf_pred,
  RF_Single_Pred = rf_single_pred
)
pred_plot <- ggplot(pred_data, aes(x = Actual)) +
  geom_point(aes(y = LM_Pred, color = "Linear Regression"), alpha = 0.5) +
  geom_point(aes(y = RF_Pred, color = "Random Forest"), alpha = 0.5) +
  geom_point(aes(y = RF_Single_Pred, color = "Random Forest (Median_Income only)"), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Predicted vs. Actual Temperatures",
    x = "Actual Temperature (°C)",
    y = "Predicted Temperature (°C)",
    color = "Model"
  ) +
  scale_color_manual(values = c("Linear Regression" = "blue", "Random Forest" = "red", "Random Forest (Median_Income only)" = "green")) +
  theme_dark() +
  theme(
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    legend.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black")
  )
cat("Generating predicted vs. actual plot...\n")
png("plots/pred_vs_actual.png", width = 8, height = 6, units = "in", res = 300)
print(pred_plot)
dev.off()

# Visualization: Feature importance (full model)
imp_data <- data.frame(
  Feature = rownames(importance$importance),
  Importance = importance$importance$Overall
)
imp_plot <- ggplot(imp_data, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Feature Importance (Random Forest)", x = "Feature", y = "Importance") +
  theme_dark() +
  theme(
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black")
  )
cat("Generating feature importance plot...\n")
png("plots/feature_importance.png", width = 8, height = 6, units = "in", res = 300)
print(imp_plot)
dev.off()

cat("Modeling complete. Models and plots saved.\n")