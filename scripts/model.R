
library(tidyverse)
library(sf)
library(caret)

# Load data and get it ready for model
census_geo <- readRDS("data/census_geo_updated.rds")
cat("Loaded census_geo:\n")
print(head(census_geo))

model_data <- census_geo %>%
  mutate(
    Median_Income = scale(Median_Income),
    Tree_Canopy_Pct = scale(Tree_Canopy_Pct),
    Pop_Density = scale(Pop_Density),
    Area_km2 = scale(Area_km2),
    Centroid_X = scale(st_coordinates(st_centroid(geometry))[,1]),
    Centroid_Y = scale(st_coordinates(st_centroid(geometry))[,2])
  ) %>%
  st_drop_geometry() %>%
  select(Mean_Temp_C, Median_Income, Tree_Canopy_Pct, Pop_Density, Area_km2, Centroid_X, Centroid_Y)

# Split data
set.seed(123)
train_idx <- createDataPartition(model_data$Mean_Temp_C, p = 0.8, list = FALSE)
train_data <- model_data[train_idx, ]
test_data <- model_data[-train_idx, ]

# Linear regression
lm_model <- train(
  Mean_Temp_C ~ ., 
  data = train_data, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 5)
)
cat("Linear Regression Summary:\n")
print(summary(lm_model))

# Random Forest
rf_model <- train(
  Mean_Temp_C ~ ., 
  data = train_data, 
  method = "rf",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = expand.grid(mtry = c(2, 3, 4)),
  ntree = 500
)
cat("Random Forest Summary:\n")
print(rf_model)

# Single-feature Random Forest (Median_Income only)
rf_single_model <- train(
  Mean_Temp_C ~ Median_Income, 
  data = train_data, 
  method = "rf",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = expand.grid(mtry = 1),
  ntree = 500
)
cat("Single-feature Random Forest Summary:\n")
print(rf_single_model)

saveRDS(lm_model, "models/lm_model.rds")
saveRDS(rf_model, "models/rf_model.rds")
saveRDS(rf_single_model, "models/rf_single_model.rds")
cat("Models saved to models/\n")

# Predicted vs. actual plot
pred_data <- test_data %>%
  mutate(
    Pred_LM = predict(lm_model, test_data),
    Pred_RF = predict(rf_model, test_data)
  )
pred_plot <- ggplot(pred_data) +
  geom_point(aes(x = Mean_Temp_C, y = Pred_LM, color = "Linear Regression"), alpha = 0.5) +
  geom_point(aes(x = Mean_Temp_C, y = Pred_RF, color = "Random Forest"), alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Linear Regression" = "blue", "Random Forest" = "red")) +
  labs(
    title = "Predicted vs. Actual Temperatures",
    x = "Actual Temperature (°C)",
    y = "Predicted Temperature (°C)",
    color = "Model"
  ) +
  theme_dark() +
  theme(
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black")
  )
cat("Generating predicted vs. actual plot...\n")
png("plots/pred_vs_actual.png", width = 6, height = 4, units = "in", res = 300)
print(pred_plot)
dev.off()

# Feature importance plot
importance <- varImp(rf_model)$importance
importance$Variable <- rownames(importance)
imp_plot <- ggplot(importance, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(
    title = "Feature Importance (Random Forest)",
    x = "Variable",
    y = "Importance"
  ) +
  theme_dark() +
  theme(
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black")
  )
cat("Generating feature importance plot...\n")
png("plots/feature_importance.png", width = 6, height = 4, units = "in", res = 300)
print(imp_plot)
dev.off()

cat("Modeling complete. Plots saved to plots/\n")