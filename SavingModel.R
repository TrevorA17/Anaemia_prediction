# Save the best model (GBM) to a file
saveRDS(model_gbm, "./models/saved_gbm_model.rds")

# Load the saved GBM model (when needed)
loaded_gbm_model <- readRDS("./models/saved_gbm_model.rds")

# Example of new data for prediction (adjust this to match your dataset)
new_data <- data.frame(
  Red_Pixel = 45.0,
  Green_pixel = 28.5,
  Blue_pixel = 26.0,
  Hb = 10.5
)

# Use the loaded GBM model to make predictions
predictions_loaded_model <- predict(loaded_gbm_model, newdata = new_data)

# Print predictions
print(predictions_loaded_model)
