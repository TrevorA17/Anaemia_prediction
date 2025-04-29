# Load necessary libraries
library(plumber)
library(caret)

# Load the saved GBM model
loaded_gbm_model <- readRDS("./models/saved_gbm_model.rds")

# Define a prediction function for the API
#* @post /predict
#* @param Red_Pixel Numeric value for the Red pixel
#* @param Green_pixel Numeric value for the Green pixel
#* @param Blue_pixel Numeric value for the Blue pixel
#* @param Hb Numeric value for Hemoglobin level
#* @response 200 Returns the prediction (Yes or No for anaemia)
predict_anaemia <- function(Red_Pixel, Green_pixel, Blue_pixel, Hb) {
  # Prepare input data
  new_data <- data.frame(
    Red_Pixel = as.numeric(Red_Pixel),
    Green_pixel = as.numeric(Green_pixel),
    Blue_pixel = as.numeric(Blue_pixel),
    Hb = as.numeric(Hb)
  )
  
  # Make prediction using the loaded GBM model
  prediction <- predict(loaded_gbm_model, newdata = new_data)
  
  # Return the prediction as a response
  return(list(prediction = prediction))
}
