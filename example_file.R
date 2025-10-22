# Create a linear regression models relating gas consumption to 
# horsepower and number of cylinders

# Model 1 not using log of horsepower
model_1 <- lm(mpg ~ hp + cyl,
              data = mtcars)

# Model 2 using log of horsepower
model_2 <- lm(mpg ~ log(hp) + cyl,
              data = mtcars)

# Print a model summaries for both models
summary(model_1)
summary(model_2)
