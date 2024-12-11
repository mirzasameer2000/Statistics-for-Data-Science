data <- read.csv("C:/Users/Baig/Downloads/high_fertility_dataset.csv")

# Step 3: Convert High_Fertility to a factor (if not already)
data$High_Fertility <- as.factor(data$High_Fertility)

set.seed(42)
library(caTools)
split <- sample.split(data$High_Fertility, SplitRatio = 0.7)
train <- subset(data, split == TRUE)

test <- subset(data, split == FALSE)


# Step 5: Fit the logistic regression model
log_model <- glm(High_Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality, 
                 data = train, family = "binomial")

summary(log_model)


test$predicted <- predict(log_model, newdata = test, type = "response")
test$predicted_class <- ifelse(test$predicted > 0.5, 1, 0)

library(caret)
confusionMatrix(as.factor(test$predicted_class), test$High_Fertility)

