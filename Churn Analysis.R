library(tidyverse)
library(cowplot)
library(lattice)
library(caret)
library(rpart)
library(gplots)
library(ROCR)
library(rpart)

theme <- theme(
  axis.text.y = element_blank(), axis.ticks.y = element_blank(),
  legend.position="none" 
)
data <- read.csv(file.choose())
data <- data %>%
  mutate(
    # column was int but best to have it as logical
    SeniorCitizen = as.logical(SeniorCitizen)
  )
str(data)
data %>%
  summarise_all(
    funs(sum(is.na(.)))
  ) %>%
  gather(ColumnTitle, NAs, customerID:Churn)
data %>%
  select(
    customerID, tenure, TotalCharges
  ) %>%
  filter(
    is.na(TotalCharges)
  )
options(repr.plot.width = 4, repr.plot.height = 3)

data %>%
  group_by(Churn) %>%
  summarize(
    n = n()
  ) %>%
  mutate(
    percentage = round(n / sum(n), 3),
    n = NULL
  ) %>%
  ggplot(aes(x = Churn, y = percentage)) + geom_col(aes(fill = Churn)) +
  theme +
  geom_text(
    aes(x = Churn, y = percentage, label = paste(percentage*100, "%", sep = ""))
  )

options(repr.plot.width = 4, repr.plot.height = 4)

# Function to generate graphs for factor variables and churn

## Extract columns to be analyzed
function_columns <- data %>%
  select(
    "gender", "SeniorCitizen", "Partner", "Dependents", "PhoneService", "MultipleLines", 
    "InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport",
    "StreamingTV", "StreamingMovies", "Contract", "PaperlessBilling", "PaymentMethod", "Churn"
  )

## Function, goes through each column selected
for (i in 1:ncol(function_columns))
{
  # Get column names so dplyr group by works
  cname <- colnames(function_columns[c(i,17)])
  # Subset data frame by variable name selected
  a <- subset(
    function_columns, !is.na(function_columns[,i]) & function_columns[,i] != "",
    select = cname
  ) %>%
    # Create percentage statistics per variable
    group_by_at(vars(cname)) %>%
    summarize(
      n = n()
    ) %>%
    mutate(
      Percentage = round(n / sum(n), 2)
    )
  
  # Save plot in a variable so plots can be displayed sequentialy
  p <- ggplot(
    data = a, aes_string(
      x = colnames(a[1]), y = colnames(a[4]), fill = colnames(a[1])
    )
  ) +
    facet_wrap("Churn") + 
    geom_bar(stat = "identity") +
    # Make graph a bit cleaner
    theme(
      axis.text.y = element_blank(), axis.ticks.y = element_blank(),
      axis.text.x = element_text(angle = 70, hjust = 1),
      legend.position="none"
    ) +
    geom_text(
      aes(y = Percentage, label = paste0(Percentage * 100,"%"))
    ) +
    labs(
      x = colnames(a[1]), y = "Churn", title = paste("Churn and", colnames(a[1]))
    )
  
  # Display graphs
  print(p)
  # Cleanup
  rm(cname, a, p)
}
options(repr.plot.width = 7, repr.plot.height = 3)
plot_grid(
  data %>%
    filter(Churn == "Yes") %>%
    group_by(tenure) %>%
    summarize(
      n = n()
    ) %>%
    mutate(
      Percentage = round(n / sum(n), 3)
    ) %>%
    # Create plot
    ggplot(
      aes(x = tenure, y = Percentage, color = tenure)
    ) +
    stat_smooth(method = "lm", col = "red") +
    geom_point(alpha = 2/3) +
    # Clean graph visual a bit
    theme +
    labs(
      x = "Tenure", y = "Churn (%)"
    ),
  
  ggplot(
    data = data,
    aes(y = tenure, x = Churn, color = Churn)
  ) +
    theme +
    geom_boxplot()
  , align = "h")
options(repr.plot.width = 7, repr.plot.height = 3)
plot_grid(
  data %>%
    filter(Churn == "Yes") %>%
    group_by(MonthlyCharges) %>%
    summarize(
      n = n()
    ) %>%
    mutate(
      Percentage = round(n / sum(n), 3)
    ) %>%
    # Create plot
    ggplot(
      aes(x = MonthlyCharges, y = Percentage, color = MonthlyCharges)
    ) +
    stat_smooth(method = "lm", col = "red") +
    geom_point(alpha = 2/3) +
    # Clean graph visual a bit
    theme +
    labs(
      x = "Monthly Charges", y = "Churn (%)"
    ),
  
  ggplot(
    data = data,
    aes(y = MonthlyCharges, x = Churn, color = Churn)
  ) +
    theme +
    geom_boxplot()
  , align = "h")
options(repr.plot.width = 7, repr.plot.height = 3)
plot_grid(
  
  data %>%
    filter(Churn == "Yes") %>%
    group_by(TotalCharges) %>%
    summarize(
      n = n()
    ) %>%
    mutate(
      Percentage = round(n / sum(n), 3)
    ) %>%
    # Create plot
    ggplot(
      aes(x = TotalCharges, y = Percentage, color = TotalCharges)
    ) +
    stat_smooth(method = "lm", col = "red") +
    geom_point(alpha = 2/3) +
    # Clean graph visual a bit
    theme +
    labs(
      x = "Total Charges", y = "Churn (%)"
    ),
  
  data %>%
    filter(is.na(TotalCharges) == FALSE) %>%
    ggplot(
      aes(y = TotalCharges, x = Churn, color = Churn)
    ) +
    theme +
    geom_boxplot()
  , align = "h")
data.model <- data %>%
  select(
    -customerID, -gender,-PhoneService, -MultipleLines, -MonthlyCharges, -TotalCharges 
  )
set.seed(123)

# Split data, 75% distribution of churn for training
train.index <- createDataPartition(
  y = data.model$Churn, p = 0.75, list = FALSE
)

train <- data.model[train.index,]
test <- data.model[-train.index,]
options(repr.plot.width = 6, repr.plot.height = 4)

# Fit model
tree.fit <- rpart(
  Churn ~ ., 
  data = train, 
  method = "class"
)

# Graph of tree
rpart(
  tree.fit,
  type = 4,
  extra = 2,
  under = TRUE,
  fallen.leaves = F
)
tree.pred <- predict(
  tree.fit,
  test, 
  type = "class"
)

confusionMatrix(
  tree.pred,test$Churn
)

# Prediction, probability
tree.pred.prob <- predict(
  tree.fit, 
  test, 
  type = "prob"
)
tree.pred.prob.val <- prediction(
  tree.pred.prob[,2],
  test$Churn
)
tree.pred.prob.perf <- performance(
  tree.pred.prob.val,
  "auc"
)

# print AUC value
paste(
  "AUC Value is:", 
  as.numeric(performance(
    tree.pred.prob.val, 
    "auc"
  )@y.values
  )
)
# plots the ROC curve with colors where the splits are.
plot(performance(tree.pred.prob.val, "tpr", "fpr"), colorize = TRUE)
rf.fit  <- train(Churn ~ . , method = "rf", data = train, importance = TRUE)

rf.pred <- predict(rf.fit, test)
