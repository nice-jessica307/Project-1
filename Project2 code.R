library(tidyverse)
library(caret)
library(Metrics)
library(glmnet)

library(gt)
library(gtsummary)


####Data Loading and EDA######
data <- read.csv("project2.csv")

data <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))


data <- data %>%
  mutate(across(where(is.factor), as.character)) %>%
  mutate(across(where(is.character), ~factor(.))) %>%
  model.matrix(~ . - 1, data = .) %>% 
  as.data.frame()


my_data <- my_data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.character), as.factor))


library(ggplot2)

ggplot(data, aes(x = age_ps, fill = BA)) +
  geom_histogram(bins = 30, alpha = 0.6) +
  labs(title = "Age Distribution by Treatment Group",
       x = "Age",
       y = "Frequency") +
  theme_minimal()

ggplot(data, aes(x = ftcd_score, fill = Var)) +
  geom_density(alpha = 0.7) +
  labs(title = "Fagerström Score Distribution by Varenicline Use",
       x = "FTCD Score",
       y = "Density") +
  theme_minimal()

ggplot(data, aes(x = BA, y = bdi_score_w00, color = BA)) +
  geom_boxplot() +
  labs(title = "Depression Scores by Treatment Group",
       x = "Treatment Group",
       y = "BDI Score") +
  theme_minimal()


print(names(data))

heatmap_data <- data %>%
  select(abst, Var, BA, age_ps, sex_ps, ftcd_score, bdi_score_w00, cpd_ps, readiness)

print(head(heatmap_data))

heatmap_data_scaled <- as.data.frame(scale(heatmap_data))


library(corrplot) 
correlations <- cor(heatmap_data_scaled, use = "complete.obs")

corrplot(correlations, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         title = "Heatmap of Variables Related to Smoking Cessation",
         cl.lim = c(-1, 1))


data1 <- read.csv("project2.csv")

data1$sex_ps <- factor(data1$sex_ps, levels = c("1", "2"), labels = c("Male", "Female"))

data1 <- data1 %>%
  mutate(
    TreatmentGroup = factor(
      paste0(
        ifelse(BA == 0, "ST", "BASC"),
        " + ",
        ifelse(Var == 0, "placebo", "varenicline")
      )
    )
  )


data1 <- data1 %>%
  mutate(
    Race = case_when(
      NHW == 1 ~ "Non-Hispanic White",
      Black == 1 ~ "Black/African American",
      Hisp == 1 ~ "Hispanic",
      TRUE ~ "Other"
    )
  ) %>%
  select(-NHW, -Black, -Hisp)

data1 <- data1 %>%
  mutate(
    inc = factor(inc, levels = c("1", "2", "3", "4", "5"), labels = c("Less than $20,000", "$20,000–35,000", "$35,001–50,000", "$50,001–75,000", "More than $75,000")),
    edu = factor(edu, levels = c("1", "2", "3", "4", "5"), labels = c("Grade school", "Some high school", "High school graduate or GED", "Some college/technical school", "College graduate")),
    antidepmed = factor(antidepmed, levels = c("0", "1"), labels = c("No", "Yes")),
    mde_curr = factor(mde_curr, levels = c("0", "1"), labels = c("Past", "Current")),
    Only.Menthol = factor(Only.Menthol, levels = c("0", "1"), labels = c("No", "Yes")),
    otherdiag = factor(otherdiag, levels = c("0", "1"), labels = c("No", "Yes")),
    abst = factor(abst, levels = c("0", "1"), labels = c("No", "Yes")),
  )

data1$readiness <- as.numeric(as.character(data1$readiness))

data1 <- data1 %>%
  rename(
    "Interview Age" = age_ps,
    "Gender at Interview" = sex_ps,
    "Income Bracket" = inc,
    "Educational Attainment" = edu,
    "Baseline Dependence Score" = ftcd_score,
    "Smoking Within First 5 Minutes" = ftcd.5.mins,
    "Baseline Depression Score" = bdi_score_w00,
    "Daily Cigarette Count" = cpd_ps,
    "Initial Cigarette Value Perception" = crv_total_pq1,
    "Alternative Rewards Scale" = hedonsum_n_pq1,
    "Additional Rewards Scale" = hedonsum_y_pq1,
    "Pleasure Deficit Score" = shaps_score_pq1,
    "Additional DSM Diagnoses" = otherdiag,
    "Use of Antidepressants" = antidepmed,
    "Current vs Past Depression" = mde_curr,
    "Nicotine Conversion Rate" = NMR,
    "Exclusive Menthol Usage" = Only.Menthol,
    "Motivation to Quit Smoking" = readiness
  )


# Create the summary table
table_summary <- data1 %>%
  select(TreatmentGroup, Race, `Interview Age`, `Gender at Interview`, `Income Bracket`, `Educational Attainment`, `Baseline Dependence Score`, 
         `Smoking Within First 5 Minutes`, `Baseline Depression Score`, `Daily Cigarette Count`, `Initial Cigarette Value Perception`, 
         `Alternative Rewards Scale`, `Additional Rewards Scale`, `Pleasure Deficit Score`, `Additional DSM Diagnoses`, 
         `Use of Antidepressants`, `Current vs Past Depression`, `Nicotine Conversion Rate`, `Exclusive Menthol Usage`, `Motivation to Quit Smoking`) %>%
  tbl_summary(
    by = TreatmentGroup,  
    type = all_continuous() ~ "continuous2",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall(col_label = "Overall Sample", last = TRUE) %>%
  bold_labels() %>%
  as_gt() %>%
  gt::tab_header(
    title = "Table 1. Summary Table",
    subtitle = "Summary of baseline characteristics by treatment group"
  )

# Print the table
print(table_summary)

gt_table <- as_gt(table_summary)
gt::gtsave(gt_table, filename = "table_summary.pdf")

####Data Loading and EDA######










####### Variable Selection#######
library(tidyverse)  
library(glmnet)     
library(leaps)       
library(MASS)        
library(caret)       
library(gt)         
library(gtsummary)   
library(broom)      

set.seed(123)

#----------------------------
# Data Preparation
#----------------------------


data <- read.csv("project2.csv")

data <- data %>%
  mutate(across(where(is.character), as.factor),
         abst = as.factor(abst)) 


data <- na.omit(data)

response_var <- "abst"
predictor_vars <- setdiff(names(data), response_var)

X <- model.matrix(as.formula(paste(response_var, "~ .")), data)[, -1]  
y <- data[[response_var]]


trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[trainIndex, ]
X_test  <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test  <- y[-trainIndex]


coef_tables <- list()
metrics_tables <- list()





#----------------------------
# 1. Variable Selection using Relaxed Lasso (Simulated with glmnet)
#----------------------------


cv_lasso <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 1)


best_lambda <- cv_lasso$lambda.min


lasso_coef <- coef(cv_lasso, s = best_lambda)
selected_vars <- rownames(lasso_coef)[lasso_coef[, 1] != 0]
selected_vars <- selected_vars[-1]  


if (length(selected_vars) > 0) {
  formula_relaxed <- as.formula(paste(response_var, "~", paste(selected_vars, collapse = "+")))
  model_relaxed <- glm(formula_relaxed, data = data[trainIndex, ], family = "binomial")
  

  coef_table_rl <- tbl_regression(model_relaxed, exponentiate = TRUE) %>%
    bold_labels() %>%
    italicize_levels() %>%
    modify_header(label = "**Variable**", estimate = "**Odds Ratio (OR)**")
  

  coef_table_rl_df <- coef_table_rl %>% as_tibble() %>%
    mutate(method = "Relaxed Lasso", .before = 1)
  

  coef_tables[[1]] <- coef_table_rl_df
  
  pred_prob <- predict(model_relaxed, newdata = data[-trainIndex, ], type = "response")
  pred_class <- ifelse(pred_prob > 0.5, levels(y_train)[2], levels(y_train)[1])
  

  cm_rl <- confusionMatrix(factor(pred_class, levels = levels(y_test)), y_test)
  

  metrics_table_rl <- tibble(
    Method = "Relaxed Lasso",
    Accuracy = as.numeric(cm_rl$overall['Accuracy']),
    Sensitivity = as.numeric(cm_rl$byClass['Sensitivity']),
    Specificity = as.numeric(cm_rl$byClass['Specificity']),
    Kappa = as.numeric(cm_rl$overall['Kappa'])
  )
  
  # Store the metrics table
  metrics_tables[[1]] <- metrics_table_rl
} else {
  print("No variables selected by Lasso.")
}



#----------------------------
# 2. Variable Selection using Best Subset Selection
#----------------------------


data_subset <- data.frame(y_train = as.numeric(as.character(y_train)), X_train)


regfit_full <- regsubsets(y_train ~ ., data = data_subset, nvmax = 10, method = "exhaustive")
reg_summary <- summary(regfit_full)

best_model_size <- which.min(reg_summary$bic)
best_vars <- names(coef(regfit_full, best_model_size))[-1]  

if (length(best_vars) > 0) {
  formula_best_subset <- as.formula(paste(response_var, "~", paste(best_vars, collapse = "+")))
  model_best_subset <- glm(formula_best_subset, data = data[trainIndex, ], family = "binomial")
  
  coef_table_bss <- tbl_regression(model_best_subset, exponentiate = TRUE) %>%
    bold_labels() %>%
    italicize_levels() %>%
    modify_header(label = "**Variable**", estimate = "**Odds Ratio (OR)**")
  
  coef_table_bss_df <- coef_table_bss %>% as_tibble() %>%
    mutate(method = "Best Subset Selection", .before = 1)
  
  coef_tables[[2]] <- coef_table_bss_df
  pred_prob <- predict(model_best_subset, newdata = data[-trainIndex, ], type = "response")
  pred_class <- ifelse(pred_prob > 0.5, levels(y_train)[2], levels(y_train)[1])
  cm_bss <- confusionMatrix(factor(pred_class, levels = levels(y_test)), y_test)
  
  metrics_table_bss <- tibble(
    Method = "Best Subset Selection",
    Accuracy = as.numeric(cm_bss$overall['Accuracy']),
    Sensitivity = as.numeric(cm_bss$byClass['Sensitivity']),
    Specificity = as.numeric(cm_bss$byClass['Specificity']),
    Kappa = as.numeric(cm_bss$overall['Kappa'])
  )
  
  # Store the metrics table
  metrics_tables[[2]] <- metrics_table_bss
} else {
  print("No variables selected by Best Subset Selection.")
}



#----------------------------
# 3. Variable Selection using Forward Stepwise Selection
#----------------------------
null_model <- glm(as.formula(paste(response_var, "~ 1")), data = data[trainIndex, ], family = "binomial")
full_model <- glm(as.formula(paste(response_var, "~ .")), data = data[trainIndex, ], family = "binomial")

step_forward <- stepAIC(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward", trace = FALSE)

coef_table_fs <- tbl_regression(step_forward, exponentiate = TRUE) %>%
  bold_labels() %>%
  italicize_levels() %>%
  modify_header(label = "**Variable**", estimate = "**Odds Ratio (OR)**")

coef_table_fs_df <- coef_table_fs %>% as_tibble() %>%
  mutate(method = "Forward Stepwise", .before = 1)

coef_tables[[3]] <- coef_table_fs_df

pred_prob <- predict(step_forward, newdata = data[-trainIndex, ], type = "response")
pred_class <- ifelse(pred_prob > 0.5, levels(y_train)[2], levels(y_train)[1])

cm_fs <- confusionMatrix(factor(pred_class, levels = levels(y_test)), y_test)

metrics_table_fs <- tibble(
  Method = "Forward Stepwise",
  Accuracy = as.numeric(cm_fs$overall['Accuracy']),
  Sensitivity = as.numeric(cm_fs$byClass['Sensitivity']),
  Specificity = as.numeric(cm_fs$byClass['Specificity']),
  Kappa = as.numeric(cm_fs$overall['Kappa'])
)

metrics_tables[[3]] <- metrics_table_fs



#----------------------------
# 4. Variable Selection using Backward Stepwise Selection
#----------------------------
full_model <- glm(as.formula(paste(response_var, "~ .")), data = data[trainIndex, ], family = "binomial")

step_backward <- stepAIC(full_model, direction = "backward", trace = FALSE)

coef_table_bws <- tbl_regression(step_backward, exponentiate = TRUE) %>%
  bold_labels() %>%
  italicize_levels() %>%
  modify_header(label = "**Variable**", estimate = "**Odds Ratio (OR)**")
coef_table_bws_df <- coef_table_bws %>% as_tibble() %>%
  mutate(method = "Backward Stepwise", .before = 1)

coef_tables[[4]] <- coef_table_bws_df

pred_prob <- predict(step_backward, newdata = data[-trainIndex, ], type = "response")
pred_class <- ifelse(pred_prob > 0.5, levels(y_train)[2], levels(y_train)[1])

cm_bws <- confusionMatrix(factor(pred_class, levels = levels(y_test)), y_test)

metrics_table_bws <- tibble(
  Method = "Backward Stepwise",
  Accuracy = as.numeric(cm_bws$overall['Accuracy']),
  Sensitivity = as.numeric(cm_bws$byClass['Sensitivity']),
  Specificity = as.numeric(cm_bws$byClass['Specificity']),
  Kappa = as.numeric(cm_bws$overall['Kappa'])
)

metrics_tables[[4]] <- metrics_table_bws



#----------------------------
# Combine Coefficient Tables into One Table
#----------------------------
combined_coef_df <- bind_rows(coef_tables)

combined_coef_table <- combined_coef_df %>%
  gt(groupname_col = "method") %>%
  tab_header(
    title = "Combined Coefficient Table",
    subtitle = "Variable Selection Methods"
  ) %>%
  cols_label(
    `**Variable**` = "Variable",
    `**Odds Ratio (OR)**` = "Odds Ratio",
    `**95% CI**` = "95% CI",
    `**p-value**` = "p-value"
  ) %>%
  fmt_number(
    columns = c(`**Odds Ratio (OR)**`, `**p-value**`),
    decimals = 3
  )

print(combined_coef_table)



#----------------------------
# Combine Evaluation Metrics Tables into One Table
#----------------------------
combined_metrics_df <- bind_rows(metrics_tables)
combined_metrics_table <- combined_metrics_df %>%
  gt(rowname_col = "Method") %>%
  tab_header(
    title = "Combined Evaluation Metrics Table",
    subtitle = "Variable Selection Methods"
  ) %>%
  fmt_number(
    columns = c("Accuracy", "Sensitivity", "Specificity", "Kappa"),
    decimals = 3
  )

print(combined_metrics_table)

#####Variable selection#######










#####Model Selection#########
library(caret)
library(glmnet)
library(pROC)
library(ggplot2)
library(dplyr)
library(ROSE)  
data <- read.csv("project2.csv")

data$abst <- factor(data$abst, levels = c("0", "1"), labels = c("Class0", "Class1"))
data$sex_ps <- factor(data$sex_ps, levels = c("1", "2"), labels = c("Male", "Female"))
data$mde_curr <- factor(data$mde_curr, levels = c("0", "1"), labels = c("No", "Yes"))

set.seed(123)
trainIndex <- createDataPartition(data$abst, p = 0.8, list = TRUE)
train_data <- data[trainIndex[[1]], ]
test_data <- data[-trainIndex[[1]], ]

if (!require("ROSE")) {
  install.packages("ROSE", dependencies = TRUE)
  library(ROSE)
}

train_data_balanced <- ovun.sample(abst ~ ., data = train_data, method = "over", N = 2*table(train_data$abst)[1])$data
print(table(train_data_balanced$abst)) 

X_train_scaled <- scale(train_data_balanced[, sapply(train_data_balanced, is.numeric)])
X_test_scaled <- scale(test_data[, sapply(test_data, is.numeric)])
X_test_scaled <- scale(test_data[, sapply(test_data, is.numeric)])
y_train <- train_data_balanced$abst
y_test <- test_data$abst

weights <- ifelse(y_train == "Class1", (1/table(y_train)[1]), (1/table(y_train)[2]))
logistic_model <- glm(abst ~ ., data = train_data_balanced, family = binomial(), weights = weights)

cv_lasso <- cv.glmnet(X_train_scaled, y_train, alpha = 1, family = "binomial", nfolds = 10, weights = weights)
best_lambda_lasso <- cv_lasso$lambda.min
lasso_model <- glmnet(X_train_scaled, y_train, alpha = 1, lambda = best_lambda_lasso, family = "binomial")

cv_ridge <- cv.glmnet(X_train_scaled, y_train, alpha = 0, family = "binomial", nfolds = 10, weights = weights)
best_lambda_ridge <- cv_ridge$lambda.min
ridge_model <- glmnet(X_train_scaled, y_train, alpha = 0, lambda = best_lambda_ridge, family = "binomial")

elastic_net_model <- train(
  x = X_train_scaled,
  y = y_train,
  method = "glmnet",
  metric = "ROC",
  trControl = trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary, savePredictions = "final"),
  tuneLength = 10
)

logistic_pred_class <- predict(logistic_model, newdata = test_data, type = "response")
lasso_pred_class <- predict(lasso_model, newx = X_test_scaled, type = "response")
ridge_pred_class <- predict(ridge_model, newx = X_test_scaled, type = "response")
enet_pred_class <- predict(elastic_net_model, newdata = X_test_scaled, type = "raw")

cm_logistic <- confusionMatrix(as.factor(logistic_pred_class), as.factor(y_test))
cm_lasso <- confusionMatrix(as.factor(lasso_pred_class), as.factor(y_test))
cm_ridge <- confusionMatrix(as.factor(ridge_pred_class), as.factor(y_test))
cm_enet <- confusionMatrix(as.factor(enet_pred_class), as.factor(y_test))

data_lasso <- extract_cm_data(cm_lasso, "Lasso")
data_ridge <- extract_cm_data(cm_ridge, "Ridge")
data_enet <- extract_cm_data(cm_enet, "Elastic Net")

combined_cm_data <- bind_rows(data_lasso, data_ridge, data_enet, data_logistic)
plot_data <- combined_cm_data %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = -Model, names_to = "Prediction", values_to = "Count")

plot_data$Prediction <- factor(plot_data$Prediction, levels = c("Reference", "Prediction"))

confusion_matrix_plot <- ggplot(plot_data, aes(x = Prediction, y = Count, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~Model, scales = "free") +
  theme_minimal() +
  labs(title = "Confusion Matrix Comparison",
       x = "Class Type",
       y = "Count",
       fill = "Model") +
  scale_fill_brewer(palette = "Dark2") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(colour = "black", fill = "lightblue")
  )

print(confusion_matrix_plot)

roc_lasso <- roc(y_test, as.numeric(lasso_pred_prob))
roc_ridge <- roc(y_test, as.numeric(ridge_pred_prob))
roc_enet <- roc(y_test, as.numeric(enet_pred_prob))


# ROC Curves
roc_logistic <- roc(y_test, as.numeric(logistic_pred_prob))

plot(roc_lasso, main = "ROC Curves for All Models", col = "red", xlim = c(0, 1), ylim = c(0, 1))
lines(roc_ridge, col = "blue")
lines(roc_enet, col = "green")
lines(roc_logistic, col = "purple")

legend("bottomright", legend = c(paste("Lasso (AUC =", round(auc(roc_lasso), 2), ")"),
                                 paste("Ridge (AUC =", round(auc(roc_ridge), 2), ")"),
                                 paste("Elastic Net (AUC =", round(auc(roc_enet), 2), ")"),
                                 paste("Logistic (AUC =", round(auc(roc_logistic), 2), ")")),
       col = c("red", "blue", "green", "purple"), lwd = 2)


grid()
xlabel("False Positive Rate")
ylabel("True Positive Rate")

# Cross-Validation Error Plot
plot(cv_lasso, main="Cross-Validation Error for Lasso")
plot(cv_ridge, main="Cross-Validation Error for Ridge")
library(ggplot2)

results <- logistic_cv$resample  # This assumes that 'resample' has the summary by fold
ggplot(results, aes(x = Resample, y = ROC)) +
  geom_line(group=1) +
  geom_point() +
  labs(title = "Cross-Validation Error for Logistic", x = "Fold", y = "ROC AUC") +
  theme_minimal()
plot(elastic_net_model$results$lambda, elastic_net_model$results$ROC, type="b", main="Cross-Validation Error for Elastic Net", xlab="Lambda", ylab="ROC")


data <- read.csv("project2.csv")
data <- na.omit(data)
data$abst <- factor(data$abst, levels = c("0", "1"), labels = c("Class0", "Class1"))
data$sex_ps <- factor(data$sex_ps, levels = c("1", "2"), labels = c("Male", "Female"))
data$mde_curr <- factor(data$mde_curr, levels = c("0", "1"), labels = c("No", "Yes"))

set.seed(123)
trainIndex <- createDataPartition(data$abst, p = 0.8, list = TRUE)
train_data <- data[trainIndex[[1]], ]
test_data <- data[-trainIndex[[1]], ]

train_data_balanced <- ovun.sample(abst ~ ., data = train_data, method = "over", N = 2*table(train_data$abst)[1])$data
X_train_scaled <- scale(train_data_balanced[, sapply(train_data_balanced, is.numeric)])
X_test_scaled <- scale(test_data[, sapply(test_data, is.numeric)])
y_train <- train_data_balanced$abst
y_test <- test_data$abst

# Model training and evaluation
# Lasso Regression
cv_lasso <- cv.glmnet(X_train_scaled, y_train, alpha = 1, family = "binomial", nfolds = 10)
best_lambda_lasso <- cv_lasso$lambda.min
lasso_model <- glmnet(X_train_scaled, y_train, alpha = 1, lambda = best_lambda_lasso, family = "binomial")
lasso_pred_class <- predict(lasso_model, newx = X_test_scaled, type = "class")

# Ridge Regression
cv_ridge <- cv.glmnet(X_train_scaled, y_train, alpha = 0, family = "binomial", nfolds = 10)
best_lambda_ridge <- cv_ridge$lambda.min
ridge_model <- glmnet(X_train_scaled, y_train, alpha = 0, lambda = best_lambda_ridge, family = "binomial")
ridge_pred_class <- predict(ridge_model, newx = X_test_scaled, type = "class")

# Elastic Net Regression
cv_enet <- cv.glmnet(X_train_scaled, y_train, alpha = 0.5, family = "binomial", nfolds = 10)
best_lambda_enet <- cv_enet$lambda.min
enet_model <- glmnet(X_train_scaled, y_train, alpha = 0.5, lambda = best_lambda_enet, family = "binomial")
enet_pred_class <- predict(enet_model, newx = X_test_scaled, type = "class")

# Logistic Regression Model
logistic_model <- glm(abst ~ ., data = train_data_balanced, family = "binomial")
logistic_pred_class <- predict(logistic_model, newdata = test_data, type = "response")
logistic_pred_class <- ifelse(logistic_pred_class > 0.5, "Class1", "Class0")

# Calculate confusion matrices
cm_lasso <- confusionMatrix(as.factor(lasso_pred_class), y_test)
cm_ridge <- confusionMatrix(as.factor(ridge_pred_class), y_test)
cm_enet <- confusionMatrix(as.factor(enet_pred_class), y_test)
cm_logistic <- confusionMatrix(as.factor(logistic_pred_class), y_test)

# Model Metrics Table
metrics_df <- data.frame(
  Model = c("Lasso", "Ridge", "Elastic Net", "Logistic Regression"),
  Accuracy = c(cm_lasso$overall['Accuracy'], cm_ridge$overall['Accuracy'], cm_enet$overall['Accuracy'], cm_logistic$overall['Accuracy']),
  Recall = c(cm_lasso$byClass['Sensitivity'], cm_ridge$byClass['Sensitivity'], cm_enet$byClass['Sensitivity'], cm_logistic$byClass['Sensitivity']),
  Precision = c(cm_lasso$byClass['Precision'], cm_ridge$byClass['Precision'], cm_enet$byClass['Precision'], cm_logistic$byClass['Precision']),
  F1_Score = c(cm_lasso$byClass['F1'], cm_ridge$byClass['F1'], cm_enet$byClass['F1'], cm_logistic$byClass['F1'])
)


gt_table <- gt(metrics_df) %>%
  tab_header(
    title = "Model Evaluation Metrics",
    subtitle = "Comparative overview of model performance"
  ) %>%
  cols_label(
    Model = "Model",
    Accuracy = "Accuracy",
    Recall = "Recall",
    Precision = "Precision",
    F1_Score = "F1 Score"
  ) %>%
  fmt_number(
    columns = vars(Accuracy, Recall, Precision, F1_Score),
    decimals = 3
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "gray95")
    ),
    locations = cells_body(columns = TRUE)
  ) %>%
  tab_options(
    table.font.size = "small"
  )

print(gt_table)


data <- read.csv("project2.csv")
data <- na.omit(data)

if ("Treatment" %in% names(data) && "Varenicline" %in% names(data)) {
  data$Treatment <- factor(data$Treatment, levels = c("BASC", "ST"))
  data$Varenicline <- factor(data$Varenicline, levels = c("placebo", "varenicline"))
  
  if (length(unique(data$Treatment)) > 1) {
    balanced_data <- ovun.sample(Treatment ~ ., data = data, method = "over", N = 2 * table(data$Treatment)[1])$data
    
    trainIndex <- createDataPartition(balanced_data$Treatment, p = 0.8, list = FALSE)
    train_data <- balanced_data[trainIndex, ]
    test_data <- balanced_data[-trainIndex, ]
    
    logit_model <- glm(Treatment ~ Varenicline + Age + Smoking_Duration + Anhedonia_Score, family = binomial, data = train_data)

    predictions <- predict(logit_model, newdata = test_data, type = "response")
    predicted_classes <- ifelse(predictions > 0.5, "BASC", "ST")
    conf_matrix <- table(Predicted = predicted_classes, Actual = test_data$Treatment)
    
    # Calculate performance metrics
    accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
    precision <- posPredValue(predicted_classes, test_data$Treatment, positive = "BASC")
    recall <- sensitivity(predicted_classes, test_data$Treatment, positive = "BASC")
    f1_score <- (2 * precision * recall) / (precision + recall)
    
    cat("Accuracy:", accuracy, "\n")
    cat("Precision:", precision, "\n")
    cat("Recall:", recall, "\n")
    cat("F1 Score:", f1_score, "\n")
  } else {
    cat("Insufficient levels in Treatment for oversampling.\n")
  }
} else {
  cat("Required columns are missing or have been filtered out.\n")
}



data <- read.csv("project2.csv")
data <- na.omit(data) 

# Ensure 'Treatment' and 'Varenicline' are correctly factored
data$Treatment <- factor(data$Treatment, levels = c("BASC", "ST"))
data$Varenicline <- factor(data$Varenicline, levels = c("placebo", "varenicline"))

# Address class imbalance via oversampling
balanced_data <- ovun.sample(Treatment ~ ., data = data, method = "over", N = 2 * table(data$Treatment)[1])$data

set.seed(123)  
trainIndex <- createDataPartition(balanced_data$Treatment, p = 0.8, list = FALSE)
train_data <- balanced_data[trainIndex, ]
test_data <- balanced_data[-trainIndex, ]

# Model fitting with Logistic Regression
logit_model <- glm(Treatment ~ Varenicline + Age + Smoking_Duration + Anhedonia_Score, family = binomial(), data = train_data)

predictions <- predict(logit_model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, "BASC", "ST")
conf_matrix <- table(Predicted = predicted_classes, Actual = test_data$Treatment)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- posPredValue(predicted_classes, test_data$Treatment, positive = "BASC")
recall <- sensitivity(predicted_classes, test_data$Treatment, positive = "BASC")
f1_score <- (2 * precision * recall) / (precision + recall)

cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")


set.seed(42)

# Calculate indices for 60% training, 20% validation, 20% test
total_rows <- nrow(data)
train_rows <- round(total_rows * 0.6)
temp_rows <- total_rows - train_rows
validate_rows <- round(temp_rows * 0.5)

indices <- sample(total_rows)
train_indices <- indices[1:train_rows]
validate_indices <- indices[(train_rows + 1):(train_rows + validate_rows)]
test_indices <- indices[(train_rows + validate_rows + 1):total_rows]

train <- data[train_indices, ]
validate <- data[validate_indices, ]
test <- data[test_indices, ]

scale_data <- function(data) {
  numeric_columns <- sapply(data, is.numeric)
  data[numeric_columns] <- scale(data[numeric_columns])
  return(data)
}

train_scaled <- scale_data(train)
validate_scaled <- scale_data(validate)
test_scaled <- scale_data(test)


train$set <- 'Train'
validate$set <- 'Validate'
test$set <- 'Test'

combined_data <- rbind(train, validate, test)

ggplot(combined_data, aes(x = age_ps, fill = set)) +
  geom_histogram(bins = 30, alpha = 0.6, position = 'identity') +
  labs(title = "Distribution of Age Across Data Splits",
       x = "Age",
       y = "Frequency") +
  scale_fill_manual(values = c("Train" = "blue", "Validate" = "green", "Test" = "red")) +
  theme_minimal()

train_scaled$set <- 'Train Scaled'
validate_scaled$set <- 'Validate Scaled'
test_scaled$set <- 'Test Scaled'

combined_scaled <- rbind(train_scaled, validate_scaled, test_scaled)

# Plotting original vs. scaled distributions of 'age_ps'
ggplot(combined_scaled, aes(x = age_ps, fill = set)) +
  geom_histogram(bins = 30, alpha = 0.6, position = 'identity') +
  labs(title = "Distribution of Scaled Age Across Data Splits",
       x = "Scaled Age",
       y = "Frequency") +
  scale_fill_manual(values = c("Train Scaled" = "blue", "Validate Scaled" = "green", "Test Scaled" = "red")) +
  theme_minimal()


x_train <- as.matrix(train_scaled[, -which(names(train_scaled) == "abst")])
y_train <- train_scaled$abst
x_validate <- as.matrix(validate_scaled[, -which(names(validate_scaled) == "abst")])
y_validate <- validate_scaled$abst
x_test <- as.matrix(test_scaled[, -which(names(test_scaled) == "abst")])
y_test <- test_scaled$abst


ridge_model <- glmnet(x_train, y_train, alpha = 0)
lasso_model <- glmnet(x_train, y_train, alpha = 1)


cv_ridge <- cv.glmnet(x_validate, y_validate, alpha = 0)
cv_lasso <- cv.glmnet(x_validate, y_validate, alpha = 1)


best_lambda_ridge <- cv_ridge$lambda.min
best_lambda_lasso <- cv_lasso$lambda.min

ridge_best <- glmnet(x_train, y_train, alpha = 0, lambda = best_lambda_ridge)
lasso_best <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda_lasso)

ridge_pred <- predict(ridge_best, s = best_lambda_ridge, newx = x_test)
lasso_pred <- predict(lasso_best, s = best_lambda_lasso, newx = x_test)

ridge_test_error <- sqrt(mean((y_test - ridge_pred)^2))
lasso_test_error <- sqrt(mean((y_test - lasso_pred)^2))

print(paste("Ridge Test RMSE:", ridge_test_error))
print(paste("Lasso Test RMSE:", lasso_test_error))

# Coefficient path for Ridge
plot(ridge_model, xvar = "lambda", label = TRUE)
title("Coefficient Path for Ridge Regression")

# Coefficient path for Lasso
plot(lasso_model, xvar = "lambda", label = TRUE)
title("Coefficient Path for Lasso Regression")

plot(cv_ridge)
with(cv_ridge, abline(v = log(lambda.min), col = "red"))
title("Cross-Validation for Ridge")

# Plot for Lasso Cross-Validation
plot(cv_lasso)
with(cv_lasso, abline(v = log(lambda.min), col = "red"))
title("Cross-Validation for Lasso")

ridge_results <- data.frame(Actual = y_test, Predicted = as.vector(ridge_pred))
lasso_results <- data.frame(Actual = y_test, Predicted = as.vector(lasso_pred))

# Ridge Prediction Plot
ggplot(ridge_results, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  ggtitle("Ridge Regression: Actual vs Predicted") +
  labs(x = "Actual Values", y = "Predicted Values") +
  theme_minimal()

# Lasso Prediction Plot
ggplot(lasso_results, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Lasso Regression: Actual vs Predicted") +
  labs(x = "Actual Values", y = "Predicted Values") +
  theme_minimal()


if (!require(pROC)) install.packages("pROC")
if (!require(ROCR)) install.packages("ROCR")
if (!require(ResourceSelection)) install.packages("ResourceSelection")
library(pROC)
library(ROCR)
library(ResourceSelection)

ridge_prob <- predict(ridge_best, newx = x_test, s = best_lambda_ridge, type = "response")
lasso_prob <- predict(lasso_best, newx = x_test, s = best_lambda_lasso, type = "response")

# ROC Curve for Ridge
roc_ridge <- roc(y_test, ridge_prob)
plot(roc_ridge, main = "ROC Curve for Ridge Regression")
auc_ridge <- auc(roc_ridge)
print(paste("AUC for Ridge:", auc_ridge))

# ROC Curve for Lasso
roc_lasso <- roc(y_test, lasso_prob)
plot(roc_lasso, main = "ROC Curve for Lasso Regression")
auc_lasso <- auc(roc_lasso)
print(paste("AUC for Lasso:", auc_lasso))

# Create deciles of risk
lasso_prob_adjusted <- lasso_prob + runif(length(lasso_prob), min = -1e-5, max = 1e-5)
lasso_prob_deciles <- cut(lasso_prob_adjusted, breaks = quantile(lasso_prob_adjusted, probs = seq(0, 1, by = 0.1)), include.lowest = TRUE, labels = FALSE)
hosmer_test_lasso <- hoslem.test(y_test, lasso_prob_adjusted, g = 10)
print(hosmer_test_lasso)

ggplot(data.frame(Probability = c(ridge_prob, lasso_prob_adjusted), 
                  Model = rep(c("Ridge", "Lasso"), each = length(ridge_prob))),
       aes(x = Probability, fill = Model)) +
  geom_histogram(bins = 40, alpha = 0.6, position = "identity") +
  labs(title = "Distribution of Predicted Probabilities",
       x = "Predicted Probability", y = "Count") +
  scale_fill_manual(values = c("blue", "red"))


my_data <- read.csv("project2.csv")
my_data <- my_data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.character), as.factor))

print(is.data.frame(my_data))
str(my_data)

tbl <- tbl_summary(
  data = my_data,
  by = "mde_curr",  
  include = c("abst", "Var", "BA", "age_ps", "sex_ps", "NHW", "Black", "Hisp", "inc"),
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  )
) %>%
  add_p() 

gt_table <- as_gt(tbl)
print(gt_table)


library(dplyr)

#### Model Selection#####










######Final Result#####
data <- read.csv("project2.csv", stringsAsFactors = TRUE)

print(data)

data$abst <- factor(data$abst)
data$sex_ps <- factor(data$sex_ps, labels = c("Male", "Female"))
data$treatment_type <- factor(data$Var)  
data$pharmacotherapy <- factor(data$BA)  

data$readiness <- ifelse(is.na(data$readiness), mean(data$readiness, na.rm = TRUE), data$readiness)  # Impute with mean or another method

model <- glm(abst ~ treatment_type * age_ps + treatment_type * sex_ps +
               treatment_type * ftcd_score + treatment_type * bdi_score_w00 +
               treatment_type * readiness + pharmacotherapy,
             family = binomial(link = "logit"), data = data)

par(mfrow = c(2, 2))
plot(model)
if (!requireNamespace("gtsummary", quietly = TRUE)) {
  install.packages("gtsummary")
}

library(gtsummary)

model <- glm(abst ~ treatment_type * age_ps + treatment_type * sex_ps +
               treatment_type * ftcd_score + treatment_type * bdi_score_w00 +
               treatment_type * readiness + pharmacotherapy,
             family = binomial(link = "logit"), data = data)

tbl_regression <- tbl_regression(model, exponentiate = TRUE)
print(tbl_regression)


library(broom)
library(gtsummary)

tbl_regression_model <- tbl_regression(model, exponentiate = TRUE)


print(tbl_regression_model)

###### Final Result######

