############################################################################
############################################################################
## This is the source code for Jeff Idle's HR attrition analysis example.
## This script sets up the environment and creates the environment objects
## that are used by the main R markdown file (hr_example_notebook.Rmd).
############################################################################
############################################################################



# Load libraries
############################################################################

library(tidyverse)
library(shiny)
library(survival)
library(survminer)
library(ggfortify)
library(scales)
library(lubridate)
library(caret)
library(randomForest)
library(ipred)
library(gbm)
library(broom)
library(rlang)
library(gridExtra)
library(klaR)
library(rpart)
library(gt)
library(survey)
library(pscl)
library(C50)
library(ROCR)
library(knitr)



# Set up color palette for this analysis
############################################################################

color_test_df <- data.frame(
        
        letter = letters[1:10],
        number = seq(11:20)
        
)

custom_palette <- c("#000000",
                    "#8f8f8f",
                    "#5378b5",
                    "#f46242",
                    "#1bb764",
                    "#cc4b4b",
                    "#8a32c1",
                    "#398ed3",
                    "#aa9620",
                    "#d83aae")

color_test_viz <- ggplot(color_test_df, aes(x = letter, y = number, fill = letter)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = custom_palette) +
        coord_flip() +
        theme_minimal()

#color_test_viz



# Load the data
############################################################################

attrition_df <- read.csv("data/ibm_attrition.csv", stringsAsFactors = FALSE)
attrition_ref_data_df <- read.csv("data/ibm_attrition_ref_data.csv", stringsAsFactors = FALSE)



# Structure the data and add columns to convert numeric values to text
############################################################################

attrition_df <- merge(x = attrition_df, y = attrition_ref_data_df[attrition_ref_data_df$column == "Education", 2:3], by.x = "Education", by.y = "code", all.x = TRUE, all.y = FALSE)
col_count <- ncol(attrition_df)
colnames(attrition_df)[col_count] <- "EducationText"

attrition_df <- merge(x = attrition_df, y = attrition_ref_data_df[attrition_ref_data_df$column == "EnvironmentSatisfaction", 2:3], by.x = "EnvironmentSatisfaction", by.y = "code", all.x = TRUE, all.y = FALSE)
col_count <- ncol(attrition_df)
colnames(attrition_df)[col_count] <- "EnvironmentSatisfactionText"

attrition_df <- merge(x = attrition_df, y = attrition_ref_data_df[attrition_ref_data_df$column == "JobInvolvement", 2:3], by.x = "JobInvolvement", by.y = "code", all.x = TRUE, all.y = FALSE)
col_count <- ncol(attrition_df)
colnames(attrition_df)[col_count] <- "JobInvolvementText"

attrition_df <- merge(x = attrition_df, y = attrition_ref_data_df[attrition_ref_data_df$column == "JobSatisfaction", 2:3], by.x = "JobSatisfaction", by.y = "code", all.x = TRUE, all.y = FALSE)
col_count <- ncol(attrition_df)
colnames(attrition_df)[col_count] <- "JobSatisfactionText"

attrition_df <- merge(x = attrition_df, y = attrition_ref_data_df[attrition_ref_data_df$column == "PerformanceRating", 2:3], by.x = "PerformanceRating", by.y = "code", all.x = TRUE, all.y = FALSE)
col_count <- ncol(attrition_df)
colnames(attrition_df)[col_count] <- "PerformanceRatingText"

attrition_df <- merge(x = attrition_df, y = attrition_ref_data_df[attrition_ref_data_df$column == "RelationshipSatisfaction", 2:3], by.x = "RelationshipSatisfaction", by.y = "code", all.x = TRUE, all.y = FALSE)
col_count <- ncol(attrition_df)
colnames(attrition_df)[col_count] <- "RelationshipSatisfactionText"

attrition_df <- merge(x = attrition_df, y = attrition_ref_data_df[attrition_ref_data_df$column == "WorkLifeBalance", 2:3], by.x = "WorkLifeBalance", by.y = "code", all.x = TRUE, all.y = FALSE)
col_count <- ncol(attrition_df)
colnames(attrition_df)[col_count] <- "WorkLifeBalanceText"

attrition_df <- attrition_df %>%
        mutate(terminated = ifelse(Attrition == "Yes", 1, 0))

attrition_df <- as.data.frame(unclass(attrition_df))

attrition_data_df <- attrition_df[ , c(16:42, 8:14, 43)]
attrition_data_df$terminated <- factor(attrition_data_df$terminated)
attrition_df$Attrition <- factor(attrition_df$Attrition, levels = c("Yes", "No"))



# Create descriptive summary of the data
############################################################################

options(scipen = 999)

data_summary_df <- data.frame(
        
        data_element = names(attrition_df),
        data_type = unlist(lapply(attrition_df, class))
        
)

numeric_cols_df <- attrition_df %>% select_if(is.numeric)
integer_cols_df <- attrition_df %>% select_if(is.integer)
num_int_cols_df <- bind_cols(numeric_cols_df, integer_cols_df)
factor_cols_df <- attrition_df %>% select_if(is.factor)
character_cols_df <- attrition_df %>% select_if(is.character)

data_levels <- lapply(factor_cols_df, levels)

min_vals <- lapply(num_int_cols_df, min)
mean_vals <- lapply(num_int_cols_df, mean)
max_vals <- lapply(num_int_cols_df, max)
col_count <- length(data_levels)
col_count2 <- length(min_vals)

for(i in 1:col_count){
        
        tmp_level_df <- data.frame(
                
                data_element = names(data_levels[i]),
                data_level_count = paste0(length(data_levels[[i]]), collapse = ""),
                data_level_text = paste0(data_levels[[i]], collapse = ", ")
                
        )
        
        if(!exists("level_df")){
                
                level_df <- tmp_level_df 
                
        } else level_df <- bind_rows(level_df, tmp_level_df)
        
}

level_df <- level_df %>% mutate(data_summary = paste0("level count = ", data_level_count, "; levels = ", data_level_text))

for(i in 1:col_count2){
        
        tmp_numsum_df <- data.frame(
                
                data_element = names(min_vals[i]),
                min_value = min_vals[[i]],
                mean_value = mean_vals[[i]],
                max_value = max_vals[[i]]
                
        )
        
        if(!exists("numsum_df")){
                
                numsum_df <- tmp_numsum_df 
                
        } else numsum_df <- bind_rows(numsum_df, tmp_numsum_df)
        
}

numsum_df <- numsum_df %>% mutate(data_summary = paste0("min = ", min_value, "; mean = ", round(mean_value, 3), "; max = ", max_value))

col_summaries_df <- bind_rows(level_df[ , c(1, 4)], numsum_df[ , c(1, 5)])

data_summary_df <- merge(data_summary_df, col_summaries_df, by = "data_element", all.x = TRUE, all.y = FALSE)

data_summary_df <- data_summary_df %>%
        arrange(data_type, data_element)



# Create active versus terminated distribution chart
############################################################################

term_dist_viz <- ggplot(attrition_df, aes(x = Attrition, fill = Attrition)) +
        geom_bar(width = 0.5) +
        geom_text(stat = "count", aes(label = ..count..), hjust = -0.5) +
        scale_fill_manual(values = c("#ED8800", "#5378b5")) +
        labs(title = "Employee count by attrition category", subtitle = "(no = currently active; yes = terminated)", x = "", y = "") +
        coord_flip() +
        theme(panel.background = element_blank(),
              axis.line.x = element_blank(),
              axis.line.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.x = element_blank(),
              legend.position = "none")



# Create survival analysis data objects and visualizations
############################################################################

surv_object <- Surv(time = attrition_df$YearsAtCompany, event = attrition_df$terminated)

fit1 <- survfit(surv_object ~ 1, data = attrition_df)

model_df <- fortify(fit1)

model_top20_df <- model_df %>%
        arrange(time) %>%
        mutate(avg_probability_decrease = lag(surv) - surv) %>%
        filter(time < 40 & time > 0)

mean_rr <- mean(model_top20_df$avg_probability_decrease, na.rm = TRUE)
sd_rr <- sd(model_top20_df$avg_probability_decrease, na.rm = TRUE)
sd15_top_rr <- mean_rr + (1.5 * sd_rr)

model_top20_df <- model_top20_df %>%
        mutate(risk_increase_size = ifelse(avg_probability_decrease >= sd15_top_rr, ">= 1.5 standard deviations", "< 1.5 standard deviations")) %>%
        filter(time <= 20 & time > 0)

survival_viz <- ggplot(model_df, aes(x = time, y = surv)) +
        geom_line(size = 1.25) +
        scale_y_continuous(label = percent_format()) +
        labs(title = "Retention probability analysis", subtitle = "", x = "years at company", y = "retention probability") +
        theme_minimal()

risk_viz <- ggplot(model_top20_df, aes(x = reorder(time, desc(time)), y = avg_probability_decrease, fill = risk_increase_size)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("#798396", "#ED8800")) +
        scale_y_continuous(label = percent_format()) +
        geom_hline(yintercept = mean_rr, color = "#8F8F8F", size = 2, alpha = 0.5) +
        annotate("text", label = "- overall average = 1.4%", x = 5, y = 0.016, vjust = 0, hjust = 0.005) +
        coord_flip() +
        labs(title = "Increase in retention risk for each year of service", subtitle = "", x = "tenure (years)", y = "average retention risk increase") +
        theme_minimal()



# Create driver analysis data objects and visualization
############################################################################

driver_data_df <- attrition_data_df

# Remove StandardHours and Over18 since there is no variation and remove Attrition since it is redundant
driver_data_df <- driver_data_df[ , -c(10, 13, 29)]

model_driver_rf <- randomForest(terminated ~ ., data = driver_data_df[, -1])
driver_imp_rf <- varImp(model_driver_rf)
driver_imp_rf$driver <- row.names(driver_imp_rf)
driver_imp_rf <- driver_imp_rf[ , c(2, 1)] %>%
        arrange(desc(Overall))

variable_viz <- ggplot(driver_imp_rf, aes(x=reorder(driver, Overall), y = Overall)) +
        geom_point() +
        geom_segment(aes(x=driver,xend=driver,y=0,yend=Overall)) +
        labs(title = "Shadowrun attrition factor importance", subtitle = "(generated by random forest model)", x = "", y = "mean decrease in Gini") +
        theme(panel.background = element_blank(),
              panel.grid.major.x = element_line(color = "#dee2e8")) +
        coord_flip()


# Format data for modeling and create 80% training dataset and 20% test
# dataset
############################################################################

modeling_data_df <- attrition_data_df

# Remove StandardHours and Over18 since there is no variation and remove Attrition since it is redundant
modeling_data_df <- modeling_data_df[ , -c(10, 13, 35)]
modeling_data_df$Attrition <- as.character(modeling_data_df$Attrition)
modeling_data_df <- modeling_data_df %>% 
        dplyr::select(-Attrition, Attrition)

modeling_data_df$Attrition[modeling_data_df$Attrition == "No"] <- "active"
modeling_data_df$Attrition[modeling_data_df$Attrition == "Yes"] <- "termed"
modeling_data_df$Attrition <- factor(modeling_data_df$Attrition, levels = c("active", "termed"))

set.seed(1)

# Split the data into training and test set
training_samples_df <- modeling_data_df$EmployeeNumber %>%
        createDataPartition(p = 0.8, list = FALSE)  

train_data_df  <- modeling_data_df[training_samples_df, ]
test_data_df <- modeling_data_df[-training_samples_df, ]



# Create Naive Bayes, logistic regression, random forest and regression
# tree models using 80/20 train/test split validation
############################################################################

set.seed(1)

# Fit the models
model_nb <- NaiveBayes(Attrition ~ ., data = train_data_df[, -1])
model_lr <- glm(Attrition ~ ., data = train_data_df[, -1], family = "binomial")
model_rf <- randomForest(Attrition ~ ., data = train_data_df[, -1])
model_rt <- rpart(Attrition ~ ., data = train_data_df[, -1])

# Make predictions
predictions_nb <- model_nb %>% predict(test_data_df)
predictions_lr_resp <- model_lr %>% predict(test_data_df, type = "response")
predictions_lr <- factor(ifelse(predictions_lr_resp > 0.5, "termed", "active"), levels = c("active", "termed"))
predictions_rf <- model_rf %>% predict(test_data_df)
predictions_rt <- model_rt %>% predict(test_data_df, type = "class")



# Create confusion matrix for each 80/20 validated model and combine into
# a single table
############################################################################

# Confusion matrices
model_et_cm_nb <- confusionMatrix(predictions_nb$class, test_data_df$Attrition)
model_et_cm_lr <- confusionMatrix(predictions_lr, test_data_df$Attrition)
model_et_cm_rf <- confusionMatrix(predictions_rf, test_data_df$Attrition)
model_et_cm_rt <- confusionMatrix(predictions_rt, test_data_df$Attrition)

cm_nb_ut <- data.frame(as.matrix(model_et_cm_nb$table)) %>%
        mutate(pct = Freq / sum(Freq, na.rm = TRUE),
               outcome = ifelse(Reference == Prediction, "correct", ifelse(Reference == "active" & Prediction == "termed", "false negative", ifelse(Reference == "termed" & Prediction == "active", "false positive", "unknown"))),
               algorithm = "naive_bayes")

cm_lr_ut <- data.frame(as.matrix(model_et_cm_lr$table)) %>%
        mutate(pct = Freq / sum(Freq, na.rm = TRUE),
               outcome = ifelse(Reference == Prediction, "correct", ifelse(Reference == "active" & Prediction == "termed", "false negative", ifelse(Reference == "termed" & Prediction == "active", "false positive", "unknown"))),
               algorithm = "logistic_regression")

cm_rf_ut <- data.frame(as.matrix(model_et_cm_rf$table)) %>%
        mutate(pct = Freq / sum(Freq, na.rm = TRUE),
               outcome = ifelse(Reference == Prediction, "correct", ifelse(Reference == "active" & Prediction == "termed", "false negative", ifelse(Reference == "termed" & Prediction == "active", "false positive", "unknown"))),
               algorithm = "random_forest")

cm_rt_ut <- data.frame(as.matrix(model_et_cm_rt$table)) %>%
        mutate(pct = Freq / sum(Freq, na.rm = TRUE),
               outcome = ifelse(Reference == Prediction, "correct", ifelse(Reference == "active" & Prediction == "termed", "false negative", ifelse(Reference == "termed" & Prediction == "active", "false positive", "unknown"))),
               algorithm = "regression_tree")

confusion_matrix_et <- bind_rows(cm_nb_ut, cm_lr_ut, cm_rf_ut, cm_rt_ut)
names(confusion_matrix_et) <- c("predicted", "actual", "n_ut", "pct_ut", "outcome", "algorithm")

names(cm_lr_ut) <- c("predicted", "actual", "n_lr", "pct_lr", "outcome", "algorithm")
names(cm_nb_ut) <- c("predicted", "actual", "n_nb", "pct_nb", "outcome", "algorithm")
names(cm_rf_ut) <- c("predicted", "actual", "n_rf", "pct_rf", "outcome", "algorithm")
names(cm_rt_ut) <- c("predicted", "actual", "n_rt", "pct_rt", "outcome", "algorithm")

confusion_matrix_et_w <- merge(x = cm_lr_ut[ , c(1:5)], y = cm_nb_ut[ , c(1:5)], by = c("actual", "predicted", "outcome"), all.x = TRUE, all.y = TRUE)
confusion_matrix_et_w <- merge(x = confusion_matrix_et_w, y = cm_rf_ut[ , c(1:5)], by = c("actual", "predicted", "outcome"), all.x = TRUE, all.y = TRUE)
confusion_matrix_et_w <- merge(x = confusion_matrix_et_w, y = cm_rt_ut[ , c(1:5)], by = c("actual", "predicted", "outcome"), all.x = TRUE, all.y = TRUE)

confusion_matrix_et_w <- confusion_matrix_et_w %>%
        arrange(outcome, actual, predicted)


names(confusion_matrix_et) <- c("predicted", "actual", "n_ut", "pct_ut", "outcome", "algorithm")

confusion_matrix_et <- confusion_matrix_et %>%
        dplyr::select(actual, predicted, outcome, n_ut, pct_ut, algorithm) %>%
        arrange(algorithm, outcome, actual, predicted)

confusion_matrix_et$phase <- "eighty_twenty"



# Put confusion matrix performance measures into a table to compare across
# the four 80/20 validated models
############################################################################

metric_labels <- row.names(as.data.frame(c(model_et_cm_nb$overall, model_et_cm_nb$byClass)))

perf_metrics_nb_et <- data.frame(metric = metric_labels,
                                 metric_result = c(model_et_cm_nb$overall, model_et_cm_nb$byClass)) %>%
        mutate(algorithm = "naive_bayes",
               phase = "eighty_twenty")

perf_metrics_lr_et <- data.frame(metric = metric_labels,
                                 metric_result = c(model_et_cm_lr$overall, model_et_cm_lr$byClass)) %>%
        mutate(algorithm = "logistic_regression",
               phase = "eighty_twenty")

perf_metrics_rf_et <- data.frame(metric = metric_labels,
                                 metric_result = c(model_et_cm_rf$overall, model_et_cm_rf$byClass)) %>%
        mutate(algorithm = "random_forest",
               phase = "eighty_twenty")

perf_metrics_rt_et <- data.frame(metric = metric_labels,
                                 metric_result = c(model_et_cm_rt$overall, model_et_cm_rt$byClass)) %>%
        mutate(algorithm = "regression_tree",
               phase = "eighty_twenty")

perf_metrics_et_df <- bind_rows(perf_metrics_nb_et, perf_metrics_lr_et, perf_metrics_rf_et,
                                perf_metrics_rt_et)

names(perf_metrics_lr_et) <- c("metric", "metric_result_lr", "algorithm", "phase")
names(perf_metrics_nb_et) <- c("metric", "metric_result_nb", "algorithm", "phase")
names(perf_metrics_rf_et) <- c("metric", "metric_result_rf", "algorithm", "phase")
names(perf_metrics_rt_et) <- c("metric", "metric_result_rt", "algorithm", "phase")

perf_metrics_et_w_df <- merge(x = perf_metrics_lr_et[ , c(1:2)], y = perf_metrics_nb_et[ , c(1:2)],
                              by = "metric", all.x = TRUE, all.y = TRUE)
perf_metrics_et_w_df <- merge(x = perf_metrics_et_w_df, y = perf_metrics_rf_et[ , c(1:2)],
                              by = "metric", all.x = TRUE, all.y = TRUE)
perf_metrics_et_w_df <- merge(x = perf_metrics_et_w_df, y = perf_metrics_rt_et[ , c(1:2)],
                              by = "metric", all.x = TRUE, all.y = TRUE)



# Create Naive Bayes, logistic regression, random forest and regression
# tree models using 10-fold cross-validation
############################################################################

set.seed(1)

# Build the models >> note that we use the full dataset (not the 80/20 split) because we are
# using the cross-validation method
model_cv_nb_df <- train(Attrition ~ ., data = modeling_data_df[, -1], method = "nb",
                        trControl = trainControl("cv", number = 10))
model_cv_lr_df <- train(Attrition ~ ., data = modeling_data_df[, -1], method = "glm", family = "binomial",
                        trControl = trainControl("cv", number = 10))
model_cv_rf_df <- train(Attrition ~ ., data = modeling_data_df[, -1], method = "rf",
                        trControl = trainControl("cv", number = 10))
model_cv_rt_df <- train(Attrition ~ ., data = modeling_data_df[, -1], method = "rpart",
                        trControl = trainControl("cv", number = 10))

# Make predictions
predicted_classes_nb <- model_cv_nb_df %>% predict(modeling_data_df)
predicted_classes_lr <- model_cv_lr_df %>% predict(modeling_data_df)
predicted_classes_rf <- model_cv_rf_df %>% predict(modeling_data_df)
predicted_classes_rt <- model_cv_rt_df %>% predict(modeling_data_df)



# Create confusion matrix for each 10-fold cross-validated model and 
# combine into a single table
############################################################################

# Calculate the confusion matrices
model_cv_cm_nb <- confusionMatrix(predicted_classes_nb, modeling_data_df$Attrition)
model_cv_cm_lr <- confusionMatrix(predicted_classes_lr, modeling_data_df$Attrition)
model_cv_cm_rf <- confusionMatrix(predicted_classes_rf, modeling_data_df$Attrition)
model_cv_cm_rt <- confusionMatrix(predicted_classes_rt, modeling_data_df$Attrition)

cm_nb_t <- data.frame(as.matrix(model_cv_cm_nb$table)) %>%
        mutate(algorithm = "naive_bayes")

cm_lr_t <- data.frame(as.matrix(model_cv_cm_lr$table)) %>%
        mutate(algorithm = "logistic_regression")

cm_rf_t <- data.frame(as.matrix(model_cv_cm_rf$table)) %>%
        mutate(algorithm = "random_forest")

cm_rt_t <- data.frame(as.matrix(model_cv_cm_rt$table)) %>%
        mutate(algorithm = "regression_tree")

confusion_matrix_cv <- bind_rows(cm_nb_t, cm_lr_t, cm_rf_t, cm_rt_t)
names(confusion_matrix_cv) <- c("predicted", "actual", "n_tr", "algorithm")

confusion_matrix_cv <- confusion_matrix_cv %>%
        group_by(algorithm) %>%
        mutate(pct_tr = n_tr / sum(n_tr, na.rm = TRUE)) %>%
        ungroup(algorithm) %>%
        mutate(outcome = ifelse(actual == predicted, "correct", ifelse(actual == "active" & predicted == "termed", "false negative", ifelse(actual == "termed" & predicted == "active", "false positive", "unknown")))) %>%
        dplyr::select(actual, predicted, outcome, n_tr, pct_tr, algorithm) %>%
        arrange(algorithm, outcome, actual, predicted)

confusion_matrix_cv$phase <- "cross_validation"

confusion_matrix_combo <- merge(x = confusion_matrix_et[, c(1:6)], y = confusion_matrix_cv[, c(1:6)], by = c("actual", "predicted", "outcome", "algorithm"), all.x = TRUE, all.y = TRUE)

confusion_matrix_combo <- confusion_matrix_combo %>%
        arrange(algorithm, outcome, actual, predicted)



# Put confusion matrix performance measures into a table to compare across
# the four 10-fold cross-validated models
############################################################################

metric_labels <- row.names(as.data.frame(c(model_cv_cm_nb$overall, model_cv_cm_nb$byClass)))

perf_metrics_nb_cv <- data.frame(metric = metric_labels,
                                 metric_result = c(model_cv_cm_nb$overall, model_cv_cm_nb$byClass)) %>%
        mutate(algorithm = "naive_bayes",
               phase = "cross_validation")

perf_metrics_lr_cv <- data.frame(metric = metric_labels,
                                 metric_result = c(model_cv_cm_lr$overall, model_cv_cm_lr$byClass)) %>%
        mutate(algorithm = "logistic_regression",
               phase = "cross_validation")

perf_metrics_rf_cv <- data.frame(metric = metric_labels,
                                 metric_result = c(model_cv_cm_rf$overall, model_cv_cm_rf$byClass)) %>%
        mutate(algorithm = "random_forest",
               phase = "cross_validation")

perf_metrics_rt_cv <- data.frame(metric = metric_labels,
                                 metric_result = c(model_cv_cm_rt$overall, model_cv_cm_rt$byClass)) %>%
        mutate(algorithm = "regression_tree",
               phase = "cross_validation")

perf_metrics_cv_df <- bind_rows(perf_metrics_nb_cv, perf_metrics_lr_cv, perf_metrics_rf_cv,
                                perf_metrics_rt_cv)

names(perf_metrics_lr_cv) <- c("metric", "metric_result_lr_tr", "algorithm", "phase")
names(perf_metrics_nb_cv) <- c("metric", "metric_result_nb_tr", "algorithm", "phase")
names(perf_metrics_rf_cv) <- c("metric", "metric_result_rf_tr", "algorithm", "phase")
names(perf_metrics_rt_cv) <- c("metric", "metric_result_rt_tr", "algorithm", "phase")

perf_metrics_cv_w_df <- merge(x = perf_metrics_lr_cv[ , c(1:2)], y = perf_metrics_nb_cv[ , c(1:2)],
                              by = "metric", all.x = TRUE, all.y = TRUE)
perf_metrics_cv_w_df <- merge(x = perf_metrics_cv_w_df, y = perf_metrics_rf_cv[ , c(1:2)],
                              by = "metric", all.x = TRUE, all.y = TRUE)
perf_metrics_cv_w_df <- merge(x = perf_metrics_cv_w_df, y = perf_metrics_rt_cv[ , c(1:2)],
                              by = "metric", all.x = TRUE, all.y = TRUE)

perf_metrics_df <- merge(x = perf_metrics_et_w_df, y = perf_metrics_cv_w_df, by = "metric", all.x = TRUE, all.y = TRUE) %>%
        dplyr::select(metric, metric_result_lr, metric_result_lr_tr, metric_result_nb, metric_result_nb_tr, metric_result_rf,
                      metric_result_rf_tr, metric_result_rt, metric_result_rt_tr)



# Feature selection
############################################################################

modeling_data_improve_df <- modeling_data_df %>%
        mutate(AnnualWages = MonthlyIncome * 12) %>%
        dplyr::select(-HourlyRate, -DailyRate, - MonthlyRate, -MonthlyIncome)

set.seed(1)

options(scipen = 999)

fit_rf <- randomForest(Attrition ~ ., data = modeling_data_improve_df[-1])

mod_fit_one <- glm(Attrition ~ ., data = modeling_data_improve_df, family="binomial")

factor_list <- names(modeling_data_improve_df[ , -c(1, 28)])

for(factor_l in factor_list){
        
        tmp_df <- data.frame(factor = regTermTest(mod_fit_one, factor_l)$test.terms,
                             p.value = regTermTest(mod_fit_one, factor_l)$p
        )
        
        if(!exists("factor_sig_df")){
                
                factor_sig_df <- tmp_df 
                
        } else factor_sig_df <- bind_rows(factor_sig_df, tmp_df)
        
}

# Create an importance based on mean decreasing gini
feature_select_df <- as.data.frame(importance(fit_rf))
feature_select_df$factor <-row.names(feature_select_df)
feature_select_df <- feature_select_df %>%
        arrange(desc(MeanDecreaseGini)) %>%
        mutate(importance = round(MeanDecreaseGini, 1)) %>%
        dplyr::select(factor, importance)

feature_select_df <- merge(x = feature_select_df, y = factor_sig_df, by = "factor", all.x = TRUE, all.y = TRUE)

feature_select_df <- feature_select_df %>%
        mutate(significant = ifelse(p.value <= 0.05, "significant (p-value <= 0.05)", "not significant (p-value > 0.05)")) %>%
        arrange(desc(significant), desc(importance), p.value)



# Likelihood ratio
############################################################################

feature_list <- unique(feature_select_df$factor)

# Likelihood ratio test

iter_count <- length(feature_list) - 1

for(i in 1:iter_count){
        
        j <- i+1
        
        mod1_factors <- feature_list[1:i]
        mod2_factors <- feature_list[1:j]
        
        mod1_df <- modeling_data_improve_df %>% dplyr::select(Attrition, mod1_factors)
        mod2_df <- modeling_data_improve_df %>% dplyr::select(Attrition, mod2_factors)
        
        model1_less <- glm(Attrition ~ ., data = mod1_df, family="binomial")
        model2_more <- glm(Attrition ~ ., data = mod2_df, family="binomial")
        
        likelihood_ratio <- anova(model1_less, model2_more, test ="Chisq")
        
        tmp_likelihood_df <- data.frame(
                model1_attributes = paste0(mod1_factors, collapse = ", "),
                model2_attributes = paste0(mod2_factors, collapse = ", "),
                deviance = likelihood_ratio[2, 4],
                p.value = likelihood_ratio[2, 5]
        )
        
        if(!exists("likelihood_df")){
                
                likelihood_df <- tmp_likelihood_df
                
        } else likelihood_df <- bind_rows(likelihood_df, tmp_likelihood_df)
        
}

likelihood_df <- likelihood_df %>%
        mutate(record_id = row_number() + 1)

names(likelihood_df) <- c("fewer_attributes_model", "more_attributes_model", "deviance", "likelihood_p.value", "record_id")



# Pseudo R squared (McFadden's R)
############################################################################

model_count <- length(feature_list)

for(k in 1:model_count){

        mod_factors <- feature_list[1:k]

        mod_df <- modeling_data_improve_df %>% dplyr::select(Attrition, mod_factors)

        model_iter <- glm(Attrition ~ ., data = mod_df, family="binomial")

        mod_output <- pR2(model_iter)

        tmp_pseudo_r_df <- data.frame(
                model_attributes = paste0(mod_factors, collapse = ", "),
                attribute_count = length(mod_factors),
                mcfadden_r = mod_output[4]
        )

        if(!exists("pseudo_r_df")){

               pseudo_r_df <- tmp_pseudo_r_df

        } else pseudo_r_df <- bind_rows(pseudo_r_df, tmp_pseudo_r_df)

}

pseudo_r_df <- pseudo_r_df %>%
        mutate(record_id = row_number())

feature_select_df <- feature_select_df %>%
        mutate(record_id = row_number())

feature_select_df <- merge(x = feature_select_df, y = likelihood_df, by = "record_id", all.x = TRUE, all.y = FALSE)
feature_select_df <- merge(x = feature_select_df, y = pseudo_r_df, by = "record_id", all.x = TRUE, all.y = FALSE)



# Compare performance of 10-fold cross-validated logistic regression
# model without feature selection to the 10-fold cross-validated logistic
# regression model with feature selection
############################################################################

features_selected1_df <- feature_select_df %>%
        filter(significant == "significant (p-value <= 0.05)") %>%
        dplyr::select(-significant)

features_selected_list <- unique(features_selected1_df$factor)

final_model_data_df <- modeling_data_improve_df %>%
        dplyr::select(EmployeeNumber, features_selected_list, Attrition)

set.seed(1)

# Build the models >> note that we use the full dataset (not the 80/20 split) because we are
# using the cross-validation method
model_fs_cv_lr_df <- train(Attrition ~ ., data = final_model_data_df[, -1], method = "glm", family = "binomial",
                           trControl = trainControl("cv", number = 10))

# Make predictions
predicted_classes_fs_cv_lr <- model_fs_cv_lr_df %>% predict(final_model_data_df)

# Calculate the confusion matrices
model_fs_cv_cm_lr <- confusionMatrix(predicted_classes_fs_cv_lr, final_model_data_df$Attrition)

cm_cv_fs_lr <- data.frame(as.matrix(model_fs_cv_cm_lr$table)) %>%
        mutate(algorithm = "logistic_regression")

names(cm_cv_fs_lr) <- c("predicted", "actual", "n_fs", "algorithm")

cm_cv_fs_lr <- cm_cv_fs_lr %>%
        mutate(pct_fs = n_fs / sum(n_fs, na.rm = TRUE)) %>%
        mutate(outcome = ifelse(actual == predicted, "correct", ifelse(actual == "active" & predicted == "termed", "false negative", ifelse(actual == "termed" & predicted == "active", "false positive", "unknown")))) %>%
        dplyr::select(actual, predicted, outcome, n_fs, pct_fs, algorithm) %>%
        arrange(algorithm, outcome, actual, predicted)

cm_cv_fs_lr$phase <- "feature_selected_cross_validation"

cm_cv_fs_print_lr <- cm_cv_fs_lr %>%
        dplyr::select(-algorithm, -phase)

cm_lr_compare <- confusion_matrix_combo %>%
        filter(algorithm == "logistic_regression")

cm_lr_compare <- merge(x = cm_lr_compare, y = cm_cv_fs_lr, by = c("algorithm", "actual", "predicted", "outcome"), all.x = TRUE, all.y = FALSE)

cm_lr_compare <- cm_lr_compare %>%
        dplyr::select(-c(phase, algorithm)) %>%
        arrange(outcome, actual, predicted)



# Create performance statistics for 10-fold cross-validated logistic
# regression with feature selection
############################################################################

metric_fs_labels <- row.names(as.data.frame(c(model_fs_cv_cm_lr$overall, model_fs_cv_cm_lr$byClass)))

perf_metrics_fs_cv_lr <- data.frame(metric = metric_fs_labels,
                                    metric_result = c(model_fs_cv_cm_lr$overall, model_fs_cv_cm_lr$byClass)) %>%
        mutate(algorithm = "logistic_regression",
               phase = "fs_controlled_cross_validation")

names(perf_metrics_fs_cv_lr) <- c("metric", "metric_result_lr_fs", "algorithm", "phase")

perf_metrics_lr <- merge(x = perf_metrics_df[ , c(1:3)], y = perf_metrics_fs_cv_lr[ , c(1:2)], all.x = TRUE, all.y = FALSE)



# Create a C5.0 Decision Tree model and confusion matrix
############################################################################

model_c5_data_df <- modeling_data_improve_df

# Remove StandardHours and Over18 since there is no variation and remove Attrition since it is redundant
model_c5_data_df$Attrition <- as.character(modeling_data_df$Attrition)
model_c5_data_df <- model_c5_data_df %>% 
        dplyr::select(-Attrition, Attrition)

model_c5_data_df$Attrition <- factor(modeling_data_df$Attrition, levels = c("active", "termed"))

set.seed(1)

# Split the data into training and test set
training_c5_samples_df <- model_c5_data_df$EmployeeNumber %>%
        createDataPartition(p = 0.8, list = FALSE)  

train_c5_data_df  <- model_c5_data_df[training_c5_samples_df, ]
test_c5_data_df <- model_c5_data_df[-training_c5_samples_df, ]

error_cost <- matrix(c(0, 8, 8, 0), nrow = 2)

c5_model <- C5.0(train_c5_data_df[-29], train_c5_data_df$Attrition, trials = 10, costs = error_cost)
c5_model_nocost <- C5.0(train_c5_data_df[-29], train_c5_data_df$Attrition, trials = 10)

c5_model_predictions <- predict(c5_model, test_c5_data_df)

c5_cm <- confusionMatrix(c5_model_predictions, test_c5_data_df$Attrition)

c5_cm_tbl <- data.frame(as.matrix(c5_cm$table)) %>%
        mutate(algorithm = "c5_decision_tree")

names(c5_cm_tbl) <- c("predicted", "actual", "n_c5", "algorithm")

c5_cm_tbl <- c5_cm_tbl %>%
        mutate(pct_c5 = n_c5 / sum(n_c5, na.rm = TRUE)) %>%
        mutate(outcome = ifelse(actual == predicted, "correct", ifelse(actual == "active" & predicted == "termed", "false negative", ifelse(actual == "termed" & predicted == "active", "false positive", "unknown")))) %>%
        dplyr::select(actual, predicted, outcome, n_c5, pct_c5, algorithm) %>%
        arrange(algorithm, outcome, actual, predicted)

c5_cm_tbl$phase <- "cost_controlled_adaptive_boost"

c5_cm_tbl_print <- c5_cm_tbl %>%
        dplyr::select(-c(phase, algorithm))



# Create C5.0 Decision Tree model performance metrics
############################################################################

metric_c5_labels <- row.names(as.data.frame(c(c5_cm$overall, c5_cm$byClass)))

c5_perf_measures <- data.frame(metric = metric_c5_labels,
                               metric_result = c(c5_cm$overall, c5_cm$byClass)) %>%
        mutate(algorithm = "C5.0 Decision Tree",
               phase = "Cost Controlled Adaptive Boost")

names(c5_perf_measures) <- c("metric", "metric_result_c5", "algorithm", "phase")

c5_perf_measures_print <- c5_perf_measures %>%
        dplyr::select(-c(algorithm, phase))



# Generate data for ROC curve comparisons
############################################################################

predicted_probs_nb <- as.data.frame(model_nb %>% predict(test_data_df, type = "response"))
predicted_probs_lr <- model_lr %>% predict(test_data_df, type = "response")
predicted_probs_rf <- model_rf %>% predict(test_data_df, type = "prob")
predicted_probs_rt <- model_rt %>% predict(test_data_df, type = "prob")
predicted_probs_nb_cv <- as.data.frame(model_cv_nb_df %>% predict(modeling_data_df, type = "prob"))
predicted_probs_lr_cv <- model_cv_lr_df %>% predict(modeling_data_df, type = "prob")
predicted_probs_rf_cv <- model_cv_rf_df %>% predict(modeling_data_df, type = "prob")
predicted_probs_rt_cv <- model_cv_rt_df %>% predict(modeling_data_df, type = "prob")
predicted_probs_fs_cv_lr <- model_fs_cv_lr_df %>% predict(final_model_data_df, type = "prob")
c5_model_predictions_probs <- predict(c5_model_nocost, test_c5_data_df, type = "prob")

pred_nb_80 <- prediction(predictions = predicted_probs_nb$posterior.termed, labels = test_data_df$Attrition)
pred_lr_80 <- prediction(predictions =  predicted_probs_lr, labels = test_data_df$Attrition)
pred_rf_80 <- prediction(predictions =  predicted_probs_rf[ , 2], labels = test_data_df$Attrition)
pred_rt_80 <- prediction(predictions =  predicted_probs_rt[ , 2], labels = test_data_df$Attrition)
pred_nb_cv <- prediction(predictions = predicted_probs_nb_cv[ , 2], labels = modeling_data_df$Attrition)
pred_lr_cv <- prediction(predictions =  predicted_probs_lr_cv$termed, labels = modeling_data_df$Attrition)
pred_rf_cv <- prediction(predictions =  predicted_probs_rf_cv$termed, labels = modeling_data_df$Attrition)
pred_rt_cv <- prediction(predictions =  predicted_probs_rt_cv$termed, labels = modeling_data_df$Attrition)
pred_lr_fs_cv <- prediction(predictions =  predicted_probs_fs_cv_lr$termed, labels = final_model_data_df$Attrition)
pred_c5_ab <- prediction(predictions =  c5_model_predictions_probs[ , 2], labels = test_c5_data_df$Attrition)



# 
############################################################################




# 
############################################################################





# 
############################################################################















