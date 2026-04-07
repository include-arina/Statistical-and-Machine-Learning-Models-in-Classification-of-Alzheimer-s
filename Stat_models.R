library(ggplot2)
data <- read.csv("AD_data.csv")

#-----------------------------Plots for data visualization--------------------------

#plot of medical conditions distribution
med_history <- function(AD_data) {
  medical_cols <- c("FamilyHistoryAlzheimers", "CardiovascularDisease", 
                    "Diabetes", "Depression", "HeadInjury", "Hypertension")
  
  counts <- colSums(AD_data[ , medical_cols])
  
  medical_data <- data.frame(
    Condition = names(counts),
    Count = as.numeric(counts)
  )
  
  ggplot(medical_data, aes(x = Condition, y = Count)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "gray40", linewidth = 0.5) +
    labs(title = "Prevalence of Medical Conditions",
         x = "Medical Condition",
         y = "Number of Patients") +
    scale_y_continuous(expand = c(0, 0)) +    
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x  = element_text(angle = 45, 
                                  hjust = 1),
      axis.line    = element_line(color = "black", linewidth = 0.6),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_line(color = "grey90"),
      panel.border = element_blank()
    )
  
}

med_history(data)

#barplot of gender distribution
gender_dist <- function(AD_data) {
  AD_data$Gender <- factor(AD_data$Gender,
                        levels = c(0, 1),
                        labels = c("Male", "Female"))
  
  barplot(table(AD_data$Gender),
          col = c("skyblue", "pink"),
          main = "Gender Distribution",
          xlab = "Gender",
          ylab = "Count",
          border = "gray40")
  grid()
  abline(h = 0, lwd = 1) 
} 
gender_dist(data)

#ethnicity distribution barplot 
ethnicity_dist <- function(AD_data){
  AD_data$Ethnicity <- factor(AD_data$Ethnicity,
                           levels = c(0, 1, 2, 3),
                           labels = c("Caucasian", "African American", "Asian", "Other"))
  ethnic_data <- as.data.frame(table(AD_data$Ethnicity))
  colnames(ethnic_data) <- c("Ethnicity", "Count")
  
  ggplot(ethnic_data, aes(x = Ethnicity, y = Count)) +
    geom_bar(stat = "identity", fill = "salmon", color = "gray40", linewidth = 0.5) +
    labs(title = "Ethnicity Distribution",
         x = "Ethnic Group",
         y = "Number of Patients") +
    scale_y_continuous(expand = c(0, 0)) +   
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5), 
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.line = element_line(color = "black", linewidth = 0.6),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_line(color = "grey90"),
      panel.border = element_blank()
    )
}
ethnicity_dist(data)

#boxplot of age vs. gender
age_gender <- function(AD_data){
  AD_data$Gender <- factor(AD_data$Gender,
                        levels = c(0, 1),
                        labels = c("Male", "Female"))
  
  boxplot(AD_data$Age ~ AD_data$Gender, data = AD_data,
          col = c("skyblue", "pink"),
          main = "Age Distribution by Gender",
          xlab = "Gender",
          ylab = "Age",
          border = "gray40")
  grid()
}
age_gender(data)

#MMSE vs. Diagnosis boxplots
mmse_diag <- function(AD_data){
  AD_data$Diagnosis <- factor(AD_data$Diagnosis,
                           levels = c(0, 1),
                           labels = c("No AD", "AD"))
  
  boxplot(AD_data$MMSE~AD_data$Diagnosis, 
          main = "MMSE Scores vs. Diagnosis", 
          xlab = "AD Diagnosis", 
          ylab = "MMSE Scores", 
          col = c("lightgreen","salmon"))
}
mmse_diag(data)

#diagnosis distribution
diagnosis <- function(AD_data) {
  AD_data$Diagnosis <- factor(AD_data$Diagnosis,
                           levels = c(0, 1),
                           labels = c("No AD", "AD"))
  barplot(table(AD_data$Diagnosis),
          col = c("yellow", "salmon"),
          main = "Distribution of Alzheimer's Diagnosis",
          xlab = "Diagnosis",
          ylab = "Count",
          ylim = c(0,1500),
          border = "gray40")
  grid()
  abline(h = 0, lwd = 1)
}
diagnosis(data)

#-----------------------------Statistical Models-----------------------------------

#multiple linear regression
linear_reg <- lm(MMSE ~ Age + SystolicBP + BMI + EducationLevel, data = data)
summary(linear_reg)
plot(linear_reg)


#baseline logistic regression 
baseline <- glm(Diagnosis ~ Age + MMSE + ADL + FunctionalAssessment, data = data, family = binomial) #binomial = binary outcome
summary(baseline)
plot(baseline)




