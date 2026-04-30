# Load required packages
library(readxl)
library(ggplot2)
library(stargazer)

# Load dataset
Train_Tracking <- read_excel("data/Train Tracking.xlsx")

# Create summary statistics table for Hours (dependent variable)
# and Volume (independent variable)
summary_table <- data.frame(
        Variable = c("Hours", "Volume (BBLS)"),
        Mean = c(mean(Train_Tracking$Hours),
                 mean(Train_Tracking$`Volume (BBLS)`)),
        Median = c(median(Train_Tracking$Hours),
                   median(Train_Tracking$`Volume (BBLS)`)),
        Variance = c(var(Train_Tracking$Hours),
                     var(Train_Tracking$`Volume (BBLS)`)),
        SD = c(sd(Train_Tracking$Hours),
               sd(Train_Tracking$`Volume (BBLS)`))
)

# Display summary statistics table
stargazer(summary_table,
          type = "text",
          summary = FALSE,
          title = "Summary Statistics",
          digits = 2,
          rownames = FALSE)

# Histogram of Hours (load time)
ggplot(Train_Tracking, aes(x = Hours)) +
        geom_histogram(bins = 10, fill = "lightblue", color = "white") +
        labs(
                title = "Distribution of Load Time (Hours)",
                x = "Hours",
                y = "Frequency"
        )

# Histogram of Volume (BBLS)
ggplot(Train_Tracking, aes(x = `Volume (BBLS)`)) +
        geom_histogram(bins = 10, fill = "lightgreen", color = "white") +
        labs(
                title = "Distribution of Shipment Volume (BBLS)",
                x = "Volume (BBLS)",
                y = "Frequency"
        )

# Scatterplot of Volume vs Hours with regression line
ggplot(Train_Tracking, aes(x = `Volume (BBLS)`, y = Hours)) +
        geom_point(color = "steelblue") +
        geom_smooth(method = "lm", se = FALSE, color = "darkred") +
        labs(
                title = "Volume vs Load Time",
                x = "Volume (BBLS)",
                y = "Hours"
        )

# Create linear regression model (Hours as a function of Volume)
model <- lm(Hours ~ `Volume (BBLS)`, data = Train_Tracking)

# Create table for correlation and covariance
corr_table <- data.frame(
        Measure = c("Correlation", "Covariance"),
        Value = c(
                cor(Train_Tracking$`Volume (BBLS)`, Train_Tracking$Hours),
                cov(Train_Tracking$`Volume (BBLS)`, Train_Tracking$Hours)
        )
)

# Display correlation and covariance table
stargazer(corr_table,
          type = "text",
          summary = FALSE,
          title = "Correlation and Covariance",
          digits = 4,
          rownames = FALSE)

# Display regression results using stargazer
stargazer(model,
          type = "text",
          title = "Regression Results: Volume vs Load Time",
          dep.var.labels = "Hours",
          covariate.labels = "Volume (BBLS)",
          digits = 4)
