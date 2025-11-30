set.seed(2025)

n <- 100 # Number of communications

# Variables
users <- sample(c("Visually Impaired", "Hearing Impaired"), n, replace=TRUE)
gestures <- sample(paste("Gesture", LETTERS[1:5]), n, replace=TRUE) # 5 gesture types
contexts <- sample(c("Quiet room", "Crowded public", "Outdoor", "Emergency"), n, replace=TRUE)
difficulty_level <- sample(1:5, n, replace=TRUE) # 1=Easy, 5=Hard
response_time <- round(rnorm(n, mean=4 + difficulty_level/2, sd=1.2),2) # seconds
accuracy <- rbinom(n, 1, prob=1 - (difficulty_level * 0.10 + ifelse(contexts=="Emergency", 0.2, 0))) # binary accuracy

# Dataframe
df <- data.frame(User=users,
                 Gesture=gestures,
                 Context=contexts,
                 Difficulty=difficulty_level,
                 ResponseTime=response_time,
                 Accuracy=accuracy)

# Data summary
library(dplyr)
summary_stats <- df %>%
  group_by(User, Context) %>%
  summarise(AvgRespTime = mean(ResponseTime),
            AccuracyRate = mean(Accuracy),
            AvgDifficulty = mean(Difficulty),
            .groups = "drop")

print("Summary Statistics by User and Context:")
print(summary_stats)

# Visualization
library(ggplot2)
# Boxplot: Response Time by Context
ggplot(df, aes(x=Context, y=ResponseTime, fill=User)) +
  geom_boxplot() +
  labs(title="Response Time by Context and User Group")

# Bar plot: Accuracy Rate by Difficulty Level
ggplot(df, aes(x=factor(Difficulty), fill=factor(Accuracy))) +
  geom_bar(position="fill") +
  labs(title="Accuracy Rate by Difficulty Level", x="Difficulty Level", y="Proportion") +
  scale_y_continuous(labels=scales::percent)

# Gesture-wise accuracy
gesture_acc <- df %>%
  group_by(Gesture) %>%
  summarise(GestureAccuracy = mean(Accuracy))

print("Gesture-wise Accuracy:")
print(gesture_acc)