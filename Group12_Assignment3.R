

df <- read.csv("C:/Users/Migel/Downloads/enhanced_student_habits_performance_dataset.csv")

mean(df$study_hours)
median(df$study_hours)

hist(df$study_hours,
     main="Study Hours Distribution",
     xlab="Study Hours",
     ylab="Frequency")

gender_counts <- table(df$gender)

barplot(gender_counts,
        main = "Gender Distribution of Students",
        xlab = "Gender",
        ylab = "Number of Students",
        col = c("skyblue", "pink", "lightgreen"),
        border = "black")

# Add values on top of bars
text(x = seq_along(gender_counts),
     y = gender_counts,
     label = gender_counts,
     pos = 3)