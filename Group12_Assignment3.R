#----Group 12----
#Tambo, Kurt Nathaniel
#Tan, Migel H. 
#Tuastomban, Jino Gabriel
#Valdez, Cielo Mae

#Depends on your file path, subject-to-change
df <- read.csv("C:/Users/Migel/Downloads/enhanced_student_habits_performance_dataset.csv")

#----UNIVARIATE ANALYSIS----
##----Number 1----
##Question: What is the average number of study_hours per day among students?
mean(df$study_hours)
median(df$study_hours)

hist(df$study_hours,
     main="Study Hours Distribution",
     xlab="Study Hours",
     ylab="Frequency")

##----Number 2----
##Question: What is the distribution of students based on gender?
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

##----Number 3----
##Question: How many students are at the legal age (18+)?

# Load data
data <- read.csv("C:/Users/valde/OneDrive/Desktop/SPR/Enhanced Student Habits Performace Dataset.csv")
# Count students
legal <- sum(data$age >= 18, na.rm = TRUE)
minor <- sum(data$age < 18, na.rm = TRUE)
# Combine counts
values <- c(legal, minor)
categories <- c("18 and above", "Below 18")
# Add percentages
percentages <- round(values / sum(values) * 100)
labels <- paste(categories, percentages, "%")
# Create pie chart
pie(values,
    labels = labels,
    main = "Age (18+ vs <18)",
    col = c("skyblue", "darkblue"))

##----Number 4----
##Question: What's the average number of weekly social activities attended by 80,000 students?

# Load data
data <- read.csv("C:/Users/valde/OneDrive/Desktop/SPR/Enhanced Student Habits Performace Dataset.csv")
# Create histogram
hist(data$social_activity,
     main   = "Weekly Social Activities Attended by Students",
     xlab   = "Social Activities per Week",
     ylab   = "Number of Students",
     col    = "skyblue",
     border = "darkblue",
     breaks = seq(-0.5, 5.5, by = 1),
     xaxt   = "n")
# Add x-axis with exact labels 0-5
axis(1, at = 0:5)

##----Number 5----
##Question: How many students are likely to drop out?


#----BIVARIATE ANALYSIS----
##----Number 1----
##Question: How do motivation level and exam anxiety score influence students' exam scores?
# Correlation
cor(df$motivation_level, df$exam_score)
cor(df$exam_anxiety_score, df$exam_score)

# Scatterplots
plot(df$motivation_level, df$exam_score,
     main="Motivation Level vs Exam Score",
     xlab="Motivation Level",
     ylab="Exam Score")

plot(df$exam_anxiety_score, df$exam_score,
     main="Exam Anxiety vs Exam Score",
     xlab="Exam Anxiety Score",
     ylab="Exam Score")

##----Number 2----
##Question: How do social media hours, Netflix hours, and total screen time affect students' time management scores?
# Correlations
cor(df$social_media_hours, df$time_management_score)
cor(df$netflix_hours, df$time_management_score)
cor(df$screen_time, df$time_management_score)
# Scatterplots
plot(df$social_media_hours, df$time_management_score,
     main="Social Media Hours vs Time Management",
     xlab="Social Media Hours",
     ylab="Time Management Score")
plot(df$netflix_hours, df$time_management_score,
     main="Netflix Hours vs Time Management",
     xlab="Netflix Hours",
     ylab="Time Management Score")
plot(df$screen_time, df$time_management_score,
     main="Screen Time vs Time Management",
     xlab="Screen Time",
     ylab="Time Management Score")

##----Number 3----
##Question: How does major relates to sleep_hour and mental_health_rating?

##----Number 4----
##Question: How does motivation_level and exam_anxiety_score directly impacts exam_score?

##----Number 5----
##How does study_environment affects study_hours_per_day
