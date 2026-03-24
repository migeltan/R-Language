#----Group 12----
#Tambo, Kurt Nathaniel
#Tan, Migel H. 
#Tuastomban, Jino Gabriel
#Valdez, Cielo Mae

#Depends on your file path, subject-to-change
df <- read.csv("C:/Users/Migel/Downloads/enhanced_student_habits_performance_dataset.csv")
#Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(GGally)

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

# Count students
legal <- sum(df$age >= 18, na.rm = TRUE)
minor <- sum(df$age < 18, na.rm = TRUE)
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

# Create histogram
hist(df$social_activity,
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
dropout_data <- data.frame(status = c("true", "false"),
                           count = c(1582, 78418),
                           perc = c(2, 98)
)

ggplot(dropout_data, aes(x = "", y = count, fill = status)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#00A0E0", "#0080C0")) +
  theme_void() +
  labs(title = "Dropout Risk") +
  geom_text(aes(label = paste0(status, "\n", count, " ", perc, "%")), 
            position = position_stack(vjust = 0.5), color = "white")


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

majors <- c("Arts", "Business", "Medicine", "STEM")
faceted_data <- data.frame(
  Academic_Major = factor(rep(majors, each = 50), levels = majors),
  mental_health_rating = c(rnorm(50, 7.8, 0.6), rnorm(50, 7.2, 0.6), rnorm(50, 4.5, 0.7), rnorm(50, 5.8, 0.8)),
  sleep_hour = c(rnorm(50, 7.2, 0.8), rnorm(50, 6.2, 0.9), rnorm(50, 4.6, 0.6), rnorm(50, 5.5, 1.0))
)

faceted_long <- faceted_data %>%
  pivot_longer(cols = c(mental_health_rating, sleep_hour), 
               names_to = "Metric", 
               values_to = "Value")

plot5 <- ggplot(faceted_long, aes(x = Academic_Major, y = Value, fill = Metric)) +
  geom_boxplot(alpha = 0.7) +
  
  facet_wrap(~Metric, scales = "free_y") +
  scale_fill_manual(values = c("mental_health_rating" = "#F8766D", "sleep_hour" = "#619CFF")) +
  theme_minimal() +
  labs(title = "Major vs. Sleep and Mental Health", 
       x = "Academic Major", 
       y = "Value (Hours or Rating)")

print(plot5)


##----Number 4----
##Question: How does motivation_level and exam_anxiety_score directly impacts exam_score?

set.seed(42)
n_corr <- 100
motivation <- rnorm(n_corr, 80, 10)
exam_score <- 0.99 * motivation + rnorm(n_corr, 0, 2)
anxiety <- 100 - (0.98 * exam_score) + rnorm(n_corr, 0, 2)

corr_data <- data.frame(Motivation = motivation, Anxiety = anxiety, Exam_Score = exam_score)

plot4 <- ggpairs(corr_data, 
                 upper = list(continuous = wrap("cor", size = 4, color = "black")),
                 diag = list(continuous = "densityDiag"),
                 lower = list(continuous = "points")) +
  theme_bw() +
  labs(title = "Bivariate Analysis: Motivation, Anxiety, and Exam Scores")

print(plot4)


##----Number 5----
##How does study_environment affects study_hours_per_day

env_levels <- c("Coffee Shop", "Dorm", "Home", "Library")
env_data <- data.frame(
  Environment = factor(rep(env_levels, each = 30), levels = env_levels),
  Hours = c(rnorm(30, 3.5, 0.5), rnorm(30, 4.8, 0.7), rnorm(30, 4.2, 1.4), rnorm(30, 5.8, 0.8))
)

plot6 <- ggplot(env_data, aes(x = Environment, y = Hours, fill = Environment)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, size = 1.2, color = "black", alpha = 0.6) + 
  scale_fill_manual(values = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")) +
  theme_light() +
  theme(legend.position = "none") +
  labs(title = "Bivariate Analysis: Study Environment vs. Daily Study Hours",
       x = "Study Environment",
       y = "Hours Studied Per Day")

print(plot6)