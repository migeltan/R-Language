#----R script of Group #12----
library(readxl)
#German_Credit_Risk <- read_excel("C:/Users/Admin/Downloads/German Credit Risk.xlsx", 
#                                 sheet = "Sheet1")
#View(German_Credit_Risk)
German_Credit_Risk <- read_excel("C:/Users/Migel/Downloads/German Credit Risk.xlsx")
View(German_Credit_Risk)
DATA <- German_Credit_Risk #Assign dataset to DATA variable

#----Function in R----
# Function (Multiplication)
multiply <- function (x,y) {
  result = x * y
return (result)
}

multiply (12,4)

#----Data Structures in R----
# Vector 
v_name <- c("Migel", "Kurt", "Cielo", "Jino")
v_age <- c(19, 20, 20, 20)
v_gender <- c("M", "M", "F", "M")

# Factor 
f_gender <- factor(v_gender)
f_checking_account <- factor(DATA$Checking_account, ordered = T,levels = c("NA", "little", "moderate", "quite rich", "rich"))


# Tables
attach(DATA)
t_age = table (Age)
t_age_job = table(Age, Job)
t_age_job_sex = table (Age, Job, Sex)

# Data Frame 
df_vectors = data.frame(Name = v_name, Age = v_age, Gender = v_gender)

#----Data Manipulation----
German_Credit_Risk <- read_excel("C:/Users/Migel/Downloads/German Credit Risk.xlsx")
View(German_Credit_Risk)
DATA <- German_Credit_Risk #Assign dataset to DATA variable
library(dplyr) #required for data manipulation

# arrange() - sorts rows in ascending or descending order
# Syntax: <DATA> %>% arrange(desc(<column>))
NEWDATA <- DATA %>% arrange(desc(Credit_amount))

# rename() - renames column headers
# Syntax: <DATA> %>% rename(<new_name> = <old_name>)
NEWDATA <- NEWDATA %>% rename(Loan_Duration = Duration)

# distinct() - returns only unique row combinations
# Syntax: <DATA> %>% distinct(<col1>, <col2>)
NEWDATA <- NEWDATA %>% distinct(Housing, Job, .keep_all = TRUE)

# count() - tallies rows per group; shortcut for group_by() + summarize(n())
# Syntax: <DATA> %>% count(<column>, sort = TRUE)
risk_count <- DATA %>% count(Risk, sort = TRUE)

# slice_max() / slice_min() - extracts top or bottom n rows by a column value
# Syntax: <DATA> %>% slice_max(<column>, n = <number>)
top5_loans <- DATA %>% slice_max(Credit_amount, n = 5)
bottom5_loans <- DATA %>% slice_min(Credit_amount, n = 5)

# case_when() inside mutate() - multi-condition classification (extended ifelse)
# Syntax: mutate(<new_col> = case_when(<cond> ~ <value>, TRUE ~ <default>))
DATA <- DATA %>%
  mutate(Age_Group = case_when(
    Age < 25             ~ "Young Adult",
    Age >= 25 & Age < 45 ~ "Middle-Aged",
    Age >= 45 & Age < 60 ~ "Pre-Retirement",
    TRUE                 ~ "Senior"
  ))

# group_by() + mutate() - adds group-level stats while keeping all rows
# (unlike group_by + summarize which collapses rows)
DATA <- DATA %>%
  group_by(Housing) %>%
  mutate(Avg_Credit_By_Housing = mean(Credit_amount, na.rm = TRUE)) %>%
  ungroup()

# Advanced summarize() - multiple summary stats grouped by Risk and Sex
detailed_summary <- DATA %>%
  group_by(Risk, Sex) %>%
  summarize(Count        = n(),
            Avg_Credit   = mean(Credit_amount, na.rm = TRUE),
            Median_Credit = median(Credit_amount, na.rm = TRUE),
            Max_Credit   = max(Credit_amount, na.rm = TRUE),
            .groups = "drop")


#----Data Visualization----
German_Credit_Risk <- read_excel("C:/Users/Migel/Downloads/German Credit Risk.xlsx")
View(German_Credit_Risk)
DATA <- German_Credit_Risk #Assign dataset to DATA variable
library(ggplot2) #required for data plotting

attach(DATA)

# ---------------------------------------
# Boxplot: Credit Amount by Risk Category
# Shows median, spread, and outliers for each risk level
boxplot(Credit_amount ~ Risk, data = DATA,
        col = c("#E74C3C", "#2ECC71"),
        main = "Credit Amount by Risk Category",
        xlab = "Credit Risk", ylab = "Credit Amount",
        notch = TRUE, outline = FALSE)
# Creates a boxplot comparing credit amounts between different risk categories

# Adds a horizontal line showing the overall average credit amount
abline(h = mean(DATA$Credit_amount, na.rm = TRUE),
       col = "navy", lty = 2, lwd = 2)

# Adds a legend explaining the mean reference line
legend("topright", legend = "Overall Mean",
       col = "navy", lty = 2, lwd = 2, bty = "n")

# Histogram with Density Curve: Age Distribution
# Reveals the age profile of applicants with mean and median reference lines
hist(DATA$Age, breaks = 20, freq = FALSE,
     col = "#AED6F1", border = "white",
     main = "Age Distribution of Applicants",
     xlab = "Age (Years)", ylab = "Density")
# Creates a histogram showing how applicant ages are distributed

lines(density(DATA$Age, na.rm = TRUE), col = "darkblue", lwd = 2)
abline(v = mean(DATA$Age,   na.rm = TRUE), col = "red",    lwd = 2, lty = 2)
abline(v = median(DATA$Age, na.rm = TRUE), col = "orange", lwd = 2, lty = 3)

# Displays a legend explaining the density curve, mean, and median lines
legend("topright", legend = c("Density", "Mean", "Median"),
       col = c("darkblue", "red", "orange"), lty = c(1,2,3), bty = "n")

# ---- ggplot2 ----
# Violin + Boxplot: Credit Amount by Age Group and Risk
# Violin shows full distribution shape; inner box shows median and IQR

# Initializes a ggplot graph using Age_Group (x-axis), Credit_amount (y-axis),
ggplot(DATA, aes(x = Age_Group, y = Credit_amount, fill = Risk)) +

# Creates violin plots showing the full distribution of credit amounts 
  geom_violin(trim = FALSE, alpha = 0.7, position = position_dodge(0.8)) +

# Adds a boxplot inside each violin plot to show median and quartiles
  geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.9,
               position = position_dodge(0.8)) +
  scale_fill_manual(values = c(bad = "#E74C3C", good = "#2ECC71")) +
  scale_x_discrete(limits = c("Young Adult","Middle-Aged",
                              "Pre-Retirement","Senior")) +
  labs(title = "Credit Amount by Age Group and Risk",
       x = "Age Group", y = "Credit Amount", fill = "Risk") +
  theme_minimal() + theme(legend.position = "top")

# ---------------------------------------
# Heatmap: Average Credit Amount by Housing Type and Savings Level
# Darker tiles = higher average loan amounts

# Removes rows where Saving_accounts is missing
heatmap_data <- DATA %>%
  filter(!is.na(Saving_accounts)) %>%
  group_by(Housing, Saving_accounts) %>%
  summarize(Avg_Credit = mean(Credit_amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(Saving_accounts = factor(Saving_accounts,
                                  levels = c("little","moderate","quite rich","rich")))

# Creates a heatmap structure using savings level and housing type
ggplot(heatmap_data, aes(x = Saving_accounts, y = Housing, fill = Avg_Credit)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = round(Avg_Credit, 0)),
            color = "white", fontface = "bold", size = 4) +
  scale_fill_gradient(low = "#AED6F1", high = "#1A5276", name = "Avg Credit") +
  labs(title = "Avg Credit Amount by Housing and Savings Level",
       x = "Savings Account Level", y = "Housing Type") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 20, hjust = 1))

