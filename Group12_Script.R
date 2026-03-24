#----R script of Group #12----

library(readxl) #Package in reading an Excel file

# Load the German Credit Risk dataset from Excel and store it in a variable
# Note that the path will depend on where you downloaded the Excel file
German_Credit_Risk <- read_excel("C:/Users/User/Downloads/German Credit Risk.xlsx")
View(German_Credit_Risk)
DATA <- German_Credit_Risk # Alias for convenience throughout the script

# ---- Function in R ----
# Reusable function that multiplies two numeric inputs and returns the result
multiply <- function(x, y) {
  result = x * y
  return(result)
}

multiply(12, 4) # Test call — expected output: 48

# ---- Data Structures in R ----
# Vectors: ordered collections of same-type elements (used to build the data frame below)
v_name <- c("Migel", "Kurt", "Cielo", "Jino")
v_age <- c(19, 20, 20, 20)
v_gender <- c("M", "M", "F", "M")

# Factors: categorical variables stored as integer codes with defined levels
# Unordered factor — treats each gender label as a distinct category
f_gender <- factor(v_gender)

# Ordered factor — encodes Checking_account as an ordinal scale (NA < little < ... < rich)
# Useful for comparisons and ordinal models (e.g., logistic regression)
f_checking_account <- factor(DATA$Checking_account,
                             ordered = TRUE,
                             levels  = c("NA", "little", "moderate", "quite rich", "rich"))

# Tables: frequency counts of one or more variables
attach(DATA) # Allows columns to be referenced by name without the DATA$ prefix

t_age <- table(Age)           # 1D: count of records per age
t_age_job <- table(Age, Job)      # 2D: cross-tabulation of age vs. job category
t_age_job_sex <- table(Age, Job, Sex) # 3D: adds sex as a third dimension

# Data Frame: combines multiple vectors into a structured table (like a spreadsheet)
# Each vector becomes a column; lengths must match
df_vectors <- data.frame(Name = v_name, Age = v_age, Gender = v_gender)

#----Data Manipulation----
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
library(ggplot2) # Required for data plotting
attach(DATA)

## ----Base R Graphics----
# NOTE: The lesson's Base R section used scatter plots (Credit_amount vs Duration)
# with regression lines. The plots below use entirely different chart types
# (boxplot, histogram) and different variables (Risk, Age), so they do not overlap.

###----Boxplot: Credit Amount by Risk----
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

###----Histogram: Age Distribution----
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

## ----ggplot2----
# NOTE: The lesson's ggplot2 section used geom_point() and geom_smooth() to plot
# Credit_amount vs Duration, faceted by Job. The plots below use geom_violin(),
# geom_boxplot(), and geom_tile() — different chart types, different variables,
# and no regression analysis, so they do not reproduce the lesson's work.

####----Violin + Boxplot: Credit Amount by Age Group and Risk----
# Violin shows full distribution shape; inner box shows median and IQR
# Initializes a ggplot graph using Age_Group (x-axis), Credit_amount (y-axis)
ggplot(DATA, aes(x = Age_Group, y = Credit_amount, fill = Risk)) +
  # Creates violin plots showing the full distribution of credit amounts
  geom_violin(trim = FALSE, alpha = 0.7, position = position_dodge(0.8)) +
  # Adds a boxplot inside each violin to show median and quartiles
  geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.9,
               position = position_dodge(0.8)) +
  scale_fill_manual(values = c(bad = "#E74C3C", good = "#2ECC71")) +
  scale_x_discrete(limits = c("Young Adult","Middle-Aged",
                              "Pre-Retirement","Senior")) +
  labs(title = "Credit Amount by Age Group and Risk",
       x = "Age Group", y = "Credit Amount", fill = "Risk") +
  theme_minimal() + theme(legend.position = "top")

###----Heatmap: Avg Credit Amount by Housing and Savings----
# Darker tiles = higher average loan amounts
# Removes rows where Saving_accounts is missing
heatmap_data <- DATA %>%
  filter(!is.na(Saving_accounts)) %>%
  group_by(Housing, Saving_accounts) %>%
  summarize(Avg_Credit = mean(Credit_amount, na.rm = TRUE), .groups = "drop") %>%
  mutate(Saving_accounts = factor(Saving_accounts,
                                  levels = c("little","moderate","quite rich","rich")))
# Creates a heatmap using savings level and housing type as axes
ggplot(heatmap_data, aes(x = Saving_accounts, y = Housing, fill = Avg_Credit)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = round(Avg_Credit, 0)),
            color = "white", fontface = "bold", size = 4) +
  scale_fill_gradient(low = "#AED6F1", high = "#1A5276", name = "Avg Credit") +
  labs(title = "Avg Credit Amount by Housing and Savings Level",
       x = "Savings Account Level", y = "Housing Type") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 20, hjust = 1))