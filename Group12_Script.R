#----R script of Group #12----
library(readxl)
German_Credit_Risk <- read_excel("C:/Users/Admin/Downloads/German Credit Risk.xlsx", 
                                 sheet = "Sheet1")
View(German_Credit_Risk)

# Function (Multiplication)
multiply <- function (x,y) {
  result = x * y
return (result)
}

multiply (12,4)

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

#----Data Visualization----
