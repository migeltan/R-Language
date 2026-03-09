#----Data Structure in R----
library(readxl)
German_Credit_Risk <- read_excel("C:/Users/Migel/Downloads/German Credit Risk.xlsx")
View(German_Credit_Risk)
DATA <- German_Credit_Risk #Assign dataset to DATA variable

#Organizing and storing data in R
#Vector, Factor, Table, DataFrame

#Vector - 1 Dimensional array, uses c()
#Syntax: c(element n, ...)
v_name <- c("Migel", "Herrera", "Tan", "Amiel", "Andrei") #same data type only
v_age <- c(18, 21, 20, 19, 22) #numeric
v_gender <- c("F", "M", "F", "M", "F")
#Extracted from a dataframe: uses $
#Syntax: <dataset>$<column_name>
age <- DATA$Age #vector extracted from excel

#Factor - Represents categorical data, ordering (low, med, high)
#Syntax: factor(<vector>, levels = c(<order of labels>), ordered = TRUE/FALSE)
#gender <- factor(c("Male", "Female", "Male")) #Unordered
#education <- factor(c("High School", "College", "PhD", "College"),
#                   levels = c("High School", "College", "PhD", ordered = T))
#Binabasa na as categorial ordered data 
f_gender <- factor(v_gender)
f_savings_account <- factor(DATA$Saving_accounts, ordered = T, levels = c("NA", "little", "moderate", "quite rich", "rich"))

#Table - store counts of frequencies or categorical data.
#Syntax: table(x), table(x, y), table(x, y, z)
#create vector 
colors <- c("red", "blue", "red", "green", "blue", "red")
#generate frequency table
tab <- table(colors)
print(tab)

attach(DATA) #access vectors
#frequency table
t_job <- table(Job) #one variable table
t_sex_job <- table(Sex, Job)
t_sex_job_savings <- table(Sex, Job, Saving_accounts)

#Dataframe - is a 2d structure, consists of vectors
#Syntax: my_df data.frame(column1 = <vector>, col2 = <vector>, ...)
df_vectors <- data.frame(Name = v_name, Age = v_age, Gender = v_gender)
detach(df_vectors)
