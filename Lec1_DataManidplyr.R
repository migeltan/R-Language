#----Data Manipulation using dplry Package----
#Importing German Credit Risk Data set
library(readxl)
library(dplyr)
German_Credit_Risk <- read_excel("C:/Users/Migel/Downloads/German Credit Risk.xlsx")
View(German_Credit_Risk)
DATA <- German_Credit_Risk #Assign dataset to DATA variable

#dplry - data manipulation, filtering, arranging, mutating, summarizing, and joinging data
#Syntax: pipe chaining operator (%>%)
#data %>% filter(condition) %>% select(columns) %>% arrange(order)
#ctrl + shift + m
#avoids nested function calls

#common function: filter()
#filter() #extract rows that meet conditions
#Syntax: <DATA> %>% filter<condition> & ... &<condition>)
NEWDATA <- DATA %>% filter(Sex == "male" & Job >= 2)

#Logical Comparison
# <, >, <=, >=, ==, !=, &- multiple conditions

#common function: select()
#select() chooses specific columns from a dataset
#Syntax: <DATA> %>% select (<var>, ... , <var>)
#select series of columns
#Syntax: <DATA> %>%  select (<start var>:<end var>)
#remove columns
#Syntax: <DATA> %>% select (-<vector containing the variable to remove>)
NEWDATA <- NEWDATA %>%  select(Age, Sex, Job, Housing, Credit_amount:Risk)

#common function: mutate()
#mutate() creates or transforms variables
#Syntax: <DATA> %>%  mutate(<newcolumnname> = <formula>)
mtcars %>%  mutate(kmpl = mpg * 0.425)
NEWDATA <- NEWDATA %>% mutate(New_loan = ifelse(Credit_amount <= 5000 & Duration <= 12, "Approve", "Not Approved"))
#new col: loan

#conditional statement in R
#Syntax: ifelse(<condition>, <true value returned>, <false returned>)
ifelse(Credit_amount <= 5000 & Duration <= 12, "Approve", "Not Approved")

#common function: summarize()
#summarize() produces summary statistics for variables
#Syntax <DATA> %>%  summarize(<newcolumnname> = <function> (<variable>, na.rm = <T/F>))
descriptive_stats <- NEWDATA %>% summarize(avg_crecit_amount = mean(Credit_amount, na.rm = T),
                      SD_credit_amount = sd(Credit_amount, na.rm = T))

#common function: group_by()
#group_by() - organizes data intro groups based on one or more variables
#Syntax: data %>%  group_by(column) %>%  summarize(summary_stat = function(variable))
ds_male_groupedby_housing <- NEWDATA %>% group_by(Housing) %>%  summarize(avg_credit_amount = mean(Credit_amount, na.rm = T),
                                             SD_credit_amount = sd(Credit_amount, na.rm = T))

#----Data Visualization----






