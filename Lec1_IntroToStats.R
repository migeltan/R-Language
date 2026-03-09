#----Introduction to R----
#Importing German Credit Risk Data set
library(readxl)
German_Credit_Risk <- read_excel("C:/Users/Migel/Downloads/German Credit Risk.xlsx")
View(German_Credit_Risk)

#Assigning a variable, syntax is <-
#R is case sensitive

#R data types
x <- 10
var <- 3.14 #numeric
int1 <- 76L #L represents integer/whole no.
flag <- FALSE #True or False/Boolean CAPITAL
name <- "Alice" #String or char
DATA <- German_Credit_Risk #Assign dataset to DATA variable

#R class
class(x)
class(var)
class(int1)
class(DATA)

#R package and library
library(dplyr) #data manipulation
library(ggplot2) #plotting/visualization

attach(DATA) #access vectors
detach(DATA) #detach to avoid data redundancy
#class(Age)

print("Hello World!")

#----User-Defined Functions----
#name <- function(arguments) {
  #statements
#  return(value)
#}

#function to square
square <- function(x){
  result <- x*x
  return(result)
}

#call function
square(6)
