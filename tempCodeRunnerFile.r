# Install required package for VS Code R extension
if (!require(jsonlite)) {
  install.packages("jsonlite")
  library(jsonlite)
}

# Basic computation
a <- 10
b <- 20
sum_result <- a + b


print("Testing R environment...")
print(paste("a =", a))
print(paste("b =", b))
print(paste("Sum =", sum_result))

# Create a small data frame
students <- data.frame(
  Name = c("Ana", "Ben", "Cara"),
  Score = c(90, 85, 88)
)

print("Student Data:")
print(students)

# Simple plot
x <- 1:10
y <- x^2

plot(x, y, type="b", main="Test Plot", xlab="X", ylab="X^2")

print("R test completed successfully!")