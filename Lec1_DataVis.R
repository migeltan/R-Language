#----Data Visualization----
#Base graphics and ggplot2
library(readxl)
library(dplyr)
library(ggplot2)
German_Credit_Risk <- read_excel("C:/Users/Migel/Downloads/German Credit Risk.xlsx")
View(German_Credit_Risk)
DATA <- German_Credit_Risk #Assign dataset to DATA variable

attach(DATA)

#----BASE GRAPHICS----
#STEP 1: EMPTY PLOT
plot(Duration, Credit_amount, trype = "n",
     xlab = "Loan Duration (Months)", ylab = "Credit Amount")

#STEP 2: ADD POINTS
points(Duration, Credit_amount, pch = 19, col = "darkblue")

#STEP 3: ADD REGIS FEE
abline(lm(Credit_amount ~ Duration, data = NEWDATA), col = "red")

#STEP 4: ADD TITLE
title("Credit Amount vs Duration (Base R")

#STEP 5: ADD LEGEND
legend("topright", legend = c("Data", "Fit"),
       col = c("darkblue", "red"), pch = c(19,NA), lty = c(NA, 1))

#----ggplot2 graphics----
#Layered approach, run nang buo
ggplot(DATA, aes(x=Duration, y=Credit_amount)) +
  #layer 1: points
  geom_point(aes(color=Housing), size=2, alpha=0.7) +
  #layer 2: smooth regression line
  geom_smooth(method="lm", se=FALSE, color ="red") +
  #layer 3: add text labels for high credit amounts
  geom_text(aes(label=ifelse(Credit_amount > 15000, rownames(NEWDATA), "")),
            hjust=0.2, vjust=0.5, size=3) +
  #layer 4: facet by job type
  facet_wrap(~Job) +
  #layer 5: Customize theme
  theme_minimal()+
  theme(legend.position="bottom") +
  #layer 6: titles and labels
  labs(title="Credit Amount vs Duration",
       subtitle="Colored by Housing, Faceted by Job",
       x="Duration (Months)",
       y= "Credit Amount",
       color = "Housing")





