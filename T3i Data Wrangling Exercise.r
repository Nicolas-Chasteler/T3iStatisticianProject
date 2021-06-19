# Load Libraries
library(tidyr)
library(dplyr)
library(ggplot2)

# Load dataset
data <- read.csv("C:\\Users\\Nic Chasteler\\Desktop\\fitness_data.csv")


# Exercise 1
# Only keep completed data points
data <- subset(data, initial_complete == "Complete" & mid_complete == "Complete" & final_complete == "Complete")

# Remove older duplicate IDs
data <- data[order(as.Date(data$oft_initial_date, format = "%m/%d/%y")),]
data <- data[!duplicated(data$ID),]

# Subset 3mileruck columns
threemileruck.wide <- select(data, 30:32)

# Convert 3mileruck into long format
threemileruck.wide <- rename(threemileruck.wide, Initial = X3mileruck_initial_seconds, Mid = X3mileruck_mid_seconds, Final = X3mileruck_final_seconds)
threemileruck.long <- gather(threemileruck.wide, Iteration, Seconds, Initial:Final)
write.csv(threemileruck.long, "C:\\Users\\Nic Chasteler\\Desktop\\3mileruck.csv")
# Exercise 1 complete saved as 3mileruck


# Exercise 2
# Order into initial, mid, final
threemileruck.long$Iteration <- factor(threemileruck.long$Iteration, levels = c("Initial", "Mid", "Final"))

# Plot using ggplot
ggplot(threemileruck.long, aes(Seconds)) + geom_histogram(bins = 50) + facet_wrap(~Iteration, )


# Exercise 3
# To test for significant differences I will use the t-test, this test has the assumption that both groups being tested are normally distributed
# The normality can be checked with the shapiro wilk test
shapiro.test(threemileruck.wide$Initial)
shapiro.test(threemileruck.wide$Mid)
shapiro.test(threemileruck.wide$Final)
# Since all tests returned a p-value less than 0.01
# The variance can be checked with the F-test to see whether to use the standard t-test or welch t-test
var.test(threemileruck.wide$Initial, threemileruck.wide$Mid, alternative = "two.sided")
var.test(threemileruck.wide$Initial, threemileruck.wide$Final, alternative = "two.sided")
var.test(threemileruck.wide$Mid, threemileruck.wide$Final, alternative = "two.sided")
# Since the p-values was only greater than 0.01 between the intital and mid, I will use the test for equal variances for that comparison only
t.test(threemileruck.wide$Initial, threemileruck.wide$Mid, var.equal = TRUE)
t.test(threemileruck.wide$Initial, threemileruck.wide$Final)
t.test(threemileruck.wide$Mid, threemileruck.wide$Final)
# The t tests test against the null hypothesis that the true difference in means is not equal to 0
# It is clear from the p-values that the initial times have a difference of mean greater than 0 with a confidence interval greater than 99.999999999%
# Thus there was a significant improvement from the initial test
# Comparing between the Mid and Final, the p-value is 0.025 meaning that it is significantly different with a confidence level less than 97.5%


# Bonus Exercise
# My first thought is to look at how the class/time of the year affects improvements overall
# First I will create a dataframe of the ratios of improvements from the initial to final tests
df <- data
df$Broadjump <- data$broadjump_final_inches / data$broadjump_initial_inches
df$AgilityLeft <- data$agilityleft_initial_seconds / data$agilityleft_final_seconds
df$AgilityRight <- data$agilityright_initial_seconds / data$agilityright_final_seconds
df$Deadlift <- data$deadlift_final_pounds/data$deadlift_initial_pounds
df$Pullups <- data$pullups_final / data$pullups_initial
df$FarmersCarry <- data$farmerscarry_initial_seconds / data$farmerscarry_final_seconds
df$Shuttle <- data$X300yardshuttle_initial_seconds / data$X300yardshuttle_final_seconds
df$Fin <- data$X1500mfin_initial_seconds / data$X1500mfin_final_seconds
df$Ruck <- data$X3mileruck_initial_seconds / data$X3mileruck_final_seconds

#Next I will omit rows with missing data
df <- na.omit(df)

# Now I would like to look at the means of improvements
mean(df$Broadjump)
mean(df$AgilityLeft)
mean(df$AgilityRight)
mean(df$Deadlift)
mean(df$Pullups)
mean(df$FarmersCarry)
mean(df$Shuttle)
mean(df$Fin)
mean(df$Ruck)
#This produced that the 1500 meter fin had the greatest total improvement of 8.8% on average, next being the Ruck at 6.1%
#Both the broadjump and 300 meter shuttle had a negative result from the initial to end

#I will test if both significantly decreased using the t-test
t.test(df$broadjump_initial_inches, df$broadjump_final_inches)
t.test(df$X300yardshuttle_initial_seconds, df$X300yardshuttle_final_seconds)
#Both p-values resulted in less than 0.01 which means that the difference in means is not likely equal to 0

#Next I will see whether the change in performance took place before or after the mid test
t.test(df$broadjump_initial_inches, df$broadjump_mid_inches)
t.test(df$broadjump_mid_inches, df$broadjump_final_inches)
t.test(df$X300yardshuttle_initial_seconds, df$X300yardshuttle_mid_seconds)
t.test(df$X300yardshuttle_mid_seconds, df$X300yardshuttle_final_seconds)
#Both tests between the mid and final had p-values of greater than 0.3 meaning that there was no major change between the mid and final
#This could mean that the initial test is the major outlier in which they over performed 

#Next I would like to see whether different classes resulted in different performances
ggplot(df, aes(Broadjump)) + geom_histogram(bins = 100) + facet_wrap(~prep_class, )
ggplot(df, aes(Shuttle)) + geom_histogram(bins = 100) + facet_wrap(~prep_class, ) + scale_x_continuous(limits = c(0.75, 1.25))
#From these histograms it is possible to see oddities from the 21-001 class in the broadjump with far less improvement than any other class, to further check I can check the mean of just this class
mean(df[df$prep_class == "21-001", "Broadjump"])
#Which resulted in a decrease of 6.4% from the initial to the final value

#I can re-check the t-test for just this particular class
df.temp <- subset(df, prep_class == "21-001")
t.test(df.temp$broadjump_initial_inches, df.temp$broadjump_final_inches)
#By running this test it can be seen that this class has a p-value of 9e-10 which means that it is significantly decreased

#Next I will look at how all other progressed excluding class 21-001
df.temp <- subset(df, prep_class != "21-001")
t.test(df.temp$broadjump_initial_inches, df.temp$broadjump_final_inches)
#From this test it is possible to see that the mean between the initial and final values excluding class 21-001 is EXACTLY identical, meaning that class 21-001 had been the major outlier for broadjumps
#By looking at the values of the means of the specific class, the final was much lower than the average
#I would have done additional analysis of this class if I had the time to do so