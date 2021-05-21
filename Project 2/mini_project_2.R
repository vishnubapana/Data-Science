roadRace = read.csv(file = 'roadrace.csv')
head(roadRace)

# Problem 1 part a
# Stores the total sums of Maine and Away Runners
total_Maine = sum(roadRace$Maine == 'Maine')
total_Away = sum(roadRace$Maine == 'Away')
total_Runners = 5875
#generates a barplot of the data split between Maine and Away Runners
barplot(c(total_Maine, total_Away), names.arg = c('Maine', 'Away'), space = 0.1, ylab = 'Number of runners', ylim = c(0, 5000))

# Problem 1 part b
time_Maine = roadRace$Time..minutes[which(roadRace$Maine == 'Maine')]
time_Away = roadRace$Time..minutes[which(roadRace$Maine == 'Away')]
# range
range(time_Maine)
range(time_Away)
# mean
mean(time_Maine)
mean(time_Away)
# standard deviation 
sd(time_Maine)
sd(time_Away)
# Median
median(time_Maine)
median(time_Away)
# Interquartile Range
IQR(time_Maine)
IQR(time_Away)
# summary
summary(time_Maine)
summary(time_Away)
# histogram of Maine group and Away Group
hist(time_Maine, xlim = c(0,160), ylim = c(0, 2000))
hist(time_Away, xlim = c(0,160), ylim = c(0, 2000))

# Problem 1 part c
boxplot(time_Maine, time_Away, names = c('Maine', 'Away'), ylim = c(0, 160))

# Problem 1 part d
total_Male = as.integer(roadRace$Age[which(roadRace$Sex == 'M')])
total_Female = as.integer(roadRace$Age[which(roadRace$Sex == 'F')])

range(total_Male)
range(total_Female)

mean(total_Male)
mean(total_Female)

sd(total_Male)
sd(total_Female)

median(total_Male)
median(total_Female)

IQR(total_Male)
IQR(total_Female)

summary(total_Male)
summary(total_Female)

boxplot(total_Male, total_Female, names = c('Male', 'Female'), ylim = c(0, 100))

# Problem 2
data = read.csv(file = 'motorcycle.csv')
head(data)

total_FatalAccidents = data$Fatal.Motorcycle.Accidents

range(total_FatalAccidents)
mean(total_FatalAccidents)
sd(total_FatalAccidents)
median(total_FatalAccidents)
IQR(total_FatalAccidents)

summary(total_FatalAccidents)

boxplot(total_FatalAccidents, xlab = 'Fatal Accidents', ylab = 'Total Accidents')

lowerBound = max(quantile(total_FatalAccidents, prob = 0.25) - 1.5*IQR(total_FatalAccidents), min(total_FatalAccidents))

upperBound = min(quantile(total_FatalAccidents, prob = 0.75) + 1.5*IQR(total_FatalAccidents), max(total_FatalAccidents))

outlierCounty = data$County[which(data$Fatal.Motorcycle.Accidents < lowerBound|data$Fatal.Motorcycle.Accidents > upperBound)]

outlierCounty

















