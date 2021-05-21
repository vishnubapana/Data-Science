#Importing data
data = read.csv("data/VOLTAGE.csv")

remote_data = data$voltage[which(data$location == 0)]
local_data = data$voltage[which(data$location == 1)]

#Boxplot of remote and local data
boxplot(remote_data, local_data, names = c("Remote", "Local"), main = "Boxplot of voltages", range = 1.5)

#5 point summary of remote and local data
summary(remote_data)
summary(local_data)

#QQ plot of remote and local data
par(mfrow = c(1, 2))
qqnorm(remote_data, main = "Remote")
qqline(remote_data)
qqnorm(local_data, main = "Local")
qqline(local_data)

#Constructing confidence interval
mean(remote_data) #9.803667
mean(local_data) #9.422333

var(remote_data) #0.2925895
var(local_data) #0.229322

standard_error = sqrt(var(remote_data)/30 + var(local_data)/30) #0.1318979

difference_mean = mean(remote_data) - mean(local_data)

CI = difference_mean + (c(-1, 1) * qnorm(0.975) * standard_error) #0.1228182 0.6398484

#Confidence interval using t test
t.test(remote_data, local_data, alternative = "two.sided", paired =FALSE, var.equal = FALSE, conf.level = 0.95)
