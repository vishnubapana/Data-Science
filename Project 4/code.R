gpa.array <- read.csv(file = "gpa.csv")
gpa <- as.numeric(gpa.array$gpa)
act <- as.numeric(gpa.array$act)

plot(gpa, act, main="GPA vs ACT", xlab="GPA VALUES", ylab="ACT VALUES")
abline(lm(act~gpa))

cor(gpa,act)
