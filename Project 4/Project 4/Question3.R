
vapor.array <- read.csv("data/VAPOR.csv")
qqnorm(vapor.array$theor, main = "Theoretical")
qqline(vapor.array$theor)
qqnorm(vapor.array$exper, main = "Experimental")
qqline(vapor.array$exper)
boxplot(vapor.array$theor, vapor.array$exper, names = c("Theoretical", "Experimental"), main="Boxplot Readings")

vapor.array.diff = vapor.array$theor - vapor.array$exper
mean(vapor.array.diff)
sd(vapor.array.diff)
qt(0.975, 15)
mean(vapor.array.diff) + c(-1,1)*qt(0.975,15)*sd(vapor.array.diff)/sqrt(16)
t.test(vapor.array$theor, vapor.array$exper, alternative="two.sided", paired = T, var.equal = F, conf.level = 0.95)