library(boot)
gpa.array <- read.csv(file = "data/gpa.csv")
gpa <- as.numeric(gpa.array$gpa)
act <- as.numeric(gpa.array$act)

plot(gpa, act, main="GPA vs ACT", xlab="GPA VALUES", ylab="ACT VALUES")
abline(lm(act~gpa))

cor(gpa,act)

covar <- function(gpa.arr, x){
  gpavar <- gpa.arr$gpa[x]
  actvar <- gpa.arr$act[x]
  return (cor(gpavar, actvar))
}
covar.boot <- boot(gpa.array, covar, R = 999, sim = "ordinary", stype="i")
covar.boot
mean(covar.boot$t)

boot.ci(covar.boot)

#----------------------------------------------------------------------------#

