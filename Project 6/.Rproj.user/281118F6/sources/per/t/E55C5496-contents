# read data
cancer = read.csv('prostate_cancer.csv')
attach(cancer)

boxplot(psa)

psa.log = log(psa)
boxplot(psa.log)


par(mfrow=c(3,2))
plot(cancervol, psa.log, xlab="Cancer Volume", ylab="PSA Log")
abline(lm(psa.log ~ cancervol))

plot(weight, psa.log, xlab="weight", ylab="PSA Log")
abline(lm(psa.log ~ weight))

plot(age, psa.log, xlab="age", ylab="PSA Log")
abline(lm(psa.log ~ age))

plot(benpros, psa.log, xlab="Benign prostatic hyperplasia", ylab="PSA Log")
abline(lm(psa.log ~ benpros))

plot(capspen, psa.log, xlab="Capsular penetration", ylab="PSA Log")
abline(lm(psa.log ~ capspen))


first_fit = lm(psa.log ~ cancervol + weight + age + benpros + capspen)
first_fit
summary(first_fit)

second_fit = lm(psa.log ~ cancervol + capspen + benpros)
second_fit

anova(second_fit, first_fit)

third_fit = lm(psa.log ~ cancervol + benpros)
third_fit

anova(third_fit, second_fit)

qqnorm(resid(third_fit))
qqline(resid(third_fit))

fourth_fit = update(third_fit, . ~ . + factor(vesinv))
fifth_fit = update(third_fit, . ~ . + factor(gleason))

summary(fourth_fit)
anova(third_fit, fourth_fit)

summary(fifth_fit)
anova(third_fit, fifth_fit)

sixth_fit = update(third_fit, . ~ . + factor(vesinv) + factor(gleason))
summary(sixth_fit)

plot(fitted(sixth_fit), resid(sixth_fit))
abline(h = 0)

qqnorm(resid(sixth_fit))
qqline(resid(sixth_fit))

mode = function(v){
  unique_val = unique(v)
  unique_val[which.max(tabulate(match(v,unique_val)))]
}

predict_val = predict(sixth_fit, data.frame(cancervol = mean(cancervol),
                                            benpros = mean(benpros),
                                            vesinv = mode(vesinv),
                                            gleason = mode(gleason)))

exp(predict_val)