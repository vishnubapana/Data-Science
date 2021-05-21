#1b)
# i) Simulating 1 block lifetime of Xa and Xb.
Xa = rexp(n=1, rate=1/10) # Simulating 1 draw of Xa.
Xb = rexp(n=1, rate=1/10) # Simulation 1 draw of Xb.
x = max(Xa, Xb) # Taking the maximum of Xa and Xb.

# ii) Repeating step i) 10,000 times using the replicate function.
size10k = replicate(10000, max(rexp(n=1, rate=1/10), rexp(n=1, rate=1/10)))

# iii) Using Hist to generate a histogram and then superimposing 
     # the density function on the histogram using curve.
hist(size10k, prob = TRUE)
pdf = function(x){
  return (0.2*exp(-0.1*x)-0.2*exp(-0.2*x))
}
curve(pdf(x), add = TRUE)

# iv) Used mean to calculate the average of the 10,000 repetitions.
mean(size10k)
1-pexp(15, rate = 1/ mean(size10k)) # Using 1 - the distribution function to 
                                    # calculate the probability.

# 1c) Performed the step vi) using sample size of 1,000, and 100,000.
size1k = replicate(1000, max(rexp(n=1, rate=1/10), rexp(n=1, rate=1/10)))
mean(size1k)
1-pexp(15, rate = 1/(mean(size1k)))
size100k = replicate(100000, max(rexp(n=1, rate=1/10), rexp(n=1, rate=1/10)))
mean(size100k)
1-pexp(15, rate = 1/(mean(size100k)))