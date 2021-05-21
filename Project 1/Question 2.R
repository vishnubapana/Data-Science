x = runif(10000, min=0, max=1)
y = runif(10000, min=0, max=1)
inscribed = (x-0.5)^2 + (y-0.5)^2 <= 0.5^2
4*(sum(inscribed)/10000)