checkzci_function = function(n , lamda)
{ 
  U = rexp(n, lamda)
  lb = mean(U) - qnorm(0.975) * sd(U)/sqrt(n)
  ub = mean(U) + qnorm(0.975) * sd(U)/sqrt(n)
  tm = 1/lamda
  
  if(ub > tm & lb < tm)
  {
    return(1)
  }
  else
  {
    return(0)
  }
}

zproportion_function = function(n, lamda)
{
  values = replicate(5000, checkzci_function(n, lamda))
  ones = values[which(values == 1)]
  return(length(ones)/5000)
}

zproportion_function(100, 0.01)
zproportion_function(100, 0.1)
zproportion_function(100, 1)
zproportion_function(100, 10)

mean_star = function(n, lamda)
{
  u_star = rexp(n, lamda)
  return(mean(u_star))
}
  
checkbci_function = function(n, lamda)
{
  U = rexp(n, lamda)
  tm = 1/lamda
  lamda_1 = 1/mean(U)
  V = replicate(1000, mean_star(n, lamda_1))
  bound = sort(V)[c(25, 975)]
  
  if(bound[2] > tm & bound[1] < tm)
  {
    return(1)
  }
  else
  {
    return(0)
  }
  
}

bproportion_function = function(n, lamda)
{
  values = replicate(5000, checkbci_function(n, lamda))
  ones = values[which(values == 1)]
  return(length(ones)/5000)
}

bproportion_function(100, 0.01)
bproportion_function(100, 0.1)
bproportion_function(100, 1)
bproportion_function(100, 10)

zcimatrix = matrix(c(zproportion_function(5, 0.01), zproportion_function(10, 0.01), zproportion_function(30, 0.01), zproportion_function(100, 0.01),
                     zproportion_function(5, 0.1), zproportion_function(10, 0.1), zproportion_function(30, 0.1), zproportion_function(100, 0.1),
                     zproportion_function(5, 1), zproportion_function(10, 1), zproportion_function(30, 1), zproportion_function(100, 1),
                     zproportion_function(5, 10), zproportion_function(10, 10), zproportion_function(30, 10), zproportion_function(100, 10)),
                     nrow = 4, ncol = 4)

bcimatrix = matrix(c(bproportion_function(5, 0.01), bproportion_function(10, 0.01), bproportion_function(30, 0.01), bproportion_function(100, 0.01),
                     bproportion_function(5, 0.1), bproportion_function(10, 0.1), bproportion_function(30, 0.1), bproportion_function(100, 0.1),
                     bproportion_function(5, 1), bproportion_function(10, 1), bproportion_function(30, 1), bproportion_function(100, 1),
                     bproportion_function(5, 10), bproportion_function(10, 10), bproportion_function(30, 10), bproportion_function(100, 10)),
                   nrow = 4, ncol = 4)

par(mfrow = c(2, 2))
plot(c(5, 10, 30, 100), zcimatrix[, 1], main = "L = 0.01", xlab = 'n', ylab = 'Proportions', col = 'red', type = 'b', xlim = c(1, 100), ylim = c(0, 1))
lines(c(5, 10, 30, 100), bcimatrix[, 1], col = 'blue', type = 'b')

plot(c(5, 10, 30, 100), zcimatrix[, 2], main = "L = 0.1", xlab = 'n', ylab = 'Proportions', col = 'red', type = 'b', xlim = c(1, 100), ylim = c(0, 1))
lines(c(5, 10, 30, 100), bcimatrix[, 2], col = 'blue', type = 'b')

plot(c(5, 10, 30, 100), zcimatrix[, 3], main = "L = 1", xlab = 'n', ylab = 'Proportions', col = 'red', type = 'b', xlim = c(1, 100), ylim = c(0, 1))
lines(c(5, 10, 30, 100), bcimatrix[, 3], col = 'blue', type = 'b')

plot(c(5, 10, 30, 100), zcimatrix[, 4], main = "L = 10", xlab = 'n', ylab = 'Proportions', col = 'red', type = 'b', xlim = c(1, 100), ylim = c(0, 1))
lines(c(5, 10, 30, 100), bcimatrix[, 4], col = 'blue', type = 'b')



plot(c(0.01, 0.1, 1, 10), zcimatrix[, 1], main = "N = 5", xlab = 'Lambda', ylab = 'Proportions', col = 'red', type = 'b', xlim = c(0.01, 10), ylim = c(0, 1))
lines(c(0.01, 0.1, 1, 10), bcimatrix[, 1], col = 'blue', type = 'b')

plot(c(0.01, 0.1, 1, 10), zcimatrix[, 1], main = "N = 10", xlab = 'Lambda', ylab = 'Proportions', col = 'red', type = 'b', xlim = c(0.01, 10), ylim = c(0, 1))
lines(c(0.01, 0.1, 1, 10), bcimatrix[, 1], col = 'blue', type = 'b')

plot(c(0.01, 0.1, 1, 10), zcimatrix[, 1], main = "N = 30", xlab = 'Lambda', ylab = 'Proportions', col = 'red', type = 'b', xlim = c(0.01, 10), ylim = c(0, 1))
lines(c(0.01, 0.1, 1, 10), bcimatrix[, 1], col = 'blue', type = 'b')

plot(c(0.01, 0.1, 1, 10), zcimatrix[, 1], main = "N = 100", xlab = 'Lambda', ylab = 'Proportions', col = 'red', type = 'b', xlim = c(0.01, 10), ylim = c(0, 1))
lines(c(0.01, 0.1, 1, 10), bcimatrix[, 1], col = 'blue', type = 'b')















































  