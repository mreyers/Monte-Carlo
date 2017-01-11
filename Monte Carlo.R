# Estimating Pi using Monte Carlo simulation

# Challenge:
# Throw rocks randomly at a circle inscribed on a square. Use results to estimate Pi

# Approach

# Simulate. r value indicates length on each side of 0 
nsims = 1000
pi = numeric(nsims)



for(i in 1:nsims){
r = 5 
nthrows = 1000
x = numeric(nthrows)
y = numeric(nthrows)

# Use Uniform distribtuion
x = runif(n = nthrows, min = -r, max = r)
y = runif(n = nthrows, min = -r, max = r)
coords = cbind(x,y)
coordsDF = as.data.frame(coords)
head(coordsDF)

# Check if each throw landed in circle or only in square
# Check x^2 + y^2 < r^2
z = x^2 + y^2
coordsDF$z = z
coordsDF$circle = 0
coordsDF$circle[coordsDF$z < r^2] = 1
head(coordsDF)

# Area of a square = 2r * 2r = 4r^2
# Area of a circle = pi*r^2
# ratio of areas: 4 / pi
# ratio of sample: total / in circle
# Ratios should converge to same value as sample size grows
# 4 / pi = total / in circle
# pi = 4 * in circle / total

pi[i] = 4 * sum(coordsDF$circle) / length(coordsDF$circle)
}

calcStats = function(x){
  meanpi = mean(x)
  sepi = sd(x)
  lowEst = mean(x) - qnorm(0.975)*sepi
  highEst = mean(x) + qnorm(0.975)*sepi
  return(c(SampleSizeIn1000s = length(x)/1000, Mean = meanpi, StandardError = sepi, CIlowerbound = lowEst, CIupperbound = highEst))
}

calcStats(pi)

