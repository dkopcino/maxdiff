m = matrix(data = c(2, 0, 0, 2), nrow = 2, byrow = TRUE)
solve(m)

m = matrix(data = c(1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1), nrow = 4, byrow = TRUE)
solve(m)

#install.packages("PracTools")
library(PracTools)

# Example 3.1 (Sample size for a target CV). Suppose that we estimate from a
# previous survey that the population CV of some variable is 2.0. If the popu-
#   lation is extremely large and CV 0 (the target CV) is set to 0.05, then the call
# to the R function is nCont(CV0=0.05, CVpop=2). The resulting sample
# size is 1,600. If the population size is N = 500, then nCont(CV0=0.05,
# CVpop=2, N=500) results in a sample size of 381 after rounding. The fpc
# factor has a substantial effect in the latter case.
nCont(CV0 = 0.05, CVpop = 2)
nCont(CV0 = 0.05, CVpop = 2, N = 500)
nPropMoe(moe.sw = 1, e = seq(0.01, 0.08, 0.01), alpha = 0.05, pU = 0.5)
nWilson(moe.sw = 1, pU = 0.04, e = 0.01)
