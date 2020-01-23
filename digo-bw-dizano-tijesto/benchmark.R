library(choiceDes)
library(AlgDesign)

nquestionnaires = 50
nquestions = 10
nalternatives = 3

gattribs = list()
i = 1
gattribs[[paste("var_", i, sep = "")]] = paste("lev", 1:3, sep = "")
times.x = c()
times.y = c()
r = lapply(2:10, function(i) {
  
  # each time we add a new factor with 3 levels
  gattribs[[paste("var_", i, sep = "")]] <<- paste("lev", 1:3, sep = "")
  
  startt = Sys.time()
#  paste("START:", startt)
  
  # generate the generic part of the survey
  gattribs_lengths = unlist(lapply(gattribs, function(a) length(a)))
  fullfact_gdesign = gen.factorial(gattribs_lengths, 
                                   nVars = 0, 
                                   center = FALSE, 
                                   factors = "all", 
                                   varNames = names(gattribs))
  ret = lapply(names(gattribs), function(an) levels(fullfact_gdesign[[an]]) <<- unique(gattribs[[an]]))

  gdesign = dcm.design.cand(cand = fullfact_gdesign, nb = nquestionnaires, sets = nquestions, alts = nalternatives, Rd = 50, print = FALSE)
  
  endt = Sys.time()
  total = as.numeric(endt)-as.numeric(startt)
  # paste("END:", endt)
  print(paste("TOTAL:", round(total, 2)))
  times.x <<- c(times.x, nrow(fullfact_gdesign))
  times.y <<- c(times.y, total)
  
})


library(ggplot2)
times.z = times.y
#times.z = (times.x/30)^1.5)
ggplot(data = data.frame(times.x, times.y, times.z)) + geom_line(aes(times.x, times.z)) + geom_point(aes(times.x, times.y))

















library(idefix)

r = lapply(1:3, function(i) {
  
  startt = Sys.time()
  
  cand.set = Profiles(lvls = rep(3, i+1), coding = rep("D", i+1))
  # number of parameters = (i+1)*2
  npar = (i+1)*2
  mu = runif(npar, -1, 1) # Prior parameter vector
  v = diag(length(mu)) # Prior variance.
  pd = MASS::mvrnorm(n = 10, mu = mu, Sigma = v) # 10 draws.
  Modfed(cand.set = cand.set, n.sets = 500, n.alts = 3, parallel = TRUE, par.draws = pd, best = TRUE)
  
  endt = Sys.time()
  total = as.numeric(endt)-as.numeric(startt)
  # paste("END:", endt)
  print(paste("TOTAL[", i+1 , "]:", round(total, 2)))
  
})





