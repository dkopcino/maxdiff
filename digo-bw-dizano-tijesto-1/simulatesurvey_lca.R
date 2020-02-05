source("surveyconfig.R")

designctx = readRDS(designctxfile)

survey = designctx$survey
# ignoriramo cov.predictors jer oni za LCA ne idu u izračun utilitya
fml = as.formula(paste("~", paste(c(unlist(designctx$design.predictors$g.predictors), unlist(designctx$design.predictors$as.predictors)), collapse = "+")))
mm = model.matrix.lm(fml, survey[1, ], na.action = "na.pass")
coef.names = colnames(mm)[-1]

# means of the encoded parameters
# indicates relationship to the base level (- means less attractive/lower usability, + more attractive/higher usability)
mu = round(abs(rnorm(length(coef.names), 0, 1)), 2)
names(mu) = coef.names

coef.names1 = c() # covariates names
if (length(designctx$covariates) > 0) {
  fml1 = as.formula(paste("~", paste(designctx$design.predictors$cov.predictors, collapse = "+")))
  cbdf = data.frame(designctx$fullfact_covdesign[1, ])
  colnames(cbdf) = colnames(designctx$fullfact_covdesign) # ako je samo jedan cov, onda ovo treba eksplicitno postaviti
  # ovo dodajemo da izbjegnemo kombinaciju (0, 0) koja radi problem recimo za LCA: softmax(0, 0)
  # uvijek daje jednake vjerojatnosti za sve segmente pa ne možemo dobiti ništa osim toga
  contrasts(cbdf[, designctx$design.predictors$cov.predictors]) = contr.sum
  mm1 = model.matrix.lm(fml1, cbdf, na.action = "na.pass")
  coef.names1 = colnames(mm1)[-1]
}

# we handle the prices differently because they should follow the logical higher price > smaller utility order
pricecoefs = which(grepl("cijena|price", coef.names))
# we assume that the lowest price is first in line and the highest price last
#mu[pricecoefs] = (-1)*sort(round(rexp(length(pricecoefs), 0.8), 2), decreasing = FALSE)
mu[pricecoefs] = (-1)*sort(round(abs(rnorm(length(pricecoefs), 0, 3)), 2), decreasing = FALSE)

# variances
Sigma = diag(round(abs(rnorm(length(coef.names), 0, 1)), 2))
rownames(Sigma) = colnames(Sigma) = coef.names
# we can add some correlations if we want but we have to take care that Sigma stays positive definite
#Sigma[coef.names[1], coef.names[length(coef.names)-1]] = Sigma[coef.names[length(coef.names)-1], coef.names[1]] = runif(1, -1, 1)

set.seed(99312)
resp.id = 1:200 # respondent ids

library(MASS)
# library(matrixcalc)
# is.positive.definite(Sigma)

# za svaki segment trebamo kreirati iste koeficijente
K = length(coef.names)
G = length(coef.names1)
if (G == 0) G = 1 # ako nema covariates, onda će covariates biti samo intercept/prosjek/svi će imati jednaku vjerojatnost za pripadnost bilo kojem segmentu
CL = designctx$rsegments
if (CL == 0) CL = length(resp.id) # ako nema segmenata, onda je svaki ispitanik vlastiti "segment" i ima vlastite koficijente

# coefs su beta
coefs = mvrnorm(CL, mu = mu, Sigma = Sigma)
colnames(coefs) = coef.names

# gamma za prebacivanje iz covariates u segmente
# ako je G = 1, onda se generira samo jedan stupac koeficijenata
# ako je G = 1, i nema covariates, onda će matrica Z biti vektor jedinica, i svi će imati jednaku vjerojatnost
# za pripadnost svakom segmentu
covmu = round(rnorm(CL, 0, 1), 2)
names(covmu) = paste("seg_", 1:CL, sep = "")
covSigma = diag(round(abs(rnorm(CL, 0.01, 1)), 2))
rownames(covSigma) = colnames(covSigma) = names(covmu)
gammacoefs = array(mvrnorm(G, mu = covmu, Sigma = covSigma), dim = c(CL, G))
colnames(gammacoefs) = coef.names1


nquestionnaires = designctx$nquestionnaires
nquestions = designctx$nquestions
nalternatives = designctx$nalternatives

cbc.df = data.frame(NULL)
seg.df = data.frame(resp.id = resp.id, seg = 1)
for (i in seq_along(resp.id)) {
  
  q = sample(1:nquestionnaires, size = 1)
  surveydf = survey[survey$vers == q, ]
  if (length(designctx$covariates) > 0) {
    covsi = sample(nrow(designctx$fullfact_covdesign), 1)
    n_s = ncol(surveydf)
    surveydf = cbind(surveydf, designctx$fullfact_covdesign[rep(covsi, nrow(surveydf)), ])
    colnames(surveydf)[(n_s+1):ncol(surveydf)] = colnames(designctx$fullfact_covdesign) # ako je samo jedan cov, onda ovo treba eksplicitno postaviti
    
    #fml1 = as.formula(paste("~", paste(designctx$design.predictors$cov.predictors, collapse = "+"))) # isto kao gore
    cbdf = data.frame(designctx$fullfact_covdesign[covsi, ])
    colnames(cbdf) = colnames(designctx$fullfact_covdesign) # ako je samo jedan cov, onda ovo treba eksplicitno postaviti
    # ovo dodajemo da izbjegnemo kombinaciju (0, 0) koja radi problem recimo za LCA: softmax(0, 0)
    # uvijek daje jednake vjerojatnosti za sve segmente pa ne možemo dobiti ništa osim toga
    contrasts(cbdf[, designctx$design.predictors$cov.predictors]) = contr.sum
    mm1 = model.matrix.lm(fml1, cbdf, na.action = "na.pass")
    Z = array(t(mm1[, -1]), dim = c(G, 1))
  } else {
    Z = array(1, dim = c(1, 1))
  }
  
  profiles.i = model.matrix.lm(fml, surveydf, na.action = "na.pass")[, -1] # ignore the Intercept
  
  if (designctx$rsegments == 0) {
    # ako je rsegments=0, to znači da je svaki ispitanik vlastiti "segment"
    respsi = i
  } else {
    theta = as.vector(softmax(gammacoefs %*% Z)) # vektor vjerojatnosti da ispitanik pripada u segmente
    #respsi = which.max(theta) # u koji segment spada ispitanik
    respsi = extraDistr::rcat(1, prob = theta)
  }
  seg.df[i, "seg"] = respsi

  # change the NA values which are assigned for the missing alternative specific attributes to zeros (to prevent
  # influence on utility)
  r = lapply(colnames(profiles.i), function(cn) {
    profiles.i[which(is.na(profiles.i[, cn])), cn] <<- 0
  })
  utility = profiles.i %*% coefs[respsi, ]
  
  wide.util = matrix(data = utility, ncol = nalternatives, byrow = TRUE)
  probs = exp(wide.util) / rowSums(exp(wide.util))
  choice = apply(probs, 1, function(x) sample(1:nalternatives, size = 1, prob = x))
  choice = rep(choice, each = nalternatives) == rep(1:nalternatives, nquestions)
  
  # personal information does not participate in the calculation, so we just add it here to have full simulated answers
  if (length(designctx$personals) > 0) {
    r = lapply(1:length(designctx$personals), function(i) {
      p = designctx$personals[i]
      n = names(p)
      if (p[[n]][["tip"]] == "email") {
        # samo to handleamo
        surveydf[[n]] <<- rep("abc@g.com", nrow(surveydf))
      }
    })
  }
  
  conjoint.i = data.frame(questionnaire.id = rep(q, nquestions),
                          resp.id = rep(i, nquestions),
                          ques = rep(1:nquestions, each = nalternatives),
                          alt = rep(1:nalternatives, nquestions),
                          surveydf[, names(surveydf)[-(1:3)]],
                          choice = as.numeric(choice))
  cbc.df = rbind(cbc.df, conjoint.i)
  
}

rownames(cbc.df) = c()
write.csv(cbc.df, row.names = FALSE, file = answersfile)
saveRDS(list(beta = coefs, gamma = gammacoefs, seg.df = seg.df), simulatecoefsfile)
