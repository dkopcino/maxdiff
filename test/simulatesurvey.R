source("surveyconfig.R")

designctx = readRDS(designctxfile)

survey = cbind(designctx$survey, encode_items(designctx$survey$alt, designctx$items))

cnames = colnames(survey[, !(colnames(survey) %in% c("version", "task", "alt"))])

# ako radimo bez anchora, onda prvi item stavljamo kao referentni
coef.names = cnames[-1]

# means of the encoded parameters
# indicates relationship to the base level (- means less attractive/lower usability, + more attractive/higher usability)
mu = round(runif(length(coef.names), -3, 3), 2)
names(mu) = coef.names

# variances
Sigma = diag(round(runif(length(coef.names), 0.1, 1), 2))
rownames(Sigma) = colnames(Sigma) = coef.names
# we can add some correlations if we want but we have to take care that Sigma stays positive definite
#Sigma[coef.names[1], coef.names[length(coef.names)-1]] = Sigma[coef.names[length(coef.names)-1], coef.names[1]] = runif(1, -1, 1)

set.seed(99312)
resp.id = 1:50 # respondent ids
library(MASS)
# library(matrixcalc)
# is.positive.definite(Sigma)
coefs = mvrnorm(length(resp.id), mu = mu, Sigma = Sigma)
colnames(coefs) = coef.names

# make a few respondents bad buyers, this will decrease their utilities by setting "wouldn't buy" in anchor questions
non_buyers = sample(1:length(resp.id), .20*length(resp.id), replace = FALSE)

nquestionnaires = designctx$nquestionnaires
nquestions = designctx$nquestions
nalternatives = designctx$nalternatives

cbc.df = data.frame(NULL)
for (i in seq_along(resp.id)) {
  
  q = sample(1:nquestionnaires, size = 1)
  surveydf = survey[survey$version == q, coef.names]
  if (length(designctx$covariates) > 0) {
    covsi = sample(nrow(designctx$fullfact_covdesign), 1)
    surveydf = cbind(surveydf, designctx$fullfact_covdesign[rep(covsi, nrow(surveydf)), , drop = FALSE])
  }
  
  # choose the best option
  utility = as.matrix(surveydf[, coef.names]) %*% coefs[i, ]
  wide.util = matrix(data = utility, ncol = nalternatives, byrow = TRUE)
  probs = exp(wide.util) / rowSums(exp(wide.util))
  best_choice = apply(probs, 1, function(x) sample(1:nalternatives, size = 1, prob = x))
  best_choice = (rep(best_choice, each = nalternatives) == rep(1:nalternatives, nquestions))
  
  # choose the worst option (must be different from the best option)
  utility = (-1)*as.matrix(surveydf[, coef.names]) %*% coefs[i, ]
  utility[best_choice] = -Inf
  wide.util = matrix(data = utility, ncol = nalternatives, byrow = TRUE)
  probs = exp(wide.util) / rowSums(exp(wide.util))
  worst_choice = apply(probs, 1, function(x) sample(1:nalternatives, size = 1, prob = x))
  worst_choice = (rep(worst_choice, each = nalternatives) == rep(1:nalternatives, nquestions))
  
  if (designctx$anchors > 0) {
    srv = designctx$survey[designctx$survey$version == q, ]
    anchors = make.names(getanchors(
      designctx$anchors, as.character(srv[best_choice, ]$alt), as.character(srv[worst_choice, ]$alt)))
    for (anch in getanchorcolnames(designctx$anchors)) {
      # 1 for positive answer (would buy, like it), 2 for negative answer (would not buy, don't like it)
      if (i %in% non_buyers) {
#        surveydf[[anch]] = rep(ifelse(runif(1) > .1, 2, 1), nrow(surveydf))
        surveydf[[anch]] = rep(2, nrow(surveydf))
      } else {
        surveydf[[anch]] = rep(ifelse(runif(1) > .5, 2, 1), nrow(surveydf))
      }
    }
  }
  
  # personal information does not participate in the calculation, so we just add it here to have full simulated answers
  if (length(designctx$personals) > 0) {
    for (pi in 1:length(designctx$personals)) {
      p = designctx$personals[pi]
      n = names(p)
      # samo ove handleamo
      if (p[[n]][["tip"]] == "email") {
        surveydf[[n]] = rep(paste(sprintf("%04d", i), "abc@g.com", sep = ""), nrow(surveydf))
      } else if (p[[n]][["tip"]] == "dropdown") {
        surveydf[[n]] = rep(sample(p[[n]][["vrijednosti"]], 1), nrow(surveydf))
      }
    }
  }
  
  maxdiff.i = data.frame(questionnaire.id = rep(q, nquestions),
                         resp.id = rep(i, nquestions),
                         ques = rep(1:nquestions, each = nalternatives),
                         alt = survey[survey$version == q, "alt"],
                         surveydf[, !(colnames(surveydf) %in% coef.names)],
                         best_choice = as.numeric(best_choice),
                         worst_choice = as.numeric(worst_choice))
  cbc.df = rbind(cbc.df, maxdiff.i)
  
}

rownames(cbc.df) = c()
write.csv(cbc.df, row.names = FALSE, file = answersfile)
saveRDS(coefs, simulatecoefsfile)


