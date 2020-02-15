# https://www.r-bloggers.com/how-to-create-a-max-diff-experimental-design-in-r/
# https://www.r-bloggers.com/how-to-analyze-max-diff-data-in-r/

library(choiceDes)
library(AlgDesign)

source("surveyconfig.R")


# respondent personal information (not covariates or variables) to be added to the survey
# personals = list()
# personals = list(zemlja = list(naziv = "zemlja", tip = "dropdown", vrijednosti = c("Hrvatska", "Bosna i Hercegovina", "Ostalo")),
#                  dob = list(naziv = "dob", tip = "dropdown", vrijednosti = c("manje od 20 godina", "20-50 godina", "više od 50 godina")))
personals = list(email = list(naziv = "e-pošta", tip = "email"))


# number of responders segments, 0 means each respondent is a "segment"
rsegments = 2

# do we anchor the results? if yes, how many questions shall we use for acnhoring?
# if anchors == 0, we don't use anchoring
anchors = 3

# choice (respondent) dependent covariates
# covariates = list()
# covariates = list(dob = list(naziv = "dob", vrijednosti = c("mlađi od 20 godina", "20-50 godina", "stariji od 50 godina")),
#                   zemlja = list(naziv = "zemlja", vrijednosti = c("Hrvatska", "Bosna i Hercegovina")),
#                   bracni_status = list(naziv = "bračni status", vrijednosti = c("samac/samkinja", "u braku", "razveden", "udovac/udovica")),
#                   broj_djece = list(naziv = "broj djece", vrijednosti = c("bez djece", "1 dijete", "2 djece", "više od 2 djece")))
covariates = list(lokacija = list(naziv = "lokacija", vrijednosti = c("blizu pizzerije", "daleko od pizzerije")))


# items to choose from
items = factor(c("miješana", "povrtna", "pikantna", "4 vrste sira", "s tunom", "ribarska", "4 godišnja doba",
                 "calzone", "slavonska", "rukola/pršut", "bolonjez", "lovačka", "losos", "sa salamom"))

# želimo da se svaki item prikaže barem 3 puta i da se prikazuje 3 itema po pitanju
nalternatives = 3 # koliko se prikazuje u svakom pitanju?
nquestionnaires = 3 # koliko varijanti upitnika?
nquestions = ceiling((length(items) * 3)/nalternatives/nquestionnaires) + 1 # koliko pitanja po upitniku?

dz = tradeoff.des(items = length(items), shown = nalternatives, vers = nquestionnaires, tasks = nquestions, 
                  fname = NULL, Rd = 500, Rc = 10000, print = TRUE)
cp.scree(dz) # da li se critical stabilizirao?
dz$design

# output design to be constructed
fullfact_covdesign = data.frame() # for covariates

## add and build covariatess
if (length(covariates) > 0) {
  covariates_lengths = unlist(lapply(covariates, function(a) length(a[["vrijednosti"]])))
  if (length(covariates) == 1) nVars = 1 else nVars = 0 # ako je samo jedan, onda to eksplicitno navedemo, ako ih je više, onda će se to vidjeti iz vektora duljina
  fullfact_covdesign = gen.factorial(covariates_lengths, nVars = nVars, center = FALSE, 
                                     factors = "all", varNames = names(covariates))
  for (an in names(covariates)) levels(fullfact_covdesign[[an]]) = unique(covariates[[an]][["vrijednosti"]])
}

itemcol = unlist(lapply(1:max(dz$design[, "card"]), function(card) {
  array(dz$design[card, -c(1:3)], dim = c(nalternatives, 1))
}))
survey = data.frame(dz$design[rep(1:nrow(dz$design), each = nalternatives), c("version", "task")], alt = items[itemcol])

designctx = list(
  items = items,
  nquestionnaires = nquestionnaires,
  nquestions = nquestions,
  nalternatives = nalternatives,
  survey = survey,
  design = dz,
  covariates = covariates,
  fullfact_covdesign = fullfact_covdesign,
  personals = personals,
  rsegments = rsegments,
  anchors = anchors
)

saveRDS(designctx, designctxfile)
#designctx = readRDS(designctxfile)

paste("Total number of parameters to estimate:", length(items) - 1)
paste("Total number of observations:", nrow(designctx$survey))

# needed sample size, orme, page 77
ssize = c(800, 350, 200) * (2 * length(items) / nalternatives / (nquestionnaires * nquestions))
paste("Needed sample size (safe to less safe):", paste(round(ssize), collapse = ", "))

