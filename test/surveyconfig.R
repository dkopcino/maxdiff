iid = "test"
setwd(paste("C:/OnlineSync/Mega/R/work/maxdiff/", iid, sep = ""))
# define globally used CBC id.
cbcid = paste("cbc_", iid, sep = "") # used for cbc/design identification

designctxfile = paste(cbcid, "_designctx.RDS", sep = "")

limesurveyqidsfile = paste(cbcid, "_limesurveyqids.RDS", sep = "")
#change the next options (website, user, password)
# options(lime_api = 'http://localhost:8080/limesurvey/index.php/admin/remotecontrol')
# options(lime_username = 'admin')
# options(lime_password = '1234')
options(lime_api = "https://anketar.biz/limesurvey/index.php/admin/remotecontrol")
options(lime_username = 'admin')
options(lime_password = '6Q(XEbj7ylU%R7QGCrqa')

answersfile = simulatesurveyfile = paste(cbcid, "_answers.csv", sep = "")

options(contrasts = c("contr.treatment", "contr.poly"))

mnlmodelfile = paste(cbcid, "_model_mnl.RDS", sep = "")

hbmodelfile = paste(cbcid, "_model_hb.RDS", sep = "")

lcmodelfile = paste(cbcid, "_model_lc.RDS", sep = "")

stanmcmodelfile = paste(cbcid, "_model_stanmc.RDS", sep = "")

stanmclcamodelfile = paste(cbcid, "_model_stanmclca.RDS", sep = "")

valuesfile = paste(cbcid, "_vrijednosti.RDS", sep = "")

simulatecoefsfile = paste(cbcid, "_simcoefs.RDS", sep = "")

# price/cijena iz tekst formata u numerički
price2num = function(price) {
  i = regexpr("kn", price)
  j = regexpr("EUR", price)
  if (max(i, j) > 0) {
    as.numeric(substr(price, 1, max(i, j)-1))
  } else {
    -1
  }
}

# price/cijena iz numeričkog formata u tekst
num2price = function(price) {
  paste(price, "kn", sep = "")
  #  paste(price, "EUR", sep = "")
}

# softmax
softmax = function(x) exp(x)/sum(exp(x))

# get columns from a data frame
# if only one column, cast its factor to a single column data frame
getdfcolumns = function(df, cselect = NULL, cnames = NULL) {
  if (is.vector(df) || is.factor(df)) {
    df = data.frame(df)
    if (!is.null(cnames)) colnames(df) = cnames
    return(df)
  }
  # df is a data frame
  if (is.null(cnames)) {
    if (is.null(cselect())) {
      return(df)
    } else {
      cnames = colnames(df)[cselect]
    }
  }
  cols = data.frame(df[, cnames])
  colnames(cols) = cnames
  cols
}

# covs is a vector or a data frame with covariates
# returns a data frame with encoded covariates (model matrix)
encode_covariates = function(covs) {
  df.covs = data.frame(model.matrix(~ ., covs))
  cnames = colnames(df.covs)
  df.covs = data.frame(df.covs[, -1]) # bez intercepta
  colnames(df.covs) = cnames[-1]
  df.covs
}

# alt is a vector of alternatives
# returns a matrix with encoded alternatives (best or worst choice depending on the best parameter)
encode_items = function(alt, items = factor(designctx$items), best = TRUE) {
  items_coded = matrix(0, nrow = length(alt), ncol = length(items))
  colnames(items_coded) = make.names(items)
  enc = 1
  if (!best) enc = -1
  for (i in 1:length(alt)) {
    items_coded[i, make.names(as.character(alt[i]))] = enc
  }
  items_coded
}

# get anchors questions
getanchors = function(num_anchors, best_choices, worst_choices) {
  anchors = c()
  bests = unlist(lapply(unique(best_choices), function(bc) {
    setNames(as.list(sum(best_choices == bc)), bc)
  }))
  worsts = unlist(lapply(unique(worst_choices), function(bc) {
    setNames(as.list(sum(worst_choices == bc)), bc)
  }))
  bests[order(bests, decreasing = TRUE)]
  worsts[order(worsts, decreasing = TRUE)]
  
  last_added = "worst"
  i_bests = i_worsts = 1
  while (length(anchors) < num_anchors) {
    if (last_added == "worst") {
      if (length(bests) >= i_bests) {
        el = names(bests[i_bests])
        if (!(el %in% anchors)) {
          anchors = c(anchors, el)
          last_added = "best"
        }
        i_bests = i_bests + 1
      } else {
        if (length(worsts) >= i_worsts) {
          last_added = "best" # ok, ima ih još za dodati pa nastavljamo s worsts
        } else {
          break # nema više ni jednih ni drugih, stopiramo
        }
      }
    } else {
      if (length(worsts) >= i_worsts) {
        el = names(worsts[i_worsts])
        if (!(el %in% anchors)) {
          anchors = c(anchors, el)
          last_added = "worst"
        }
        i_worsts = i_worsts + 1
      } else {
        if (length(bests) >= i_bests) {
          last_added = "worst" # ok, ima ih još za dodati pa nastavljamo s bests
        } else {
          break # nema više ni jednih ni drugih, stopiramo
        }
      }
    }
  }
  anchors
}

# create anchor column names for survey results
getanchorcolnames = function(nanchors) {
  paste("anchor_", 1:nanchors, sep = "")
}

