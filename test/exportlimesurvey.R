# we assume that lime survey answers are exported from SQL (PHP my admin) to CSV (NOT CSV FOR MICROSOFT!!!) 
# files in the working direcory and with the naming convention: lime_survey_SURVEYID.csv

source("surveyconfig.R")

limesurveyallqids = readRDS(limesurveyqidsfile)
limesurveyqids = limesurveyallqids$qids
limesurveycovqids = limesurveyallqids$covqids
limesurveypersqids = limesurveyallqids$persquids
limesurveyanchorqids = limesurveyallqids$anchorquids

designctx = readRDS(designctxfile)

surveyids = levels(factor(limesurveyqids$limesurveyid))
limesurvey.df = data.frame()
for (svid in surveyids) {
  questionnaireid = min(limesurveyqids[limesurveyqids$limesurveyid == svid, ]$vers) # they are all the same, take min, max, first, last or whatever
  sfile = paste("lsuw_survey_", svid, ".csv", sep = "")
  sdf = read.csv(sfile, header = TRUE, sep = ",", quote = "\"", stringsAsFactors = FALSE)
  quids = make.names(limesurveyqids[limesurveyqids$limesurveyid == svid, ]$limesurveyqid_best)
  quids = c(quids, make.names(limesurveyqids[limesurveyqids$limesurveyid == svid, ]$limesurveyqid_worst))
  if (designctx$anchors > 0) {
    anchquids = make.names(limesurveyanchorqids[limesurveyanchorqids$vers == questionnaireid, -1]) # -1 da maknemo vers
    quids = c(quids, anchquids)
  }
  if (length(designctx$covariates) > 0) {
    covquids = make.names(limesurveycovqids[limesurveycovqids$vers == questionnaireid, -1]) # -1 da maknemo vers
    quids = c(quids, covquids)
  }
  if (length(designctx$personals) > 0) {
    persquids = make.names(limesurveypersqids[limesurveypersqids$vers == questionnaireid, -1]) # -1 da maknemo vers
    quids = c(quids, persquids)
  }
  sdf = sdf[, colnames(sdf) %in% quids]
  # change the column names to be able to rowbind all the answers into 1 frame, this also assumes that
  # all questionnaires have the same number of questions
  colnames(sdf) = paste("X", 1:ncol(sdf), sep = "")
  limesurvey.df = rbind(limesurvey.df, cbind(questionnaireid = rep(questionnaireid, nrow(sdf)), sdf))
}
# here we assume that each row is a survey filled out by a different responder
limesurvey.df = cbind(resp.id = as.numeric(rownames(limesurvey.df)), limesurvey.df)

cbc.df = data.frame()
for (r in 1:nrow(limesurvey.df)) {
  x = limesurvey.df[r, ]
  sdf = designctx$survey[designctx$survey$version == x$questionnaireid, c("task", "alt")] # remove version because we don't need it
  colnames(sdf) = gsub("task", "ques", colnames(sdf)) # rename task to ques
  sdf = cbind(resp.id = rep(x$resp.id, nrow(sdf)), sdf)
  alti = rep(1:designctx$nalternatives, nrow(sdf)/designctx$nalternatives)
  # here we assume that columns go as follows:
  # resp.id questionnaireid answers anchors covariates personals
  # strip A from answer codes to leave only the alternative number
  # start from 3 because 1st is resp.id and 2nd is questionnaireid
  # + 2*nquestions-1 because we have two subquestions/answers (best and worst choice) to each question
  # + anchors because we have this many anchor questions, also A encoded 
  # + length(covariates) because the covariates answers are also A encoded
  choices = as.numeric(gsub("A", "", x[, 3:(3+2*designctx$nquestions-1+designctx$anchors+length(designctx$covariates))]))

  # we assume that the best choice is the first option/subquestion/answer, if not change this!
  best_choices = choices[2*(1:designctx$nquestions)-1]
  # we assume that the worst choice is the second option/subquestion/answer, if not change this!
  worst_choices = choices[2*(1:designctx$nquestions)]
  # if (any(best_choices == worst_choices)) stop("best and worst choice cannot be the same")
  best_choices = rep(best_choices, each = designctx$nalternatives)
  worst_choices = rep(worst_choices, each = designctx$nalternatives)
  # match alternative numbers with choices to set the chosen alternatives
  best_choice_col = as.numeric(alti == best_choices)
  worst_choice_col = as.numeric(alti == worst_choices)
  
  if (designctx$anchors > 0) {
    fstanchchoice = length(choices) - length(designctx$covariates) - designctx$anchors + 1
    anchchoices = choices[fstanchchoice:(fstanchchoice+designctx$anchors-1)]
    anchors.df = data.frame(matrix(rep(anchchoices, nrow(sdf)), nrow = nrow(sdf), ncol = length(anchchoices), byrow = TRUE))
    colnames(anchors.df) = getanchorcolnames(designctx$anchors)
    sdf = cbind(sdf, anchors.df)
  }
  
  if (length(designctx$covariates) > 0) {
    # covariates are at the end of the survey (not taking personals into account), if not, change this!
    fstcovchoice = length(choices) - length(designctx$covariates) + 1
    covchoices = choices[fstcovchoice:length(choices)]
    covsdf = data.frame(designctx$fullfact_covdesign[rep(1, nrow(sdf)), ])
    colnames(covsdf) = colnames(designctx$fullfact_covdesign)
    for (coli in 1:ncol(covsdf)) {
      cname = colnames(covsdf)[coli]
      covsdf[, cname] = levels(covsdf[[cname]])[covchoices[coli]]
    }
    sdf = cbind(sdf, covsdf)
  }
  
  if (length(designctx$personals) > 0) {
    personals.df = data.frame(x[rep(1, nrow(sdf)), (ncol(x)-length(designctx$personals)+1):ncol(x)])
    colnames(personals.df) = names(designctx$personals)
    for (coli in (1:ncol(personals.df))) {
      pname = colnames(personals.df)[coli]
      if (designctx$personals[[pname]][["tip"]] == "dropdown") {
        pindex = as.numeric(gsub("A", "", personals.df[1, pname])) # svi su isti pa uzmemo npr. prvog
        personals.df[, pname] = designctx$personals[[pname]][["vrijednosti"]][pindex]
      } else {
        # druga varijanta je e-mail, koji ne mijenjamo, ostalo ne handleamo pa ne mijenjamo
      }
    }
    sdf = cbind(sdf, personals.df)
  }
  
  cbc.df = rbind(cbc.df, cbind(sdf, best_choice = best_choice_col, worst_choice = worst_choice_col))
}

write.csv(cbc.df, row.names = FALSE, file = answersfile)
