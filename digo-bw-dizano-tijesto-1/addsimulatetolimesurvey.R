source("surveyconfig.R")

# save the simulated answers to limesurvey
# UPITNICI MORAJU BITI AKTIVIRANI!!!
library(limer)

designctx = readRDS(designctxfile)
cbc.df = read.csv(file = simulatesurveyfile, stringsAsFactors = FALSE)

for (cov in designctx$covariates) {
  cbc.df[[cov$naziv]] = factor(cbc.df[[cov$naziv]], levels = cov$vrijednosti)
}

for (per in designctx$personals) {
  if (per$tip == "dropdown") cbc.df[[per$naziv]] = factor(cbc.df[[per$naziv]], levels = per$vrijednosti)
}

cbc.df$alti = rep(1:designctx$nalternatives, nrow(cbc.df)/designctx$nalternatives)

limesurveyallqids = readRDS(limesurveyqidsfile)
limesurveyqids = limesurveyallqids$qids
limesurveycovqids = limesurveyallqids$covqids
limesurveypersqids = limesurveyallqids$persquids
limesurveyanchorqids = limesurveyallqids$anchorquids

get_session_key()

respondents = as.numeric(levels(factor(cbc.df$resp.id)))
ret = lapply(respondents, function(r) {
  
  answlist = list()

  # best answers/choices
  answers.df_best = cbc.df[(cbc.df$resp.id == r) & (cbc.df$best_choice == 1), ]
  questionnaireid = (answers.df_best$questionnaire.id)[1]
  lsids = limesurveyqids[limesurveyqids$version == questionnaireid, ]
  iSurveyID = (lsids$limesurveyid)[1]
#  answers.df_best$limesurveyqid = lsids[lsids$task == answers.df_best$ques, ]$limesurveyqid_best
  answers.df_best$limesurveyqid = lsids$limesurveyqid_best
  answers.df_best$answer = paste("A", answers.df_best$alti, sep = "")
  rr = lapply(1:nrow(answers.df_best), function(i) {
    answlist[[answers.df_best[i, ]$limesurveyqid]] <<- answers.df_best[i, ]$answer
  })
  
  # worst answers/choices
  answers.df_worst = cbc.df[(cbc.df$resp.id == r) & (cbc.df$worst_choice == 1), ]
  questionnaireid = (answers.df_worst$questionnaire.id)[1]
  lsids = limesurveyqids[limesurveyqids$version == questionnaireid, ]
  iSurveyID = (lsids$limesurveyid)[1]
#  answers.df_worst$limesurveyqid = lsids[lsids$task == answers.df_worst$ques, ]$limesurveyqid_worst
  answers.df_worst$limesurveyqid = lsids$limesurveyqid_worst
  answers.df_worst$answer = paste("A", answers.df_worst$alti, sep = "")
  rr = lapply(1:nrow(answers.df_worst), function(i) {
    answlist[[answers.df_worst[i, ]$limesurveyqid]] <<- answers.df_worst[i, ]$answer
  })

  # anchors answers
  if (designctx$anchors > 0){
    lsids = data.frame(limesurveyanchorqids[limesurveyanchorqids$vers == questionnaireid, -1])
    colnames(lsids) = colnames(limesurveyanchorqids)[-1]
    # svejedno da li uzmemo answers.df_best ili answers.df_worst
    answs = paste("A", as.numeric(answers.df_best[1, colnames(answers.df_best) %in% colnames(lsids)]), sep = "")
    lsids = data.frame(t(lsids))
    colnames(lsids) = "Q"
    lsids$A = answs
    for (ii in 1:nrow(lsids)) {
      answlist[[as.character(lsids[ii, "Q"])]] = as.character(lsids[ii, "A"])
    }
  }
  
  # covariates answers
  if (length(designctx$covariates) > 0){
    lsids = data.frame(limesurveycovqids[limesurveycovqids$vers == questionnaireid, -1])
    colnames(lsids) = colnames(limesurveycovqids)[-1]
    # svejedno da li uzmemo answers.df_best ili answers.df_worst
    answs = paste("A", as.numeric(answers.df_best[1, colnames(answers.df_best) %in% colnames(lsids)]), sep = "")
    lsids = data.frame(t(lsids))
    colnames(lsids) = "Q"
    lsids$A = answs
    rr = lapply(1:nrow(lsids), function(i) {
      answlist[[as.character(lsids[i, "Q"])]] <<- as.character(lsids[i, "A"])
    })
  }

  # personal answers
  if (length(designctx$personals) > 0){
    lsids = data.frame(limesurveypersqids[limesurveypersqids$vers == questionnaireid, -1])
    colnames(lsids) = colnames(limesurveypersqids)[-1]
    # svejedno da li uzmemo answers.df_best ili answers.df_worst
    answs = rep("", ncol(lsids))
    for (i in 1:length(designctx$personals)) {
      p = designctx$personals[i]
      n = names(p)
      if (p[[n]][["tip"]] == "email") {
        answs[i] = answers.df_best[1, n]
      } else if (p[[n]][["tip"]] == "dropdown") {
        answs[i] = paste("A", as.numeric(answers.df_best[1, n]), sep = "")
      }
    }
    lsids = data.frame(t(lsids))
    colnames(lsids) = "Q"
    lsids$A = answs
    rr = lapply(1:nrow(lsids), function(i) {
      answlist[[as.character(lsids[i, "Q"])]] <<- as.character(lsids[i, "A"])
    })
  }
  
  # answlist = list(
  #   "675766X55X823qi5qu1sq1" = "A1",
  #   "675766X55X825qi5qu2sq1" = "A2",
  #   "675766X55X827qi5qu3sq1" = "A2",
  #   "675766X55X829qi5qu4sq1" = "A1",
  #   "675766X55X831qi5qu5sq1" = "A2",
  #   "675766X55X833qi5qu6sq1" = "A1",
  #   "675766X55X835qi5qu7sq1" = "A2",
  #   "675766X55X837qi5qu8sq1" = "A2",
  #   "675766X55X839qi5qu9sq1" = "A1",
  #   "675766X55X841qi5qu10sq1" = "A2",
  #   # "submitdate" = "2013-03-02 09:54:59",
  #   # "startlanguage" = "hr",
  #   # "datestamp" = "2013-03-02 09:54:59",
  #   # "startdate" = "2013-03-02 09:54:59",
  #   "lastpage" = "10"# = nquestions,
  #   # "seed" = "1369869259"
  # )
  
  answ = call_limer(method = 'add_response', params = list("iSurveyID" = iSurveyID, "aResponseData" = answlist))
  
})

