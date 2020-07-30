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
sdf = data.frame()

## <preprocess>
precols = c("id", "token", "submitdate", "startlanguage", "seed")
processcols = c("lastpage", "startdate", "datestamp", "ipaddr")
for (svid in surveyids) {
  questionnaireid = min(limesurveyqids[limesurveyqids$limesurveyid == svid, ]$vers) # they are all the same, take min, max, first, last or whatever
  sfile = paste("lsuw_survey_", svid, ".csv", sep = "")
  sdf0 = read.csv(sfile, header = TRUE, sep = ",", quote = "\"", stringsAsFactors = FALSE)
#  quids = make.names(limesurveyqids[limesurveyqids$limesurveyid == svid, ]$limesurveyqid_best)
#  quids = c(quids, make.names(limesurveyqids[limesurveyqids$limesurveyid == svid, ]$limesurveyqid_worst))
#  if (designctx$anchors > 0) {
#    anchquids = make.names(limesurveyanchorqids[limesurveyanchorqids$vers == questionnaireid, -1]) # -1 da maknemo vers
#    quids = c(quids, anchquids)
#  }
#  if (length(designctx$covariates) > 0) {
#    covquids = make.names(limesurveycovqids[limesurveycovqids$vers == questionnaireid, -1]) # -1 da maknemo vers
#    quids = c(quids, covquids)
#  }
#  if (length(designctx$personals) > 0) {
#    persquids = make.names(limesurveypersqids[limesurveypersqids$vers == questionnaireid, -1]) # -1 da maknemo vers
#    quids = c(quids, persquids)
#  }
#  sdf = sdf[, colnames(sdf) %in% quids]
  sdf0 = sdf0[, !(colnames(sdf0) %in% precols)] # ove ne trebamo

  # change the column names to be able to rowbind all the answers into 1 frame, this also assumes that
  # all questionnaires have the same number of questions
  varcols = which(!(colnames(sdf0) %in% processcols))
  colnames(sdf0)[varcols] = paste("X", 1:length(varcols), sep = "")
  sdf = rbind(sdf, cbind(questionnaireid = rep(questionnaireid, nrow(sdf0)), sdf0))
}

# # maknuti moju IP adresu
# sdf = sdf[sdf$ipaddr != "141.138.38.145", ]
#
# # sortiramo po poeetnom vremenu
# sdf = sdf[order(as.POSIXlt.character(x = sdf$startdate)), ]
#
# # provjeriti dupliciranje IP adrese, u dupls ostaviti one koji su stvarno duplicirani, a u dupls2 staviti one koji
# # su ipak ok
# dupls = sdf[sdf$ipaddr %in% sdf[duplicated(sdf$ipaddr), ]$ipaddr, ]
# View(dupls)
#
# readline(prompt="Press [enter] to continue")
#
# # OVE NE MICEMO JER SMO IH PROVJERILI!!!
# dupls2 = c("47", "248", "39", "119", "205", "112", "202", "238", "63", "53", "130", "51", "134", "122", "215",
#            "1", "83", "192", "18", "124", "50", "222", "52", "6", "254", "132", "228", "247", "151", "221", "140",
#            "11", "89", "186", "260", "154", "91", "187", "37", "118", "66", "195", "23", "99", "208", "173",
#            "43", "206")
#
# dupls = dupls[!(rownames(dupls) %in% dupls2), ]
#
# write.csv2(dupls, "duplicated.csv")
# # duplikate ne priznajemo bez obzira na to da li su ispunili sve ili ne
# sdf = sdf[!(rownames(sdf) %in% rownames(dupls)), ]
#
# # izvuai e-mailove za poklon bonove
# emails = sdf[, ncol(sdf)] # pretpostavka da je zadnji stupac e-mail, ispraviti ako nije
# emails = emails[emails != "NULL"]
# emails = emails[emails != ""]
# emails = emails[!duplicated(emails)]
# write.csv2(emails, "emails.csv")
#
# # u analizu ukljueiti sve koji imaju 18 odgovora (samo fali mail) i 19 odgovora
# sdf = sdf[sdf$lastpage %in% c(18, 19), ]
#
# # provjeriti vrijeme ispunjavanja upitnika (prosjek se eini oko 4-5 minuta)
# sdf$surveyduration = as.numeric(as.POSIXlt.character(x = sdf$datestamp) - as.POSIXlt.character(x = sdf$startdate))
# tooquicks = sdf[sdf$surveyduration < 2, ] # minimalno oeekujemo 2 minute za ispunjavanje upitnika
# write.csv2(tooquicks, "tooquicks.csv")
# sdf = sdf[sdf$surveyduration >= 2, !(colnames(sdf) %in% c(processcols, "surveyduration"))] # ostaviti samo stupce koje trebamo
#
# ## </preprocess>


sdf = sdf[, !(colnames(sdf) %in% processcols)] # ove ne trebamo


limesurvey.df = sdf
# here we assume that each row is a survey filled out by a different responder
#limesurvey.df = cbind(resp.id = as.numeric(rownames(limesurvey.df)), limesurvey.df)
limesurvey.df = cbind(resp.id = 1:nrow(limesurvey.df), limesurvey.df)

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
  # start from 3+length(designctx$screenings) because 1st is resp.id and 2nd is questionnaireid
  # + 2*nquestions-1 because we have two subquestions/answers (best and worst choice) to each question
  # + anchors because we have this many anchor questions, also A encoded 
  # + length(covariates) because the covariates answers are also A encoded
  choices = as.numeric(gsub("A", "", x[, (3+length(designctx$screenings)):((3+length(designctx$screenings))+2*designctx$nquestions-1+designctx$anchors+length(designctx$covariates))]))

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
  
  if (length(designctx$covariates) > 0) {
    # covariates are at the end of the survey (not taking personals into account), if not, change this!
    ### OVDJE SMO STAVILI COVARIATES NA POCETAK JER SU SLU?ILI I ZA SCREENING!!!
    #fstcovchoice = 1# length(choices) - length(designctx$covariates) + 1
    #covchoicesi = fstcovchoice:(fstcovchoice+length(designctx$covariates)-1)
    #covchoices = choices[covchoicesi]
    #choices = choices[(max(covchoicesi)+1):length(choices)] # choices[1:(fstcovchoice-1)]

    fstcovchoice = length(choices) - length(designctx$covariates) + 1
    covchoices = choices[fstcovchoice:length(choices)]
    covsdf = data.frame(designctx$fullfact_covdesign[rep(1, nrow(sdf)), , drop = FALSE])
    for (coli in 1:ncol(covsdf)) {
      cname = colnames(covsdf)[coli]
      covsdf[, cname] = levels(covsdf[[cname]])[covchoices[coli]]
    }
    sdf = cbind(sdf, covsdf)
  }
  
  if (designctx$anchors > 0) {
    fstanchchoice = length(choices) - length(designctx$covariates) - designctx$anchors + 1
    anchchoices = choices[fstanchchoice:(fstanchchoice+designctx$anchors-1)]
    anchors.df = data.frame(matrix(rep(anchchoices, nrow(sdf)), nrow = nrow(sdf), ncol = length(anchchoices), byrow = TRUE))
    colnames(anchors.df) = getanchorcolnames(designctx$anchors)
    sdf = cbind(sdf, anchors.df)
  }
  
  if (length(designctx$personals) > 0) {
    personals.df = x[rep(1, nrow(sdf)), (ncol(x)-length(designctx$personals)+1):ncol(x), drop = FALSE]
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

# compare simulation with the parsed answers
if (FALSE) {
  sfile0 = gsub(".csv", "_0.csv", simulatesurveyfile)
  cbc.df0 = read.csv(sfile0, header = TRUE, sep = ",", quote = "\"", stringsAsFactors = FALSE)

  if (!is.null(cbc.df0$questionnaire.id)) cbc.df0$questionnaire.id = NULL
  cbc.df$alt = as.character(cbc.df$alt) # we want string comparison, not factor
  # if there are other factors, convert them to strings
  
  if (nrow(cbc.df) != nrow(cbc.df0)) print("ERR")
  if (ncol(cbc.df) != ncol(cbc.df0)) print("ERR")
  if (paste(colnames(cbc.df), collapse = "+") != paste(colnames(cbc.df0), collapse = "+")) print("ERR")
  for (c in colnames(cbc.df)) {
    for (i in 1:length(cbc.df[[c]])) if (cbc.df[[c]][i] != cbc.df0[[c]][i]) print("ERR")
  }
  
}
