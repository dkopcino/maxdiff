# https://manual.limesurvey.org/Workarounds:_Question_design,_layout_and_templating#Create_MaxDiff_question_type

source("surveyconfig.R")

designctx = readRDS(designctxfile)

# if(!require("devtools")) {
#   install.packages("devtools")
#   library("devtools")
# }
# install_github("cloudyr/limer")
# #############################################################

library(limer)
library(XML)

# limesurvey questions ids for later automatic responses adding
#limesurveyqids = designctx$survey[, c("card", "vers", "task")]
limesurveyqids = designctx$survey[, c("version", "task")]
limesurveyqids$limesurveyid = ""
limesurveyqids$limesurveyqid_best = ""
limesurveyqids$limesurveyqid_worst = ""
# questions with anchors, vers is 1:nquestionnaires
limesurveyanchorqids = data.frame()
if (length(designctx$anchors) > 0) {
  limesurveyanchorqids = data.frame(vers = 1:designctx$nquestionnaires)
  ret = lapply(1:designctx$anchors, function(ai) {
    limesurveyanchorqids[[paste("anchor_", ai, sep = "")]] <<- ""
  })
}
# questions with covariates, vers is 1:nquestionnaires
limesurveycovqids = data.frame()
if (length(designctx$covariates) > 0) {
  limesurveycovqids = data.frame(vers = 1:designctx$nquestionnaires)
  ret = lapply(names(designctx$covariates), function(cc) {
    limesurveycovqids[[cc]] <<- ""
  })
}
# personal questions, vers is 1:nquestionnaires
limesurveypersqids = data.frame()
if (length(designctx$personals) > 0) {
  limesurveypersqids = data.frame(vers = 1:designctx$nquestionnaires)
  ret = lapply(names(designctx$personals), function(cc) {
    limesurveypersqids[[cc]] <<- ""
  })
}

# first get a session access key
get_session_key()

# first 3 columns are card (choice set), vers (questionnaire), task (alternatives)
questionnaires = lapply(1:designctx$nquestionnaires, function(nq) designctx$survey[designctx$survey$vers == nq, ])

## if we use template file for creating surveys without questions, here we set their survey ids
## this must be either empty or of the same length as questionnaires
# createdsurveyids = c("926214", "519995", "197659")
createdsurveyids = c()

for (i in 1:length(questionnaires)) {

  qi = questionnaires[[i]]
  
  question_subquestion_ids = c()
  
  if (length(createdsurveyids) == 0) {
    # iSurveyID = paste("survey", cbcid, i, sep = "_")
    # sSurveyTitle  = paste("TEST 1")
    # sSurveyLanguage = 'hr'
    # sformat = 'S'; # A for All in one page, G for Group by Group, S for Single questions
    # #add_survey(string $sSessionKey, integer $iSurveyID, string $sSurveyTitle, string $sSurveyLanguage, string $sformat = 'G') : integer|array
    # iNewID = call_limer(method = 'add_survey',
    #                     params = list("iSurveyID" = iSurveyID,
    #                                   "sSurveyTitle" = sSurveyTitle,
    #                                   "sSurveyLanguage" = sSurveyLanguage,
    #                                   "sformat" = sformat))
    surveyfile = readr::read_file("limesurvey_survey_XXXXXX.lss")
    sImportData = openssl::base64_encode(surveyfile)
    sImportDataType = 'lss'
    iNewID = call_limer(method = 'import_survey',
                        params = list("sImportData" = sImportData,
                                     "sImportDataType" = sImportDataType))
  } else {
    iNewID = createdsurveyids[i]
  }
  
  sGroupTitle = enc2utf8('Molimo odaberite Vama najbolju i najlošiju opciju među navedenim opcijama')
  sGroupDescription = ''
  #add_group(string $sSessionKey, integer $iSurveyID, string $sGroupTitle, string $sGroupDescription = '') : array|integer
  iGid2 = call_limer(method = 'add_group', 
                     params = list("iSurveyID" = iNewID,
                                   "sGroupTitle" = sGroupTitle,
                                   "sGroupDescription" = sGroupDescription))
  
  nquestions = designctx$nquestions
  nalternatives = designctx$nalternatives
  
  lastquestionid = 0
  
  for (j in 1:nquestions) {
    
    doc = newXMLDoc()
    documentNode = newXMLNode("document", doc = doc)
    LimeSurveyDocTypeNode = newXMLNode("LimeSurveyDocType", parent = documentNode)
    xmlValue(LimeSurveyDocTypeNode) = "Question"
    DBVersionNode = newXMLNode("DBVersion", parent = documentNode)
    xmlValue(DBVersionNode) = 359
    
    languagesNode = newXMLNode("languages", parent = documentNode)
    languageNode = newXMLNode("language", parent = languagesNode)
    xmlValue(languageNode) = "hr"
    
    ## questions node
    questionsNode = newXMLNode("questions", parent = documentNode)
    fieldsNode = newXMLNode("fields", parent = questionsNode)
    
    qidNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(qidNode) = "qid"
    parent_qidNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(parent_qidNode) = "parent_qid"
    sidNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(sidNode) = "sid"
    gidNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(gidNode) = "gid"
    typeNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(typeNode) = "type"
    titleNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(titleNode) = "title"
    questionNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(questionNode) = "question"
    pregNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(pregNode) = "preg"
    helpNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(helpNode) = "help"
    otherNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(otherNode) = "other"
    mandatoryNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(mandatoryNode) = "mandatory"
    question_orderNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(question_orderNode) = "question_order"
    languageNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(languageNode) = "language"
    scale_idNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(scale_idNode) = "scale_id"
    same_defaultNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(same_defaultNode) = "same_default"
    relevanceNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(relevanceNode) = "relevance"
    modulenameNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(modulenameNode) = "modulename"
    
    rowsNode = newXMLNode("rows", parent = questionsNode)
    rowNode = newXMLNode("row", parent = rowsNode)
    
    qidNode = newXMLNode("qid", parent = rowNode)
    lastquestionid = lastquestionid + 1
    thisquestionid = lastquestionid
    cdatanode = newXMLCDataNode(lastquestionid, parent = qidNode)
    parent_qidNode = newXMLNode("parent_qid", parent = rowNode)
    cdatanode = newXMLCDataNode(0, parent = parent_qidNode)
    sidNode = newXMLNode("sid", parent = rowNode)
    cdatanode = newXMLCDataNode(iNewID, parent = sidNode)
    gidNode = newXMLNode("gid", parent = rowNode)
    cdatanode = newXMLCDataNode(iGid2, parent = gidNode)
    typeNode = newXMLNode("type", parent = rowNode)
    cdatanode = newXMLCDataNode("H", parent = typeNode)
    
    sNewQuestionTitle1 = paste("qi", i, "qu", thisquestionid, sep = "") # ovo je kod u LimeSurvey i mora biti jedinstven na nivou surveya
    sNewqQuestion1 = paste(enc2utf8("Opcije:"),
                           "<br/>",
                           "<script type=\"text/javascript\" charset=\"utf-8\">
                           $(document).ready(function(){
                           // Call the maxDiff() function
                           // Set the second parameter to true for randomized rows
                           maxDiff({QID}, true);
                           });
                           </script>")
    sNewQuestionHelp1 = enc2utf8("Kliknite na kružić (radio dugme) za odabir")

    titleNode = newXMLNode("title", parent = rowNode)
    cdatanode = newXMLCDataNode(sNewQuestionTitle1, parent = titleNode)
    questionNode = newXMLNode("question", parent = rowNode)
    cdatanode = newXMLCDataNode(sNewqQuestion1, parent = questionNode)
    pregNode = newXMLNode("preg", parent = rowNode)
    helpNode = newXMLNode("help", parent = rowNode)
    cdatanode = newXMLCDataNode(sNewQuestionHelp1, parent = helpNode)
    
    otherNode = newXMLNode("other", parent = rowNode)
    cdatanode = newXMLCDataNode("N", parent = otherNode)
    mandatoryNode = newXMLNode("mandatory", parent = rowNode)
    cdatanode = newXMLCDataNode("Y", parent = mandatoryNode)
    question_orderNode = newXMLNode("question_order", parent = rowNode)
    cdatanode = newXMLCDataNode(1, parent = question_orderNode)
    languageNode = newXMLNode("language", parent = rowNode)
    cdatanode = newXMLCDataNode("hr", parent = languageNode)
    scale_idNode = newXMLNode("scale_id", parent = rowNode)
    cdatanode = newXMLCDataNode(0, parent = scale_idNode)
    same_defaultNode = newXMLNode("same_default", parent = rowNode)
    cdatanode = newXMLCDataNode(0, parent = same_defaultNode)
    relevanceNode = newXMLNode("relevance", parent = rowNode)
    cdatanode = newXMLCDataNode(1, parent = relevanceNode)
    modulenameNode = newXMLNode("modulename", parent = rowNode)
    ## questions node
    
    ## subquestions node
    subquestionsNode = newXMLNode("subquestions", parent = documentNode)
    fieldsNode = newXMLNode("fields", parent = subquestionsNode)
    
    qidNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(qidNode) = "qid"
    parent_qidNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(parent_qidNode) = "parent_qid"
    sidNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(sidNode) = "sid"
    gidNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(gidNode) = "gid"
    typeNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(typeNode) = "type"
    titleNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(titleNode) = "title"
    questionNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(questionNode) = "question"
    pregNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(pregNode) = "preg"
    helpNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(helpNode) = "help"
    otherNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(otherNode) = "other"
    mandatoryNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(mandatoryNode) = "mandatory"
    question_orderNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(question_orderNode) = "question_order"
    languageNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(languageNode) = "language"
    scale_idNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(scale_idNode) = "scale_id"
    same_defaultNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(same_defaultNode) = "same_default"
    relevanceNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(relevanceNode) = "relevance"
    modulenameNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(modulenameNode) = "modulename"
    
    rowsNode = newXMLNode("rows", parent = subquestionsNode)
    
    # subquestion 1
    rowNode = newXMLNode("row", parent = rowsNode)
    qidNode = newXMLNode("qid", parent = rowNode)
    lastquestionid = lastquestionid + 1
    cdatanode = newXMLCDataNode(lastquestionid, parent = qidNode)
    parent_qidNode = newXMLNode("parent_qid", parent = rowNode)
    cdatanode = newXMLCDataNode(thisquestionid, parent = parent_qidNode)
    sidNode = newXMLNode("sid", parent = rowNode)
    cdatanode = newXMLCDataNode(iNewID, parent = sidNode)
    gidNode = newXMLNode("gid", parent = rowNode)
    cdatanode = newXMLCDataNode(iGid2, parent = gidNode)
    typeNode = newXMLNode("type", parent = rowNode)
    cdatanode = newXMLCDataNode("T", parent = typeNode)
    
    sNewSubQuestionTitle1 = paste(sNewQuestionTitle1, "sq1", sep = "") # ovo je kod u LimeSurvey i mora biti jedinstven na nivou surveya
    sNewqSubQuestion1 = enc2utf8("NAJBOLJA")
    
    # save if anchor is used
    question_subquestion_ids = 
      c(question_subquestion_ids, paste(sNewQuestionTitle1, "_", sNewSubQuestionTitle1, ".shown", sep = ""))
    
    titleNode = newXMLNode("title", parent = rowNode)
    cdatanode = newXMLCDataNode(sNewSubQuestionTitle1, parent = titleNode)
    questionNode = newXMLNode("question", parent = rowNode)
    cdatanode = newXMLCDataNode(sNewqSubQuestion1, parent = questionNode)
    # pregNode = newXMLNode("preg", parent = rowNode)
    # xmlValue(pregNode) = "preg"
    # helpNode = newXMLNode("help", parent = rowNode)
    # xmlValue(helpNode) = sNewQuestionHelp1
    
    otherNode = newXMLNode("other", parent = rowNode)
    cdatanode = newXMLCDataNode("N", parent = otherNode)
    # mandatoryNode = newXMLNode("mandatory", parent = rowNode)
    # xmlValue(mandatoryNode) = "Y"
    question_orderNode = newXMLNode("question_order", parent = rowNode)
    cdatanode = newXMLCDataNode(1, parent = question_orderNode)
    languageNode = newXMLNode("language", parent = rowNode)
    cdatanode = newXMLCDataNode("hr", parent = languageNode)
    scale_idNode = newXMLNode("scale_id", parent = rowNode)
    cdatanode = newXMLCDataNode(0, parent = scale_idNode)
    same_defaultNode = newXMLNode("same_default", parent = rowNode)
    cdatanode = newXMLCDataNode(0, parent = same_defaultNode)
    relevanceNode = newXMLNode("relevance", parent = rowNode)
    cdatanode = newXMLCDataNode(1, parent = relevanceNode)
    modulenameNode = newXMLNode("modulename", parent = rowNode)
    #    xmlValue(modulenameNode) = "modulename"
    
    
    # subquestion 2
    rowNode = newXMLNode("row", parent = rowsNode)
    qidNode = newXMLNode("qid", parent = rowNode)
    lastquestionid = lastquestionid + 1
    cdatanode = newXMLCDataNode(lastquestionid, parent = qidNode)
    parent_qidNode = newXMLNode("parent_qid", parent = rowNode)
    cdatanode = newXMLCDataNode(thisquestionid, parent = parent_qidNode)
    sidNode = newXMLNode("sid", parent = rowNode)
    cdatanode = newXMLCDataNode(iNewID, parent = sidNode)
    gidNode = newXMLNode("gid", parent = rowNode)
    cdatanode = newXMLCDataNode(iGid2, parent = gidNode)
    typeNode = newXMLNode("type", parent = rowNode)
    cdatanode = newXMLCDataNode("T", parent = typeNode)
    
    sNewSubQuestionTitle2 = paste(sNewQuestionTitle1, "sq2", sep = "") # ovo je kod u LimeSurvey i mora biti jedinstven na nivou surveya
    sNewSubQuestion2 = enc2utf8("NAJLOŠIJA")
    
    # save if anchor is used
    question_subquestion_ids = 
      c(question_subquestion_ids, paste(sNewQuestionTitle1, "_", sNewSubQuestionTitle2, ".shown", sep = ""))
    
    titleNode = newXMLNode("title", parent = rowNode)
    cdatanode = newXMLCDataNode(sNewSubQuestionTitle2, parent = titleNode)
    questionNode = newXMLNode("question", parent = rowNode)
    cdatanode = newXMLCDataNode(sNewSubQuestion2, parent = questionNode)
    # pregNode = newXMLNode("preg", parent = rowNode)
    # xmlValue(pregNode) = "preg"
    # helpNode = newXMLNode("help", parent = rowNode)
    # xmlValue(helpNode) = sNewQuestionHelp1
    
    otherNode = newXMLNode("other", parent = rowNode)
    cdatanode = newXMLCDataNode("N", parent = otherNode)
    # mandatoryNode = newXMLNode("mandatory", parent = rowNode)
    # xmlValue(mandatoryNode) = "Y"
    question_orderNode = newXMLNode("question_order", parent = rowNode)
    cdatanode = newXMLCDataNode(2, parent = question_orderNode)
    languageNode = newXMLNode("language", parent = rowNode)
    cdatanode = newXMLCDataNode("hr", parent = languageNode)
    scale_idNode = newXMLNode("scale_id", parent = rowNode)
    cdatanode = newXMLCDataNode(0, parent = scale_idNode)
    same_defaultNode = newXMLNode("same_default", parent = rowNode)
    cdatanode = newXMLCDataNode(0, parent = same_defaultNode)
    relevanceNode = newXMLNode("relevance", parent = rowNode)
    cdatanode = newXMLCDataNode(1, parent = relevanceNode)
    modulenameNode = newXMLNode("modulename", parent = rowNode)
    #    xmlValue(modulenameNode) = "modulename"
    
    ## subquestions node
    
    
    ## answers node
    answersNode = newXMLNode("answers", parent = documentNode)
    fieldsNode = newXMLNode("fields", parent = answersNode)
    
    qidNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(qidNode) = "qid"
    codeNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(codeNode) = "code"
    answerNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(answerNode) = "answer"
    sortorderNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(sortorderNode) = "sortorder"
    assessment_valueNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(assessment_valueNode) = "assessment_value"
    languageNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(languageNode) = "language"
    scale_idNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(scale_idNode) = "scale_id"
    
    rowsNode = newXMLNode("rows", parent = answersNode)
    
    ## answers node
    
    alternatives = qi[qi$task == j, ]
    for (k in 1:nalternatives) {

      rowNode = newXMLNode("row", parent = rowsNode)
      
      qidNode = newXMLNode("qid", parent = rowNode)
      cdatanode = newXMLCDataNode(thisquestionid, parent = qidNode)
      codeNode = newXMLNode("code", parent = rowNode)
      cdatanode = newXMLCDataNode(paste("A", k, sep = ""), parent = codeNode) # ovo je ovisno o alternativi
      
      answerNode = newXMLNode("answer", parent = rowNode)
      cdatanode = newXMLCDataNode(enc2utf8(as.character(alternatives[k, "alt"])), parent = answerNode)
      
      sortorderNode = newXMLNode("sortorder", parent = rowNode)
      cdatanode = newXMLCDataNode(k, parent = sortorderNode)
      assessment_valueNode = newXMLNode("assessment_value", parent = rowNode)
      cdatanode = newXMLCDataNode(0, parent = assessment_valueNode)
      languageNode = newXMLNode("language", parent = rowNode)
      cdatanode = newXMLCDataNode("hr", parent = languageNode)
      scale_idNode = newXMLNode("scale_id", parent = rowNode)
      cdatanode = newXMLCDataNode(0, parent = scale_idNode)
      
    }
    
    
    questionString = saveXML(doc, file = NULL, compression = 0, indent = TRUE, 
                             prefix = '<?xml version="1.0" encoding="UTF-8"?>',
                             doctype = NULL, encoding = "UTF8")
    
    # readr::write_file(questionString, "out.xml")
    # questionString1 = readr::read_file("limesurvey_question_1.lsq")
    
    library(openssl)
    sImportData = base64_encode(questionString)
    sImportDataType = 'lsq'
    ### Sva su pitanja obavezna!
    sMandatory = 'Y'
    #import_question(string $sSessionKey, integer $iSurveyID, integer $iGroupID, string $sImportData, string $sImportDataType, string $sMandatory = 'N', string $sNewQuestionTitle = null, string $sNewqQuestion = null, string $sNewQuestionHelp = null) : array|integer    
    iQid1 = call_limer(method = 'import_question',
                       params = list("iSurveyID" = iNewID,
                                     "iGroupID" = iGid2, 
                                     "sImportData" = sImportData,
                                     "sImportDataType" = sImportDataType,
                                     "sMandatory" = sMandatory))
    
    # save new question id
    limesurveyqid_best = paste(iNewID, "X", iGid2, "X", iQid1, sNewSubQuestionTitle1, sep = "")
    limesurveyqid_worst = paste(iNewID, "X", iGid2, "X", iQid1, sNewSubQuestionTitle2, sep = "")
    limesurveyqids[(limesurveyqids$vers == i) & (limesurveyqids$task == j), ]$limesurveyqid_best = limesurveyqid_best
    limesurveyqids[(limesurveyqids$vers == i) & (limesurveyqids$task == j), ]$limesurveyqid_worst = limesurveyqid_worst
    
  }
  
  # save limesurvey survey id
  limesurveyqids[limesurveyqids$vers == i, ]$limesurveyid = iNewID
  
  
  # if anchors, add the question
  if (designctx$anchors > 0) {
    
    sGroupTitle = enc2utf8('Da li biste sljedeće proizvode općenito kupili ili ne?')
    sGroupDescription = enc2utf8('')
    iGid21 = call_limer(method = 'add_group', 
                        params = list("iSurveyID" = iNewID,
                                      "sGroupTitle" = sGroupTitle,
                                      "sGroupDescription" = sGroupDescription))
    
    
    doc = newXMLDoc()
    documentNode = newXMLNode("document", doc = doc)
    LimeSurveyDocTypeNode = newXMLNode("LimeSurveyDocType", parent = documentNode)
    xmlValue(LimeSurveyDocTypeNode) = "Question"
    DBVersionNode = newXMLNode("DBVersion", parent = documentNode)
    xmlValue(DBVersionNode) = 359
    
    languagesNode = newXMLNode("languages", parent = documentNode)
    languageNode = newXMLNode("language", parent = languagesNode)
    xmlValue(languageNode) = "hr"
    
    ## questions node
    questionsNode = newXMLNode("questions", parent = documentNode)
    fieldsNode = newXMLNode("fields", parent = questionsNode)
    
    qidNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(qidNode) = "qid"
    parent_qidNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(parent_qidNode) = "parent_qid"
    sidNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(sidNode) = "sid"
    gidNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(gidNode) = "gid"
    typeNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(typeNode) = "type"
    titleNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(titleNode) = "title"
    questionNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(questionNode) = "question"
    pregNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(pregNode) = "preg"
    helpNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(helpNode) = "help"
    otherNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(otherNode) = "other"
    mandatoryNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(mandatoryNode) = "mandatory"
    question_orderNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(question_orderNode) = "question_order"
    languageNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(languageNode) = "language"
    scale_idNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(scale_idNode) = "scale_id"
    same_defaultNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(same_defaultNode) = "same_default"
    relevanceNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(relevanceNode) = "relevance"
    modulenameNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(modulenameNode) = "modulename"
    
    rowsNode = newXMLNode("rows", parent = questionsNode)
    rowNode = newXMLNode("row", parent = rowsNode)
    
    qidNode = newXMLNode("qid", parent = rowNode)
    lastquestionid = lastquestionid + 1
    thisquestionid = lastquestionid
    cdatanode = newXMLCDataNode(lastquestionid, parent = qidNode)
    parent_qidNode = newXMLNode("parent_qid", parent = rowNode)
    cdatanode = newXMLCDataNode(0, parent = parent_qidNode)
    sidNode = newXMLNode("sid", parent = rowNode)
    cdatanode = newXMLCDataNode(iNewID, parent = sidNode)
    gidNode = newXMLNode("gid", parent = rowNode)
    cdatanode = newXMLCDataNode(iGid21, parent = gidNode)
    typeNode = newXMLNode("type", parent = rowNode)
    cdatanode = newXMLCDataNode("F", parent = typeNode)
    
    sAnchorQuestionTitle1 = paste("qi", i, "qu", thisquestionid, sep = "") # ovo je kod u LimeSurvey i mora biti jedinstven na nivou surveya
#    sAnchorQuestion1 = enc2utf8("Molimo odaberite što biste kupili a što ne biste:")
    sAnchorQuestion1 = paste(enc2utf8("Molimo odaberite što biste kupili a što ne biste:"),
                           "<br/>",
                           "<script type=\"text/javascript\" charset=\"utf-8\">
                           $(document).ready(function(){
                           // Call the arrangeAnchors() function
                           arrangeAnchors({QID});
                           });
                           </script>")
    sAnchorQuestionHelp1 = enc2utf8("Kliknite na kružić (radio dugme) za odabir")
    
    titleNode = newXMLNode("title", parent = rowNode)
    cdatanode = newXMLCDataNode(sAnchorQuestionTitle1, parent = titleNode)
    questionNode = newXMLNode("question", parent = rowNode)
    cdatanode = newXMLCDataNode(sAnchorQuestion1, parent = questionNode)
    pregNode = newXMLNode("preg", parent = rowNode)
    helpNode = newXMLNode("help", parent = rowNode)
    cdatanode = newXMLCDataNode(sAnchorQuestionHelp1, parent = helpNode)
    
    otherNode = newXMLNode("other", parent = rowNode)
    cdatanode = newXMLCDataNode("N", parent = otherNode)
    mandatoryNode = newXMLNode("mandatory", parent = rowNode)
    cdatanode = newXMLCDataNode("Y", parent = mandatoryNode)
    question_orderNode = newXMLNode("question_order", parent = rowNode)
    cdatanode = newXMLCDataNode(1, parent = question_orderNode)
    languageNode = newXMLNode("language", parent = rowNode)
    cdatanode = newXMLCDataNode("hr", parent = languageNode)
    scale_idNode = newXMLNode("scale_id", parent = rowNode)
    cdatanode = newXMLCDataNode(0, parent = scale_idNode)
    same_defaultNode = newXMLNode("same_default", parent = rowNode)
    cdatanode = newXMLCDataNode(0, parent = same_defaultNode)
    relevanceNode = newXMLNode("relevance", parent = rowNode)
    cdatanode = newXMLCDataNode(1, parent = relevanceNode)
    modulenameNode = newXMLNode("modulename", parent = rowNode)
    ## questions node
    
    ## subquestions node
    subquestionsNode = newXMLNode("subquestions", parent = documentNode)
    fieldsNode = newXMLNode("fields", parent = subquestionsNode)
    
    qidNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(qidNode) = "qid"
    parent_qidNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(parent_qidNode) = "parent_qid"
    sidNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(sidNode) = "sid"
    gidNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(gidNode) = "gid"
    typeNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(typeNode) = "type"
    titleNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(titleNode) = "title"
    questionNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(questionNode) = "question"
    pregNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(pregNode) = "preg"
    helpNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(helpNode) = "help"
    otherNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(otherNode) = "other"
    mandatoryNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(mandatoryNode) = "mandatory"
    question_orderNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(question_orderNode) = "question_order"
    languageNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(languageNode) = "language"
    scale_idNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(scale_idNode) = "scale_id"
    same_defaultNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(same_defaultNode) = "same_default"
    relevanceNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(relevanceNode) = "relevance"
    modulenameNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(modulenameNode) = "modulename"
    
    rowsNode = newXMLNode("rows", parent = subquestionsNode)
    
    # build subquestions
    # maxdiff1_SQ001.shown, maxdiff1_SQ002.shown, maxdiff2_SQ001.shown, maxdiff2_SQ002.shown...
    # question_subquestion_ids = c(
    #   "qi1qu1_qi1qu1sq1.shown", "qi1qu1_qi1qu1sq2.shown",
    #   "qi1qu2_qi1qu2sq1.shown", "qi1qu2_qi1qu2sq2.shown",
    #   "qi1qu3_qi1qu3sq1.shown", "qi1qu3_qi1qu3sq2.shown",
    #   "qi1qu4_qi1qu4sq1.shown", "qi1qu4_qi1qu4sq2.shown",
    #   "qi1qu5_qi1qu5sq1.shown", "qi1qu5_qi1qu5sq2.shown",
    #   "qi1qu6_qi1qu6sq1.shown", "qi1qu6_qi1qu6sq2.shown",
    #   "qi1qu7_qi1qu7sq1.shown", "qi1qu7_qi1qu7sq2.shown",
    #   "qi1qu8_qi1qu8sq1.shown", "qi1qu8_qi1qu8sq2.shown",
    #   "qi1qu9_qi1qu9sq1.shown", "qi1qu9_qi1qu9sq2.shown",
    #   "qi1qu10_qi1qu10sq1.shown", "qi1qu10_qi1qu10sq2.shown",
    #   "qi1qu11_qi1qu11sq1.shown", "qi1qu11_qi1qu11sq2.shown",
    #   "qi1qu12_qi1qu12sq1.shown", "qi1qu12_qi1qu12sq2.shown",
    #   "qi1qu13_qi1qu13sq1.shown", "qi1qu13_qi1qu13sq2.shown",
    #   "qi1qu14_qi1qu14sq1.shown", "qi1qu14_qi1qu14sq2.shown",
    #   "qi1qu15_qi1qu15sq1.shown", "qi1qu15_qi1qu15sq2.shown"
    # )
    question_subquestion_ids_in_q = paste(question_subquestion_ids, collapse = ",")
    
    # subquestions loop
    limesurveyanchorsubquids = c()
    for (anchi in 1:designctx$anchors) {
      rowNode = newXMLNode("row", parent = rowsNode)
      qidNode = newXMLNode("qid", parent = rowNode)
      lastquestionid = lastquestionid + 1
      cdatanode = newXMLCDataNode(lastquestionid, parent = qidNode)
      parent_qidNode = newXMLNode("parent_qid", parent = rowNode)
      cdatanode = newXMLCDataNode(thisquestionid, parent = parent_qidNode)
      sidNode = newXMLNode("sid", parent = rowNode)
      cdatanode = newXMLCDataNode(iNewID, parent = sidNode)
      gidNode = newXMLNode("gid", parent = rowNode)
      cdatanode = newXMLCDataNode(iGid21, parent = gidNode)
      typeNode = newXMLNode("type", parent = rowNode)
      cdatanode = newXMLCDataNode("T", parent = typeNode)
      
      sAnchorSubQuestionTitle1 = paste(sAnchorQuestionTitle1, "sq", anchi, sep = "") # ovo je kod u LimeSurvey i mora biti jedinstven na nivou surveya
      sAnchorSubQuestion1 = enc2utf8(paste("{getanchors(", anchi, ",", designctx$anchors, ",", question_subquestion_ids_in_q, ")}", sep = ""))
      
      limesurveyanchorsubquids = c(limesurveyanchorsubquids, sAnchorSubQuestionTitle1)
      
      titleNode = newXMLNode("title", parent = rowNode)
      cdatanode = newXMLCDataNode(sAnchorSubQuestionTitle1, parent = titleNode)
      questionNode = newXMLNode("question", parent = rowNode)
      cdatanode = newXMLCDataNode(sAnchorSubQuestion1, parent = questionNode)
      # pregNode = newXMLNode("preg", parent = rowNode)
      # xmlValue(pregNode) = "preg"
      # helpNode = newXMLNode("help", parent = rowNode)
      # xmlValue(helpNode) = sNewQuestionHelp1
      
      otherNode = newXMLNode("other", parent = rowNode)
      cdatanode = newXMLCDataNode("N", parent = otherNode)
      # mandatoryNode = newXMLNode("mandatory", parent = rowNode)
      # xmlValue(mandatoryNode) = "Y"
      question_orderNode = newXMLNode("question_order", parent = rowNode)
      cdatanode = newXMLCDataNode(anchi, parent = question_orderNode)
      languageNode = newXMLNode("language", parent = rowNode)
      cdatanode = newXMLCDataNode("hr", parent = languageNode)
      scale_idNode = newXMLNode("scale_id", parent = rowNode)
      cdatanode = newXMLCDataNode(0, parent = scale_idNode)
      same_defaultNode = newXMLNode("same_default", parent = rowNode)
      cdatanode = newXMLCDataNode(0, parent = same_defaultNode)
      relevanceNode = newXMLNode("relevance", parent = rowNode)
      cdatanode = newXMLCDataNode(1, parent = relevanceNode)
      modulenameNode = newXMLNode("modulename", parent = rowNode)
      #    xmlValue(modulenameNode) = "modulename"
    }
    ## subquestions node
    
    
    ## answers node
    answersNode = newXMLNode("answers", parent = documentNode)
    fieldsNode = newXMLNode("fields", parent = answersNode)
    
    qidNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(qidNode) = "qid"
    codeNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(codeNode) = "code"
    answerNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(answerNode) = "answer"
    sortorderNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(sortorderNode) = "sortorder"
    assessment_valueNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(assessment_valueNode) = "assessment_value"
    languageNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(languageNode) = "language"
    scale_idNode = newXMLNode("fieldname", parent = fieldsNode)
    xmlValue(scale_idNode) = "scale_id"
    
    rowsNode = newXMLNode("rows", parent = answersNode)
    
    ## answers
    
    ## answer yes/positive/i would buy/i like
    rowNode = newXMLNode("row", parent = rowsNode)
    
    qidNode = newXMLNode("qid", parent = rowNode)
    cdatanode = newXMLCDataNode(thisquestionid, parent = qidNode)
    codeNode = newXMLNode("code", parent = rowNode)
    cdatanode = newXMLCDataNode("A1", parent = codeNode)

    answerNode = newXMLNode("answer", parent = rowNode)
    cdatanode = newXMLCDataNode("KUPIO BIH", parent = answerNode)

    sortorderNode = newXMLNode("sortorder", parent = rowNode)
    cdatanode = newXMLCDataNode(1, parent = sortorderNode)
    assessment_valueNode = newXMLNode("assessment_value", parent = rowNode)
    cdatanode = newXMLCDataNode(0, parent = assessment_valueNode)
    languageNode = newXMLNode("language", parent = rowNode)
    cdatanode = newXMLCDataNode("hr", parent = languageNode)
    scale_idNode = newXMLNode("scale_id", parent = rowNode)
    cdatanode = newXMLCDataNode(0, parent = scale_idNode)
    ##
    
    ## answer no/negative/i would not buy/i do not like
    rowNode = newXMLNode("row", parent = rowsNode)
    
    qidNode = newXMLNode("qid", parent = rowNode)
    cdatanode = newXMLCDataNode(thisquestionid, parent = qidNode)
    codeNode = newXMLNode("code", parent = rowNode)
    cdatanode = newXMLCDataNode("A2", parent = codeNode)
    
    answerNode = newXMLNode("answer", parent = rowNode)
    cdatanode = newXMLCDataNode("NE BIH KUPIO", parent = answerNode)
    
    sortorderNode = newXMLNode("sortorder", parent = rowNode)
    cdatanode = newXMLCDataNode(2, parent = sortorderNode)
    assessment_valueNode = newXMLNode("assessment_value", parent = rowNode)
    cdatanode = newXMLCDataNode(0, parent = assessment_valueNode)
    languageNode = newXMLNode("language", parent = rowNode)
    cdatanode = newXMLCDataNode("hr", parent = languageNode)
    scale_idNode = newXMLNode("scale_id", parent = rowNode)
    cdatanode = newXMLCDataNode(0, parent = scale_idNode)
    ##
    
    
    questionString = saveXML(doc, file = NULL, compression = 0, indent = TRUE, 
                             prefix = '<?xml version="1.0" encoding="UTF-8"?>',
                             doctype = NULL, encoding = "UTF8")
    
    # readr::write_file(questionString, "out.xml")
    # questionString1 = readr::read_file("limesurvey_question_1.lsq")
    
    library(openssl)
    sImportData = base64_encode(questionString)
    sImportDataType = 'lsq'
    ### Sva su pitanja obavezna!
    sMandatory = 'Y'
    #import_question(string $sSessionKey, integer $iSurveyID, integer $iGroupID, string $sImportData, string $sImportDataType, string $sMandatory = 'N', string $sNewQuestionTitle = null, string $sNewqQuestion = null, string $sNewQuestionHelp = null) : array|integer    
    iQid1 = call_limer(method = 'import_question',
                       params = list("iSurveyID" = iNewID,
                                     "iGroupID" = iGid21, 
                                     "sImportData" = sImportData,
                                     "sImportDataType" = sImportDataType,
                                     "sMandatory" = sMandatory))
    
    # save new question id
    for (sqidi in 1:length(limesurveyanchorsubquids)) {
      # first column is version (this is why we have +1), then come all the anchors
      limesurveyanchorqids[(limesurveyanchorqids$vers == i), sqidi+1] = 
        paste(iNewID, "X", iGid21, "X", iQid1, limesurveyanchorsubquids[sqidi], sep = "")
    }

  }
  
  
  ## if there are any covariates, add them too
  if (length(designctx$covariates) > 0) {
    sGroupTitle = enc2utf8('Molimo da nam još odgovorite na nekoliko pitanja o sebi')
    sGroupDescription = enc2utf8('')
    #add_group(string $sSessionKey, integer $iSurveyID, string $sGroupTitle, string $sGroupDescription = '') : array|integer
    iGid3 = call_limer(method = 'add_group', 
                       params = list("iSurveyID" = iNewID,
                                     "sGroupTitle" = sGroupTitle,
                                     "sGroupDescription" = sGroupDescription))
    
    for (ci in 1:length(designctx$covariates)) {

      naziv = designctx$covariates[[ci]][["naziv"]]
      vrijednosti = designctx$covariates[[ci]][["vrijednosti"]]
      
      doc = newXMLDoc()
      documentNode = newXMLNode("document", doc = doc)
      LimeSurveyDocTypeNode = newXMLNode("LimeSurveyDocType", parent = documentNode)
      xmlValue(LimeSurveyDocTypeNode) = "Question"
      DBVersionNode = newXMLNode("DBVersion", parent = documentNode)
      xmlValue(DBVersionNode) = 359
      
      languagesNode = newXMLNode("languages", parent = documentNode)
      languageNode = newXMLNode("language", parent = languagesNode)
      xmlValue(languageNode) = "hr"
      
      ## questions node
      questionsNode = newXMLNode("questions", parent = documentNode)
      fieldsNode = newXMLNode("fields", parent = questionsNode)
      
      qidNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(qidNode) = "qid"
      parent_qidNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(parent_qidNode) = "parent_qid"
      sidNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(sidNode) = "sid"
      gidNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(gidNode) = "gid"
      typeNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(typeNode) = "type"
      titleNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(titleNode) = "title"
      questionNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(questionNode) = "question"
      pregNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(pregNode) = "preg"
      helpNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(helpNode) = "help"
      otherNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(otherNode) = "other"
      mandatoryNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(mandatoryNode) = "mandatory"
      question_orderNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(question_orderNode) = "question_order"
      languageNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(languageNode) = "language"
      scale_idNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(scale_idNode) = "scale_id"
      same_defaultNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(same_defaultNode) = "same_default"
      relevanceNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(relevanceNode) = "relevance"
      modulenameNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(modulenameNode) = "modulename"
      
      rowsNode = newXMLNode("rows", parent = questionsNode)
      rowNode = newXMLNode("row", parent = rowsNode)
      
      qidNode = newXMLNode("qid", parent = rowNode)
      lastquestionid = lastquestionid + 1
      thisquestionid = lastquestionid
      cdatanode = newXMLCDataNode(lastquestionid, parent = qidNode)
      parent_qidNode = newXMLNode("parent_qid", parent = rowNode)
      cdatanode = newXMLCDataNode(0, parent = parent_qidNode)
      sidNode = newXMLNode("sid", parent = rowNode)
      cdatanode = newXMLCDataNode(iNewID, parent = sidNode)
      gidNode = newXMLNode("gid", parent = rowNode)
      cdatanode = newXMLCDataNode(iGid3, parent = gidNode)
      typeNode = newXMLNode("type", parent = rowNode)
      # cdatanode = newXMLCDataNode("!", parent = typeNode) # za drop down listu
      cdatanode = newXMLCDataNode("L", parent = typeNode) # za radio button listu
      
      sNewQuestionTitle1 = paste("covq", ci, sep = "") # ovo je kod u LimeSurvey i mora biti jedinstven na nivou surveya
      sNewqQuestion1 = enc2utf8(paste("Molimo odaberite:", naziv))
      sNewQuestionHelp1 = enc2utf8("Izaberite jednu od ponuđenih opcija")
      
      titleNode = newXMLNode("title", parent = rowNode)
      cdatanode = newXMLCDataNode(sNewQuestionTitle1, parent = titleNode)
      questionNode = newXMLNode("question", parent = rowNode)
      cdatanode = newXMLCDataNode(sNewqQuestion1, parent = questionNode)
      pregNode = newXMLNode("preg", parent = rowNode)
      helpNode = newXMLNode("help", parent = rowNode)
      cdatanode = newXMLCDataNode(sNewQuestionHelp1, parent = helpNode)
      
      otherNode = newXMLNode("other", parent = rowNode)
      cdatanode = newXMLCDataNode("N", parent = otherNode)
      mandatoryNode = newXMLNode("mandatory", parent = rowNode)
      cdatanode = newXMLCDataNode("Y", parent = mandatoryNode)
      question_orderNode = newXMLNode("question_order", parent = rowNode)
      cdatanode = newXMLCDataNode(ci, parent = question_orderNode)
      languageNode = newXMLNode("language", parent = rowNode)
      cdatanode = newXMLCDataNode("hr", parent = languageNode)
      scale_idNode = newXMLNode("scale_id", parent = rowNode)
      cdatanode = newXMLCDataNode(0, parent = scale_idNode)
      same_defaultNode = newXMLNode("same_default", parent = rowNode)
      cdatanode = newXMLCDataNode(0, parent = same_defaultNode)
      relevanceNode = newXMLNode("relevance", parent = rowNode)
      cdatanode = newXMLCDataNode(1, parent = relevanceNode)
      modulenameNode = newXMLNode("modulename", parent = rowNode)
      ## questions node
      
      
      ## answers node
      answersNode = newXMLNode("answers", parent = documentNode)
      fieldsNode = newXMLNode("fields", parent = answersNode)
      
      qidNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(qidNode) = "qid"
      codeNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(codeNode) = "code"
      answerNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(answerNode) = "answer"
      sortorderNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(sortorderNode) = "sortorder"
      assessment_valueNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(assessment_valueNode) = "assessment_value"
      languageNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(languageNode) = "language"
      scale_idNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(scale_idNode) = "scale_id"
      
      rowsNode = newXMLNode("rows", parent = answersNode)
      
      ## answers node
      
      for (vi in 1:length(vrijednosti)) {

        rowNode = newXMLNode("row", parent = rowsNode)
        
        qidNode = newXMLNode("qid", parent = rowNode)
        cdatanode = newXMLCDataNode(thisquestionid, parent = qidNode)
        codeNode = newXMLNode("code", parent = rowNode)
        cdatanode = newXMLCDataNode(paste("A", vi, sep = ""), parent = codeNode) # ovo je ovisno o alternativi
        
        answertxt = enc2utf8(vrijednosti[vi])
        
        answerNode = newXMLNode("answer", parent = rowNode)
        cdatanode = newXMLCDataNode(answertxt, parent = answerNode)
        
        sortorderNode = newXMLNode("sortorder", parent = rowNode)
        cdatanode = newXMLCDataNode(vi, parent = sortorderNode)
        assessment_valueNode = newXMLNode("assessment_value", parent = rowNode)
        cdatanode = newXMLCDataNode(0, parent = assessment_valueNode)
        languageNode = newXMLNode("language", parent = rowNode)
        cdatanode = newXMLCDataNode("hr", parent = languageNode)
        scale_idNode = newXMLNode("scale_id", parent = rowNode)
        cdatanode = newXMLCDataNode(0, parent = scale_idNode)
        
      }
      
      ## question_attributes node
      questionString = saveXML(doc, file = NULL, compression = 0, indent = TRUE, 
                               prefix = '<?xml version="1.0" encoding="UTF-8"?>',
                               doctype = NULL, encoding = "UTF8")
      
      # readr::write_file(questionString, "out.xml")
      # questionString1 = readr::read_file("limesurvey_question_1.lsq")
      
      library(openssl)
      sImportData = base64_encode(questionString)
      sImportDataType = 'lsq'
      ### Sva su pitanja obavezna!
      sMandatory = 'Y'
      #import_question(string $sSessionKey, integer $iSurveyID, integer $iGroupID, string $sImportData, string $sImportDataType, string $sMandatory = 'N', string $sNewQuestionTitle = null, string $sNewqQuestion = null, string $sNewQuestionHelp = null) : array|integer    
      iQid2 = call_limer(method = 'import_question',
                         params = list("iSurveyID" = iNewID,
                                       "iGroupID" = iGid3, 
                                       "sImportData" = sImportData,
                                       "sImportDataType" = sImportDataType,
                                       "sMandatory" = sMandatory))
      
      # save new question id
      limesurveycovqid = paste(iNewID, "X", iGid3, "X", iQid2, sep = "")
      limesurveycovqids[limesurveycovqids$vers == i, names(designctx$covariates[ci])] = limesurveycovqid
      
    }
    
  }
  
  ## if there are any personals, add them too
  if (length(designctx$personals) > 0) {
    sGroupTitle = enc2utf8('Molimo da nam još odgovorite na nekoliko pitanja o sebi')
    sGroupDescription = enc2utf8('')
    #add_group(string $sSessionKey, integer $iSurveyID, string $sGroupTitle, string $sGroupDescription = '') : array|integer
    iGid = call_limer(method = 'add_group', 
                      params = list("iSurveyID" = iNewID,
                                    "sGroupTitle" = sGroupTitle,
                                    "sGroupDescription" = sGroupDescription))

    for (ci in 1:length(designctx$personals)) {

      naziv = designctx$personals[[ci]][["naziv"]]
      tip = designctx$personals[[ci]][["tip"]]
      
      doc = newXMLDoc()
      documentNode = newXMLNode("document", doc = doc)
      LimeSurveyDocTypeNode = newXMLNode("LimeSurveyDocType", parent = documentNode)
      xmlValue(LimeSurveyDocTypeNode) = "Question"
      DBVersionNode = newXMLNode("DBVersion", parent = documentNode)
      xmlValue(DBVersionNode) = 359
      
      languagesNode = newXMLNode("languages", parent = documentNode)
      languageNode = newXMLNode("language", parent = languagesNode)
      xmlValue(languageNode) = "hr"
      
      ## questions node
      questionsNode = newXMLNode("questions", parent = documentNode)
      fieldsNode = newXMLNode("fields", parent = questionsNode)
      
      qidNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(qidNode) = "qid"
      parent_qidNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(parent_qidNode) = "parent_qid"
      sidNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(sidNode) = "sid"
      gidNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(gidNode) = "gid"
      typeNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(typeNode) = "type"
      titleNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(titleNode) = "title"
      questionNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(questionNode) = "question"
      pregNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(pregNode) = "preg"
      helpNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(helpNode) = "help"
      otherNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(otherNode) = "other"
      mandatoryNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(mandatoryNode) = "mandatory"
      question_orderNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(question_orderNode) = "question_order"
      languageNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(languageNode) = "language"
      scale_idNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(scale_idNode) = "scale_id"
      same_defaultNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(same_defaultNode) = "same_default"
      relevanceNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(relevanceNode) = "relevance"
      modulenameNode = newXMLNode("fieldname", parent = fieldsNode)
      xmlValue(modulenameNode) = "modulename"
      
      rowsNode = newXMLNode("rows", parent = questionsNode)
      rowNode = newXMLNode("row", parent = rowsNode)
      
      qidNode = newXMLNode("qid", parent = rowNode)
      lastquestionid = lastquestionid + 1
      thisquestionid = lastquestionid
      cdatanode = newXMLCDataNode(lastquestionid, parent = qidNode)
      parent_qidNode = newXMLNode("parent_qid", parent = rowNode)
      cdatanode = newXMLCDataNode(0, parent = parent_qidNode)
      sidNode = newXMLNode("sid", parent = rowNode)
      cdatanode = newXMLCDataNode(iNewID, parent = sidNode)
      gidNode = newXMLNode("gid", parent = rowNode)
      cdatanode = newXMLCDataNode(iGid, parent = gidNode)
      typeNode = newXMLNode("type", parent = rowNode)
      
      if (tip == "email") {
        cdatanode = newXMLCDataNode("S", parent = typeNode) # za short text
        
        sNewQuestionTitle1 = paste("persq", ci, sep = "") # ovo je kod u LimeSurvey i mora biti jedinstven na nivou surveya
        sNewqQuestion1 = enc2utf8(paste("Molimo unesite:", naziv))
        sNewQuestionHelp1 = enc2utf8("")
        
        titleNode = newXMLNode("title", parent = rowNode)
        cdatanode = newXMLCDataNode(sNewQuestionTitle1, parent = titleNode)
        questionNode = newXMLNode("question", parent = rowNode)
        cdatanode = newXMLCDataNode(sNewqQuestion1, parent = questionNode)
        pregNode = newXMLNode("preg", parent = rowNode)
        cdatanode = newXMLCDataNode("/^(\\w[-._+\\w]*\\w@\\w[-._\\w]*\\w\\.\\w{2,3})$/", parent = pregNode)
        helpNode = newXMLNode("help", parent = rowNode)
        cdatanode = newXMLCDataNode(sNewQuestionHelp1, parent = helpNode)
        
        otherNode = newXMLNode("other", parent = rowNode)
        cdatanode = newXMLCDataNode("N", parent = otherNode)
        mandatoryNode = newXMLNode("mandatory", parent = rowNode)
        cdatanode = newXMLCDataNode("Y", parent = mandatoryNode)
        question_orderNode = newXMLNode("question_order", parent = rowNode)
        cdatanode = newXMLCDataNode(ci, parent = question_orderNode)
        languageNode = newXMLNode("language", parent = rowNode)
        cdatanode = newXMLCDataNode("hr", parent = languageNode)
        scale_idNode = newXMLNode("scale_id", parent = rowNode)
        cdatanode = newXMLCDataNode(0, parent = scale_idNode)
        same_defaultNode = newXMLNode("same_default", parent = rowNode)
        cdatanode = newXMLCDataNode(0, parent = same_defaultNode)
        relevanceNode = newXMLNode("relevance", parent = rowNode)
        cdatanode = newXMLCDataNode(1, parent = relevanceNode)
        modulenameNode = newXMLNode("modulename", parent = rowNode)
        ## questions node
        
      } else if (tip == "dropdown") {
        
        vrijednosti = designctx$personals[[ci]][["vrijednosti"]]
        
        cdatanode = newXMLCDataNode("!", parent = typeNode) # za drop down listu
        #cdatanode = newXMLCDataNode("L", parent = typeNode) # za radio button listu
        
        sNewQuestionTitle1 = paste("covq", ci, sep = "") # ovo je kod u LimeSurvey i mora biti jedinstven na nivou surveya
        sNewqQuestion1 = enc2utf8(paste("Molimo odaberite:", naziv))
        sNewQuestionHelp1 = enc2utf8("Izaberite jednu od ponuđenih opcija")
        
        titleNode = newXMLNode("title", parent = rowNode)
        cdatanode = newXMLCDataNode(sNewQuestionTitle1, parent = titleNode)
        questionNode = newXMLNode("question", parent = rowNode)
        cdatanode = newXMLCDataNode(sNewqQuestion1, parent = questionNode)
        pregNode = newXMLNode("preg", parent = rowNode)
        helpNode = newXMLNode("help", parent = rowNode)
        cdatanode = newXMLCDataNode(sNewQuestionHelp1, parent = helpNode)
        
        otherNode = newXMLNode("other", parent = rowNode)
        cdatanode = newXMLCDataNode("N", parent = otherNode)
        mandatoryNode = newXMLNode("mandatory", parent = rowNode)
        cdatanode = newXMLCDataNode("Y", parent = mandatoryNode)
        question_orderNode = newXMLNode("question_order", parent = rowNode)
        cdatanode = newXMLCDataNode(ci, parent = question_orderNode)
        languageNode = newXMLNode("language", parent = rowNode)
        cdatanode = newXMLCDataNode("hr", parent = languageNode)
        scale_idNode = newXMLNode("scale_id", parent = rowNode)
        cdatanode = newXMLCDataNode(0, parent = scale_idNode)
        same_defaultNode = newXMLNode("same_default", parent = rowNode)
        cdatanode = newXMLCDataNode(0, parent = same_defaultNode)
        relevanceNode = newXMLNode("relevance", parent = rowNode)
        cdatanode = newXMLCDataNode(1, parent = relevanceNode)
        modulenameNode = newXMLNode("modulename", parent = rowNode)
        ## questions node
        
        
        ## answers node
        answersNode = newXMLNode("answers", parent = documentNode)
        fieldsNode = newXMLNode("fields", parent = answersNode)
        
        qidNode = newXMLNode("fieldname", parent = fieldsNode)
        xmlValue(qidNode) = "qid"
        codeNode = newXMLNode("fieldname", parent = fieldsNode)
        xmlValue(codeNode) = "code"
        answerNode = newXMLNode("fieldname", parent = fieldsNode)
        xmlValue(answerNode) = "answer"
        sortorderNode = newXMLNode("fieldname", parent = fieldsNode)
        xmlValue(sortorderNode) = "sortorder"
        assessment_valueNode = newXMLNode("fieldname", parent = fieldsNode)
        xmlValue(assessment_valueNode) = "assessment_value"
        languageNode = newXMLNode("fieldname", parent = fieldsNode)
        xmlValue(languageNode) = "language"
        scale_idNode = newXMLNode("fieldname", parent = fieldsNode)
        xmlValue(scale_idNode) = "scale_id"
        
        rowsNode = newXMLNode("rows", parent = answersNode)
        
        ## answers node
        
        for (vi in 1:length(vrijednosti)) {
          
          rowNode = newXMLNode("row", parent = rowsNode)
          
          qidNode = newXMLNode("qid", parent = rowNode)
          cdatanode = newXMLCDataNode(thisquestionid, parent = qidNode)
          codeNode = newXMLNode("code", parent = rowNode)
          cdatanode = newXMLCDataNode(paste("A", vi, sep = ""), parent = codeNode) # ovo je ovisno o alternativi
          
          answertxt = enc2utf8(vrijednosti[vi])
          
          answerNode = newXMLNode("answer", parent = rowNode)
          cdatanode = newXMLCDataNode(answertxt, parent = answerNode)
          
          sortorderNode = newXMLNode("sortorder", parent = rowNode)
          cdatanode = newXMLCDataNode(vi, parent = sortorderNode)
          assessment_valueNode = newXMLNode("assessment_value", parent = rowNode)
          cdatanode = newXMLCDataNode(0, parent = assessment_valueNode)
          languageNode = newXMLNode("language", parent = rowNode)
          cdatanode = newXMLCDataNode("hr", parent = languageNode)
          scale_idNode = newXMLNode("scale_id", parent = rowNode)
          cdatanode = newXMLCDataNode(0, parent = scale_idNode)
          
        }
        
      } else {
        break
      }
      
      questionString = saveXML(doc, file = NULL, compression = 0, indent = TRUE, 
                               prefix = '<?xml version="1.0" encoding="UTF-8"?>',
                               doctype = NULL, encoding = "UTF8")
      
      # readr::write_file(questionString, "out.xml")
      # questionString1 = readr::read_file("limesurvey_question_1.lsq")
      
      sImportData = openssl::base64_encode(questionString)
      sImportDataType = 'lsq'
      ### Sva su pitanja obavezna!
      sMandatory = 'Y'
      #import_question(string $sSessionKey, integer $iSurveyID, integer $iGroupID, string $sImportData, string $sImportDataType, string $sMandatory = 'N', string $sNewQuestionTitle = null, string $sNewqQuestion = null, string $sNewQuestionHelp = null) : array|integer    
      iQid = call_limer(method = 'import_question',
                        params = list("iSurveyID" = iNewID,
                                      "iGroupID" = iGid, 
                                      "sImportData" = sImportData,
                                      "sImportDataType" = sImportDataType,
                                      "sMandatory" = sMandatory))
      
      # save new question id
      limesurveypersqid = paste(iNewID, "X", iGid, "X", iQid, sep = "")
      limesurveypersqids[limesurveypersqids$vers == i, names(designctx$personals[ci])] = limesurveypersqid
      
    }
    
  }
  
}

# cleanup, remove duplicates, we should do this when creating this file
limesurveyqids = limesurveyqids[!duplicated(limesurveyqids), ]
# save limesurvey questiond ids
saveRDS(list(qids = limesurveyqids, anchorquids = limesurveyanchorqids, covqids = limesurveycovqids, 
             persquids = limesurveypersqids), limesurveyqidsfile)
