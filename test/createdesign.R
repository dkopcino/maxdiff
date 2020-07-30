# https://www.r-bloggers.com/how-to-create-a-max-diff-experimental-design-in-r/
# https://www.r-bloggers.com/how-to-analyze-max-diff-data-in-r/

library(choiceDes)
library(AlgDesign)

source("surveyconfig.R")


# respondent personal information (not covariates or variables) to be added to the survey
# personals = list()
# personals = list(zemlja = list(naziv = "zemlja", tip = "dropdown", vrijednosti = c("Hrvatska", "Bosna i Hercegovina", "Ostalo")),
#                  dob = list(naziv = "dob", tip = "dropdown", vrijednosti = c("manje od 20 godina", "20-50 godina", "više od 50 godina")))
personals = list(email = list(pitanje = "Molimo unesite e-poštu (za dostavu poklon bona)!", tip = "email"))


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
covariates = list(lokacija = list(pitanje = "Da li živite blizu ili daleko od (najbliže) pizzerije?", tip = "radio", vrijednosti = c("blizu pizzerije", "daleko od pizzerije")))



# screening questions, have to be polished manually in the questionnaires
screenings = list()
screenings = list(
  # planira_putovati = list(pitanje = "Da li planirate otići na turističko putovanje u sljedećih 6 mjeseci, uz pretpostavku da će se ukinuti ograničenja putovanja uvedena zbog korona virusa?", tip = "radio", vrijednosti = c("da", "ne")),
  gdje_zivi = list(pitanje = "Gdje živite (zemlja)?", tip = "radio", vrijednosti = c("Hrvatska", "ostalo"))
)


# items to choose from
items = factor(c("miješana", "povrtna", "pikantna", "4 vrste sira", "s tunom", "ribarska", "4 godišnja doba",
                 "calzone", "slavonska", "rukola/pršut", "bolonjez", "lovačka", "losos", "sa salamom"))

# želimo da se svaki item prikaže barem 3 puta i da se prikazuje 3 itema po pitanju
nalternatives = 3 # koliko se prikazuje u svakom pitanju?
nquestionnaires = 1 # koliko varijanti upitnika?
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


# do we use metadata (images, descriptions...) in survey creation, if we do set the metadata mapping
library(ggplot2)
use_meta = TRUE
survey_meta = NULL
if (use_meta) {
  library(XML)
  # do we create new survey meta or use/edit from external file?
  if (FALSE) {
    
    # create meta XML file, save it and set to design context
    survey_meta = list()
    image_width = 200 # default image width in pixels
    image_height = 100 # default image height in pixels
    for (n in levels(items)) {
      lfname = paste0(txt2filename(n), ".jpg")
      survey_meta[[n]] = list(
        "image" = paste0(lsuploaddir, lfname),
        "image_width" = image_width,
        "image_height" = image_height,
        "description" = ""
      )
      # create some default images, 200 x 100px, with the item name in the middle
      ggsave(
        paste0("images/", lfname),
        ggplot(data = data.frame(x = c(0), y = c(0), lb = c(n))) + 
          geom_rect(mapping = aes(xmin = x, xmax = x + 2, ymin = y, ymax = y + 1), color = "black", alpha = 0.5) +
          geom_text(aes(x = x + 1, y = y + .5, label = lb), size = 2) + 
          theme_void(),
        units = "in",
        dpi = 300,
        width = image_width/300, # 200/dpi
        height = image_height/300 # 100/dpi
      )
    }
    
    doc = newXMLDoc()
    documentNode = newXMLNode("survey_meta", doc = doc)
    for (n in items) {
      levelnode = newXMLNode("level", parent = documentNode)
      levelnamenode = newXMLNode("levelname", parent = levelnode)
      cdatanode = newXMLCDataNode(enc2utf8(n), parent = levelnamenode)
      for (ln in names(survey_meta[[n]])) {
        levelmetanode = newXMLNode("levelmeta", parent = levelnode)
        levelmetanamenode = newXMLNode("levelmetaname", parent = levelmetanode)
        xmlValue(levelmetanamenode) = ln # here we assume that meta names do not need to be encoded
        levelmetavaluenode = newXMLNode("levelmetavalue", parent = levelmetanode)
        if (is.character(survey_meta[[n]][[ln]])) {
          cdatanode = newXMLCDataNode(enc2utf8(survey_meta[[n]][[ln]]), parent = levelmetavaluenode)
        } else {
          xmlValue(levelmetavaluenode) = survey_meta[[n]][[ln]]
        }
      }
    }
    
    # ispis je ljepši (novi redovi i sl.) nego sa saveXML
    sink(surveymetafile)
    print(doc)
    sink()
    
    # saveXML(doc, file = surveymetafile, compression = 0, indent = FALSE,
    #         prefix = '<?xml version="1.0" encoding="UTF-8"?>',
    #         doctype = NULL, encoding = "UTF8")
    
    
  } else {
    
    # read in meta XML file and set to design context
    
    survey_meta = list()
    doc = xmlRoot(xmlNativeTreeParse(file = surveymetafile, encoding = "UTF8"))
    levels = xmlElementsByTagName(doc, "level")
    for (l in levels) {
      levelname = xmlValue(xmlElementsByTagName(l, "levelname"))
      names(levelname) = ""
      survey_meta[[levelname]] = list()
      for (lm in xmlElementsByTagName(l, "levelmeta")) {
        levelmetaname = xmlValue(xmlElementsByTagName(lm, "levelmetaname"))
        names(levelmetaname) = ""
        levelmetavalue = xmlValue(xmlElementsByTagName(lm, "levelmetavalue"))
        names(levelmetavalue) = ""
        survey_meta[[levelname]][[levelmetaname]] = levelmetavalue
      }
    }
    #    survey_meta
    
  }
}


designctx = list(
  items = items,
  nquestionnaires = nquestionnaires,
  nquestions = nquestions,
  nalternatives = nalternatives,
  survey = survey,
  survey_meta = survey_meta,
  use_meta = use_meta,
  design = dz,
  covariates = covariates,
  fullfact_covdesign = fullfact_covdesign,
  screenings = screenings,
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

