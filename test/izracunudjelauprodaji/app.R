# # rsconnect part
# library(rsconnect)
# rsconnect::setAccountInfo(name = 'itmarket', token = 'C1A599E9B054414D2AF7752A881E154E',
#                           secret = 'aqXbJmCkZsdOiqoWmaF3Gi6UIOUXQdjYIGGcNxu3')
# rsconnect::deployApp('.')

# https://mastering-shiny.org/why-reactivity.html

iid = "test_ga"
cbcid = paste("cbc_", iid, sep = "") # used for cbc/design identification
designctxfile = paste(cbcid, "_designctx.RDS", sep = "")
options(contrasts = c("contr.treatment", "contr.poly"))
mnlmodelfile = paste(cbcid, "_model_mnl.RDS", sep = "")
hbmodelfile = paste(cbcid, "_model_hb.RDS", sep = "")

designctx = readRDS(designctxfile)
m2.hier = readRDS(mnlmodelfile)
hb.post = readRDS(hbmodelfile)

## globalne opcije i udjeli za eventualno spremanje
g_shares = data.frame()

#paste(unlist(lapply(colnames(designctx$fullfact), function(cn) paste("input[['", cn, "_", 1:12, "']]", sep = ""))), collapse = ", ")

izracunaj_udjele <- function(opcije) {

  library(mlogit)
  library(ChoiceModelR)
  
  domodelmatrix = function(fml, df) {
    #  mm = model.matrix.lm(fml, df, na.action = "na.pass")[, -1] # ignore the Intercept
    mm = model.matrix.lm(fml, df, na.action = "na.pass")
    # we do the following here because we will later use data frame anyway
    # any in data frame make.names will be called
    colnames(mm) = make.names(colnames(mm))
    # change the NA values which are assigned for the missing alternative specific attributes to zeros (to prevent
    # influence on utility) and save the column names for later usage (e.g. to build a modelling formula)
    r = lapply(colnames(mm), function(cn) {
      mm[which(is.na(mm[, cn])), cn] <<- 0
    })
    mm
  }
  
  library(MASS)
  
  # model must contain fml0, cov.df (can be empty)
  # data must be a data.frame
  # data should contain asc or alt column, if it doesn't then all the alt levels will be set to the same level (1)
  # and this will be used for comparison
  # returns matrix of utilities, nresp x nrow(data)
  calc.utilities.mnl = function(model, data) {
    # ovo moramo napraviti zbog uvjeta u mlogit.data (broj redaka mora biti višekratnik od broja alternativa)
    # ako broj redaka nije višekratnik, onda ćemo (bez utjecaja na izračun utilitya) iskopirati na kraj data
    # još n_add redaka (1. redak), a kasnije ćemo to maknuti
    n_add = 0
    if (!is.null(data$asc)) {
      # ovo moramo dodati jer ako imamo samo jedan alt (recimo ako imamo samo jedan ili dva unosa) onda je broj levela
      # od alt 1 pa model.matrix (niže) pukne
      alt = factor(data$asc, levels = unlist(designctx$ascs))
    } else if (!is.null(data$alt)) {
      alt = factor(data$alt, levels = unlist(designctx$ascs))
    } else {
      # ovo moramo napraviti iz istog razloga kao gore, s time da alt moramo izvući iz modela
      # ne možemo staviti bezveze jer ako npr. koristimo covariates, onda su njihovi koeficijenti vezani
      # za alternativu (alternative specific) pa ovise o alternativi i alternativa mora biti definirana
      # kako je ovo samo za predikciju, postavljamo alt za sve podatke na istu vrijednost (nadamo se da će
      # relativni omjeri i dalje ostati isti)
      altlevels = as.numeric(levels(attributes(model$model$choice)$index$alt))
      n_alts = length(altlevels)
      n_df = nrow(data)
      if ((n_df %% n_alts) > 0) {
        n_add = ceiling(n_df/n_alts)*n_alts - n_df
        data.add = data[rep(1, n_add), ]
        data = rbind(data, data.add)
      }
      alt = factor(rep(altlevels[1], nrow(data)), levels = altlevels)
    }
    coef.Sigma = cov.mlogit(model)
    coef.mu = model$coef[1:dim(coef.Sigma)[1]]
    if (nrow(model$cov.df) > 0) {
      nresp = nrow(model$cov.df)
    } else {
      nresp = 1000
    }
    draws = mvrnorm(n = nresp, coef.mu, coef.Sigma)
    utilities = matrix(NA, nrow = nresp, ncol = nrow(data))
    for (i in 1:nresp) {
      if (nrow(model$cov.df) > 0) {
        # add covariates to the data frame
        n_d = ncol(data)
        data = cbind(data, model$cov.df[rep(i, nrow(data)), ])
        colnames(data)[(n_d+1):ncol(data)] = colnames(model$cov.df)
      }
      ffd_mm = data.frame(domodelmatrix(model$fml0, data)[, -1]) # without the Intercept
      ffd_mm$alt = alt
      ffd_mm$choice = rep(1, nrow(ffd_mm))
      ffd.mlogit = mlogit.data(data = ffd_mm, choice = "choice", shape = "long", varying = 1:(ncol(ffd_mm)-2), alt.var = "alt", alt.levels = levels(ffd_mm$alt))
      data.model = model.matrix(model$formula, data = ffd.mlogit)
      utilities[i, ] = data.model %*% draws[i, ]
    }
    if (n_add > 0) utilities = utilities[, -((ncol(utilities)-n_add+1):ncol(utilities))]
    utilities
  }
  
  # Simulating shares
  # data must be a data.frame
  # see also calc.utilities.mnl
  predict.hier.mnl = function(model, data) {
    utilities = calc.utilities.mnl(model, data)
    nresp = nrow(utilities)
    shares = matrix(NA, nrow = nresp, ncol = ncol(utilities))
    for (i in 1:nresp) {
      utility = utilities[i, ]
      share = exp(utility)/sum(exp(utility))
      shares[i, ] = round(share * 100, digits = 2) # round to get percentage
    }
    r = cbind(data, "share %" = colMeans(shares))
    rownames(r) = c()
    r[["share %"]] = as.numeric(round(r[["share %"]], digits = 2))
    r
  }
  
  # model must contain fml0
  # data must be a data.frame
  # returns matrix of utilities, nresp x nrow(data) x ndraws
  calc.utilities.hb = function(model, data) {
    data.df = data.frame(domodelmatrix(model$fml0, data)[, -1]) # without the Intercept
    data.model = as.matrix(data.df)
    betadraw = model$betadraw
    nresp = dim(betadraw)[1]
    ndraws = dim(betadraw)[3]
    utilities = array(dim = c(nresp, nrow(data.df), ndraws))
    for (d in 1:ndraws) {
      for (i in 1:nresp) {
        utilities[i, , d] = data.model %*% betadraw[i, , d]
      }
    }
    utilities
  }
  
  ### HIERARCHICAL BAYES
  # data must be a data.frame
  # see also calc.utilities.hb
  predict.hb.mnl = function(model, data) {
    utilities = calc.utilities.hb(model, data)
    betadraw = model$betadraw
    nresp = dim(betadraw)[1]
    ndraws = dim(betadraw)[3]
    shares = array(dim = c(nresp, nrow(data), ndraws))
    for (d in 1:ndraws) {
      for (i in 1:nresp) {
        shares[i, , d] = (exp(utilities[i, , d])/sum(exp(utilities[i, , d]))) * 100
      }
    }
    shares.agg = apply(shares, 2:3, mean)
    r = cbind(data, 
              "share %" = apply(shares.agg, 1, mean)
              # ,
              # "5%" = apply(shares.agg, 1, quantile, probs = c(0.05)),
              # "95%" = apply(shares.agg, 1, quantile, probs = c(0.95))
    )
    rownames(r) = c()
    r[["share %"]] = as.numeric(round(r[["share %"]], digits = 2))
    # r[["5%"]] = as.numeric(round(r[["5%"]], digits = 2))
    # r[["95%"]] = as.numeric(round(r[["95%"]], digits = 2))
    r
  }
  
  
  ## define the new data as a data frame
  ffd_df = opcije
#  shares.mnl = predict.hier.mnl(m2.hier, data = ffd_df)
  shares.hb = predict.hb.mnl(hb.post, data = ffd_df)
  
  shares = shares.hb
  # t(shares)
  
#  print("G")
  
  g_shares <<- shares
  
  as.vector(shares[["share %"]])
  
}

# globalna varijabla koja ima sve levele
svi_leveli = list(
  # "vrijeme_pripreme" = levels(designctx$fullfact$vrijeme_pripreme),
  # "pakiranje" = levels(designctx$fullfact$pakiranje),
  # "kolicina" = levels(designctx$fullfact$kolicina),
  "juha" = levels(designctx$fullfact$juha),
  # "zitarice" = levels(designctx$fullfact$zitarice),
  "povrce" = levels(designctx$fullfact$povrce),
  # "mesni_dodaci" = levels(designctx$fullfact$mesni_dodaci),
  # "drugi_dodaci" = levels(designctx$fullfact$drugi_dodaci),
  # "tjestenina" = levels(designctx$fullfact$tjestenina),
  # "zacini" = levels(designctx$fullfact$zacini),
  "cijena" = levels(designctx$fullfact$cijena)
)


# -- NOW THE SHINY PART

library(shiny)
library(shinydashboard)
library(glue)

ui <- dashboardPage(
  
  dashboardHeader(title = "Izračun udjela u prodaji", titleWidth = "95%"),
  
  dashboardSidebar(
    numericInput("brojopcija", "Molimo odaberite broj opcija za usporedbu:", 2),
    column(
      helpText("Ovisno o odabranom broju opcija koje želite usporediti, kreirat će se stupci (Opcija 1, Opcija 2...) u kojima zatim možete mijenjati vrijednosti atributa i pratiti promjene u udjelima koje će se prikazati ispod stupaca."),
      br(),
      downloadButton("downloadData", "Spremi opcije"),
      width = 12
    ),
    fileInput(inputId = "file1", label = "",
              buttonLabel = "Učitaj opcije",
              multiple = FALSE,
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
  ),
  
  dashboardBody(
    fluidRow(
      column(12, uiOutput("ui1"))
    ),
    fluidRow(
      column(12, uiOutput("ui2"))
    )
  )
)

server <- function(input, output, session) {
  
  # varijabla za čuvanje trenutnih opcija
  trenutne_opcije = reactiveVal(designctx$fullfact[c(1, 1), ]) # 2 jer je to broj na koji postavljamo input$brojopcija
  
  observeEvent(input$brojopcija, {
    #browser()
    
    novi_broj_opcija = input$brojopcija
    trenutni_broj_opcija = nrow(trenutne_opcije())
    if (novi_broj_opcija < trenutni_broj_opcija) {
      
      trenutne_opcije(trenutne_opcije()[1:(novi_broj_opcija), ])
      
    } else if (novi_broj_opcija > trenutni_broj_opcija) {
      
      dodatne_opcije = designctx$fullfact[rep(1, novi_broj_opcija - trenutni_broj_opcija), ]
      trenutne_opcije(rbind(trenutne_opcije(), dodatne_opcije))
      
    } else {
      
    }
    
  })
  
  output$ui1 <- renderUI({
    # req(trenutne_opcije, level_eventi)
    #browser()
    
    opcije_df = trenutne_opcije()
    
    if (nrow(opcije_df) < 2) {
      
      h3("Najmanji broj opcija je 2")
      
    } else if (nrow(opcije_df) > 12) {
      
      h3("Najveći broj opcija je 12")
      
    } else {
      
      lapply(1:nrow(opcije_df), function(i) {
        column(
          h4(glue("Opcija {i}")),
          h4(verbatimTextOutput(outputId = glue("udio_{i}"))), 
          # selectInput(glue("vrijeme_pripreme_{i}"), "Vrijeme pripreme", svi_leveli$vrijeme_pripreme,
          #             selected = as.character(opcije_df[i, ]$vrijeme_pripreme), 
          #             multiple = F, selectize = T, width = NULL, size = NULL),
          # selectInput(glue("pakiranje_{i}"), "Pakiranje", svi_leveli$pakiranje,
          #             selected = as.character(opcije_df[i, ]$pakiranje), 
          #             multiple = F, selectize = T, width = NULL, size = NULL),
          # selectInput(glue("kolicina_{i}"), "Količina", svi_leveli$kolicina,
          #             selected = as.character(opcije_df[i, ]$kolicina), 
          #             multiple = F, selectize = T, width = NULL, size = NULL),
          selectInput(glue("juha_{i}"), "Juha", svi_leveli$juha,
                      selected = as.character(opcije_df[i, ]$juha), 
                      multiple = F, selectize = T, width = NULL, size = NULL),
          # selectInput(glue("zitarice_{i}"), "Žitarice", svi_leveli$zitarice,
          #             selected = as.character(opcije_df[i, ]$zitarice), 
          #             multiple = F, selectize = T, width = NULL, size = NULL),
          selectInput(glue("povrce_{i}"), "Povrće", svi_leveli$povrce,
                      selected = as.character(opcije_df[i, ]$povrce), 
                      multiple = F, selectize = T, width = NULL, size = NULL),
          # selectInput(glue("mesni_dodaci_{i}"), "Mesni dodaci", svi_leveli$mesni_dodaci,
          #             selected = as.character(opcije_df[i, ]$mesni_dodaci), 
          #             multiple = F, selectize = T, width = NULL, size = NULL),
          # selectInput(glue("drugi_dodaci_{i}"), "Drugi dodaci", svi_leveli$drugi_dodaci,
          #             selected = as.character(opcije_df[i, ]$drugi_dodaci), 
          #             multiple = F, selectize = T, width = NULL, size = NULL),
          # selectInput(glue("tjestenina_{i}"), "Tjestenina", svi_leveli$tjestenina,
          #             selected = as.character(opcije_df[i, ]$tjestenina), 
          #             multiple = F, selectize = T, width = NULL, size = NULL),
          # selectInput(glue("zacini_{i}"), "Začini", svi_leveli$zacini,
          #             selected = as.character(opcije_df[i, ]$zacini), 
          #             multiple = F, selectize = T, width = NULL, size = NULL),
          selectInput(glue("cijena_{i}"), "Cijena", svi_leveli$cijena,
                      selected = as.character(opcije_df[i, ]$cijena), 
                      multiple = F, selectize = T, width = NULL, size = NULL),
          width = min(4, floor(12/nrow(opcije_df)))
        )
      })
      
    }
  })
  
  observeEvent(list(input[['vrijeme_pripreme_1']], input[['vrijeme_pripreme_2']], input[['vrijeme_pripreme_3']], input[['vrijeme_pripreme_4']], input[['vrijeme_pripreme_5']], input[['vrijeme_pripreme_6']], input[['vrijeme_pripreme_7']], input[['vrijeme_pripreme_8']], input[['vrijeme_pripreme_9']], input[['vrijeme_pripreme_10']], input[['vrijeme_pripreme_11']], input[['vrijeme_pripreme_12']], input[['pakiranje_1']], input[['pakiranje_2']], input[['pakiranje_3']], input[['pakiranje_4']], input[['pakiranje_5']], input[['pakiranje_6']], input[['pakiranje_7']], input[['pakiranje_8']], input[['pakiranje_9']], input[['pakiranje_10']], input[['pakiranje_11']], input[['pakiranje_12']], input[['kolicina_1']], input[['kolicina_2']], input[['kolicina_3']], input[['kolicina_4']], input[['kolicina_5']], input[['kolicina_6']], input[['kolicina_7']], input[['kolicina_8']], input[['kolicina_9']], input[['kolicina_10']], input[['kolicina_11']], input[['kolicina_12']], input[['juha_1']], input[['juha_2']], input[['juha_3']], input[['juha_4']], input[['juha_5']], input[['juha_6']], input[['juha_7']], input[['juha_8']], input[['juha_9']], input[['juha_10']], input[['juha_11']], input[['juha_12']], input[['zitarice_1']], input[['zitarice_2']], input[['zitarice_3']], input[['zitarice_4']], input[['zitarice_5']], input[['zitarice_6']], input[['zitarice_7']], input[['zitarice_8']], input[['zitarice_9']], input[['zitarice_10']], input[['zitarice_11']], input[['zitarice_12']], input[['povrce_1']], input[['povrce_2']], input[['povrce_3']], input[['povrce_4']], input[['povrce_5']], input[['povrce_6']], input[['povrce_7']], input[['povrce_8']], input[['povrce_9']], input[['povrce_10']], input[['povrce_11']], input[['povrce_12']], input[['mesni_dodaci_1']], input[['mesni_dodaci_2']], input[['mesni_dodaci_3']], input[['mesni_dodaci_4']], input[['mesni_dodaci_5']], input[['mesni_dodaci_6']], input[['mesni_dodaci_7']], input[['mesni_dodaci_8']], input[['mesni_dodaci_9']], input[['mesni_dodaci_10']], input[['mesni_dodaci_11']], input[['mesni_dodaci_12']], input[['drugi_dodaci_1']], input[['drugi_dodaci_2']], input[['drugi_dodaci_3']], input[['drugi_dodaci_4']], input[['drugi_dodaci_5']], input[['drugi_dodaci_6']], input[['drugi_dodaci_7']], input[['drugi_dodaci_8']], input[['drugi_dodaci_9']], input[['drugi_dodaci_10']], input[['drugi_dodaci_11']], input[['drugi_dodaci_12']], input[['tjestenina_1']], input[['tjestenina_2']], input[['tjestenina_3']], input[['tjestenina_4']], input[['tjestenina_5']], input[['tjestenina_6']], input[['tjestenina_7']], input[['tjestenina_8']], input[['tjestenina_9']], input[['tjestenina_10']], input[['tjestenina_11']], input[['tjestenina_12']], input[['zacini_1']], input[['zacini_2']], input[['zacini_3']], input[['zacini_4']], input[['zacini_5']], input[['zacini_6']], input[['zacini_7']], input[['zacini_8']], input[['zacini_9']], input[['zacini_10']], input[['zacini_11']], input[['zacini_12']], input[['cijena_1']], input[['cijena_2']], input[['cijena_3']], input[['cijena_4']], input[['cijena_5']], input[['cijena_6']], input[['cijena_7']], input[['cijena_8']], input[['cijena_9']], input[['cijena_10']], input[['cijena_11']], input[['cijena_12']]), {
    #browser()
    
    # ovdje smo jer se neka vrijednost u opcijama promijenila, spremiti to u trenutne_opcije
    opcije = data.frame()
    r = lapply(1:nrow(trenutne_opcije()), function(i) {
      opcija = data.frame(
        # "vrijeme_pripreme" = factor(input[[paste("vrijeme_pripreme_", i, sep = "")]], levels = svi_leveli$vrijeme_pripreme),
        # "pakiranje" = factor(input[[paste("pakiranje_", i, sep = "")]], levels = svi_leveli$pakiranje),
        # "kolicina" = factor(input[[paste("kolicina_", i, sep = "")]], levels = svi_leveli$kolicina),
        "juha" = factor(input[[paste("juha_", i, sep = "")]], levels = svi_leveli$juha),
        # "zitarice" = factor(input[[paste("zitarice_", i, sep = "")]], levels = svi_leveli$zitarice),
        "povrce" = factor(input[[paste("povrce_", i, sep = "")]], levels = svi_leveli$povrce),
        # "mesni_dodaci" = factor(input[[paste("mesni_dodaci_", i, sep = "")]], levels = svi_leveli$mesni_dodaci),
        # "drugi_dodaci" = factor(input[[paste("drugi_dodaci_", i, sep = "")]], levels = svi_leveli$drugi_dodaci),
        # "tjestenina" = factor(input[[paste("tjestenina_", i, sep = "")]], levels = svi_leveli$tjestenina),
        # "zacini" = factor(input[[paste("zacini_", i, sep = "")]], levels = svi_leveli$zacini),
        "cijena" = factor(input[[paste("cijena_", i, sep = "")]], levels = svi_leveli$cijena)
      )
      opcije <<- rbind(opcije, opcija)
    })
    if (nrow(opcije) > 0) {
      trenutne_opcije(opcije)
    }
    
  })
  
  output$ui2 <- renderUI({
    #browser()
    
    opcije_df = trenutne_opcije()
    
    n_opcija = nrow(opcije_df)
    if ((n_opcija >= 2) & (n_opcija <= 12)) {
      u = izracunaj_udjele(opcije_df)
      lapply(1:n_opcija, function(i) {
        column(
          p(strong(paste("Udio:", u[i])), style = "font-size: 20px; border: 1px red dotted; padding-left: 3px;"), 
          width = min(4, floor(12/n_opcija))
        )
      })
    } else {
      ""
    }
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("opcije_i_udjeli.csv")
    },
    content = function(file) {
      write.csv(g_shares, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$file1, {
    #browser()
    
    tryCatch(
      {
        opcije_df = read.csv(input$file1$datapath, header = TRUE, sep = ",", quote = "\"", stringsAsFactors = FALSE)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    r = lapply(colnames(opcije_df), function(cn) {
      opcije_df[[cn]] <<- factor(opcije_df[[cn]], levels = svi_leveli[[cn]])
    })
    trenutne_opcije(opcije_df)
    updateNumericInput(session = session, inputId = "brojopcija", value = nrow(opcije_df))
  })
}

shinyApp(ui = ui, server = server)
