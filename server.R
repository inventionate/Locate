
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(TimeSpaceAnalysis)
db_load("/Users/fabianmundt/Library/Mobile Documents/com~apple~CloudDocs/Dissertation/Erhebungsdaten/zrs_datenbank.sqlite")
# ############################################################################
# Geometrisches Modell laden 
# ############################################################################
attach("/Users/fabianmundt/Library/Mobile Documents/com~apple~CloudDocs/Dissertation/Sozioanalysen/soziale-topologie.RData")
excl_studienalltag <- readRDS("/Users/fabianmundt/Library/Mobile Documents/com~apple~CloudDocs/Dissertation/Sozioanalysen/soziale-topologie-excl-data.rds")

shinyServer(function(input, output) {

  sup_ind <- reactive({
    sup_ind <- cbind.data.frame(input$wohnort, input$entfernung_ph, input$finanzierung_eltern,
                          input$finanzierung_bafoeg, input$finanzierung_arbeit_vorlesungszeit,
                          input$finanzierung_arbeit_semesterferien, input$finanzielle_situation,
                          input$zeitmuster, input$lehrveranstaltungen_anzahl, input$lernort_alleine,
                          input$selbstorganisierte_lerngruppe, input$aufmerksamkeit, input$sitzplatz,
                          input$sitzplatz_identisch, input$ph_hsb_recherchieren, input$ph_hsb_lernen,
                          input$mensa_essen, input$cafete_essen, input$computerräume_nutzen, 
                          input$campus_lernen, input$campus_entspannen, input$austausch_lehrende, 
                          input$ph_kommilitoninnen_entspannen, input$ph_kommilitoninnen_lernen,
                          input$ka_essen, input$ka_entspannen, input$bahn_lernen, 
                          input$fremder_wohnort_lernen, input$kommilitoninnen_treffen,
                          input$kommilitoninnen_lernen, input$website_nutzen,
                          input$web_services_andere_nutzen, input$emails_lehrende,
                          input$emails_kommilitoninnen, input$internetrecherche,
                          input$soziale_netzwerke_studieren, input$fernsehen,
                          input$musik, input$kino, input$freunde_treffen, input$soziale_netzwerke,
                          input$internet_surfen, input$chillen, input$bücher, input$zeitungen,
                          input$disco, input$sport, input$familie, input$shoppen, 
                          input$kreatives, input$kirche, input$ehrenamt)
    
    colnames(sup_ind) <- colnames(mfa_studienalltag$call$X)
    
    data.frame(sup_ind)
  })
  
  
  output$mfa_studienalltag_sup_ind <- renderPlot({
    fviz_add_sup_ind(mfa_studienalltag, excl_studienalltag, sup_ind(), label = input$name, size = 6) +
      xlab(paste0("Achse 1 (", round(mfa_studienalltag$eig$`percentage of variance`[1], 1), "%)")) +
      ylab(paste0("Achse 2 (", round(mfa_studienalltag$eig$`percentage of variance`[2], 1), "%)")) +
      ggtitle("Studienalltag (WS15/16)")
  })
  
  output$mfa_studienalltag_achse_1 <- renderPlot({
    fviz_gda_var_axis(mfa_studienalltag, axis = 1, contrib = "auto", title = "Studienalltag (WS15/16) — Achse 1", groups = "b") 
  })
  
  output$mfa_studienalltag_achse_2 <- renderPlot({
    fviz_gda_var_axis(mfa_studienalltag, axis = 2, contrib = "auto", title = "Studienalltag (WS15/16) — Achse 2", groups = "b")
  })

})
