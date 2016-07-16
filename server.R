# ############################################################################
# GDA Passive Individuen verorten
# ############################################################################
# devtools::install_github("Inventionate/TimeSpaceAnalysis")
library("shiny", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("TimeSpaceAnalysis", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
# ############################################################################
# Geometrisches Modell laden 
# ############################################################################
load("geometrisches-modell.RData")
# detach("file:geometrisches-modell.RData")

shinyServer(function(input, output) {
  
  sup_ind <- reactive({
    sup_ind <- cbind.data.frame(input$wohnort, 
                                input$entfernung_ph, 
                                input$finanzierung_eltern,
                                input$finanzierung_arbeit_vorlesungszeit,
                                input$finanzierung_arbeit_semesterferien, 
                                input$zeitmuster, 
                                input$lehrveranstaltungen_anzahl, 
                                input$lernort_alleine,
                                input$selbstorganisierte_lerngruppe, 
                                input$aufmerksamkeit, 
                                input$sitzplatz,
                                input$sitzplatz_identisch, 
                                input$ph_hsb_recherchieren, 
                                input$ph_hsb_lernen,
                                input$mensa_essen, 
                                input$cafete_essen, 
                                input$computerräume_nutzen, 
                                input$campus_lernen, 
                                input$campus_entspannen, 
                                input$austausch_lehrende, 
                                input$ph_kommilitoninnen_entspannen, 
                                input$ph_kommilitoninnen_lernen,
                                input$ka_essen, 
                                input$ka_entspannen, 
                                input$bahn_lernen, 
                                input$fremder_wohnort_lernen, 
                                input$kommilitoninnen_treffen,
                                input$kommilitoninnen_lernen, 
                                input$website_nutzen,
                                input$web_services_andere_nutzen, 
                                input$emails_lehrende,
                                input$emails_kommilitoninnen, 
                                input$internetrecherche,
                                input$soziale_netzwerke_studieren, 
                                input$fernsehen,
                                input$musik, 
                                input$kino, 
                                input$freunde_treffen, 
                                input$soziale_netzwerke,
                                input$internet_surfen, 
                                input$chillen, 
                                input$zeitungen,
                                input$disco, 
                                input$sport, 
                                input$familie, 
                                input$shoppen, 
                                input$kirche)
    
    colnames(sup_ind) <- colnames(mca_studienalltag$call$X)
    
    data.frame(sup_ind)
  })
  
  
  output$mca_studienalltag_sup_ind <- renderPlot({
    
    fviz_add_sup_ind(res_gda = mca_studienalltag, sup_ind = sup_ind(), label = input$name,
                     group = c(5, 7, 10, 6,  6, 13), size = 6, myriad = FALSE,
                     group_names = c("Persönliche Situation", "Gewöhnliche Studienwoche",
                                     "Studienaktivitäten an der PH", "Studienaktivitäten außerhalb der PH",
                                     "Studienaktivitäten Internet", "Freizeitaktivitäten")) +
      xlab(paste0("Achse 1 (", round(mca_studienalltag$eig$`percentage of variance`[1], 1), "%)")) +
      ylab(paste0("Achse 2 (", round(mca_studienalltag$eig$`percentage of variance`[2], 1), "%)")) +
      ggtitle("Studienalltag (WS15/16)")
  })
  
  # output$mca_studienalltag_sup_ind_tab <- renderPrint({
  #   sup_ind()[,3]
  # })
  
})
