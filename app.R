#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#alle variable in einem Sidebar panel links with scroll bar und Ergebnisse rechts fixed. See Kosten.!! 
#Change color of scroll bar -> sichtbar. 
#https://www.r-bloggers.com/2022/06/scrollbar-for-the-shiny-sidebar/
#put explanation of graphics 
#scroll color 

#change the presented grapha: legend, labels... 


library(shiny)
library(bslib)
library(shinythemes)
library(ggplot2)


# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* For Chrome, Safari, and Edge */
      ::-webkit-scrollbar {
        width: 10px;
      }
      ::-webkit-scrollbar-track {
        background: #F1F1F1;
      }
      ::-webkit-scrollbar-thumb {
        background: tomato;
      }
      ::-webkit-scrollbar-thumb:hover {
        background: darkred;
      }
      /* For Firefox */
      body {
        scrollbar-width: thin;
        scrollbar-color: red #F1F1F1;
      }
    "))
  ),
  # Application title
  titlePanel("Monte Carlo simulations 'HortiPrimed'"),
  theme = bs_theme(version = 5, bootswatch = "minty"), 
  tabsetPanel(               
    # tabPanel("Tab 1", h1("First tab"),
    #          h2("HortiPrimed")),
    # tabPanel("Seedling",h1("Tomatenjungepflanzeproduktion")
    # ),
    
    tabPanel("Tomato", h1("Tomatoproduktion"),
             fluidRow(
               textInput("Datum", "Datum"),
               textInput("Betrieb", "Betrieb (optional)"),
               textInput("name", "Name (optional)")
             ),
             fluidRow(
               sidebarPanel(width = 12,
                            fluidRow( h4( strong('Anwendung des Models')),
                                      h6 (HTML("<b>1.</b> Bitte geben Sie Ihre geschäzte Kennzahlen ein.")),
                                      h6 (HTML("<b>2.</b>. Wenn Sie alle Kennzahlen eingegeben haben, klicken Sie '<b>Speichern</b>'.")),
                                      h6 (HTML("<b>3.</b> Klicken Sie '<b>Download</b>', sodass Ihre Ergebnisse als ein CSV file 
                                               in Ihrem Computer/ Laptop herungergeladen wird.")),
                                      h6 (HTML("<b>4.</b> Senden Sie bitte Ihr CSV file an '<b>s62smoon@uni-bonn.de</b>'. 
                                               Bei Fragen wenden Sie sich bitte an die genannte E-mail-Adresse."))
                                      
                            ))),
             
             
             fluidRow(
               
               column(7, 
                      p(),
                      h3(strong('1. Leistung')),
                      
                      sidebarPanel(width = 12,
                        style = "height: 30vh; overflow-y: auto;", #height in % 
                        
                        
                        sliderInput("Ertrag",
                                    HTML("<b>Ertrag (kg/m2):</b>"),
                                    min = 1,
                                    max = 100,
                                    value = c(40, 60),
                                    step = 1), # kg/m2 = 10 t/ha
                        sliderInput("Tomaten_Preis",
                                    HTML("<b>Tomatenpreis (€/kg):</b>"),
                                    min = 0.1,
                                    max = 5,
                                    value = c(0.5, 1.1),
                                    step = 0.1),
                        sliderInput("Priming_Effect",
                                    HTML("<b>Priming-Effekt auf Ertrag (%):</b>"),
                                    min = 0.1,
                                    max = 20,
                                    value = c(0.1, 1.1),
                                    step = 0.1),
                        #priming negative rein (5%)),
                      ),
                      p(),
                      h3(strong('2. Kosten')),
                      sidebarPanel(width = 12,
                        style = "height: 60vh; overflow-y: auto;", #height in %
                        
                        
                        h4(strong('2. 1. Direktkosten')),
                        sliderInput("Jungpflanzen_Menge",
                                    HTML("<b>Jungpflanzen (Pflanzen/ha):</b>"),
                                    min = 20000,
                                    max = 40000,
                                    value = c(28000, 29000),
                                    step = 1000),
                        sliderInput("Jungpflanzen_Preis",
                                    HTML("<b>Jungpflanzen Preis (€/1000Pflanzen):</b>"),
                                    min = 8000,
                                    max = 18000,
                                    value = c(12000, 13000),
                                    step = 100),
                        sliderInput("Priming_Jungpflanzen",
                                    HTML( "<b>Primed Jungpflanzen (%):</b>"),
                                    min = 0.1,
                                    max = 30,
                                    value = c(0.1, 1.0),
                                    step = 0.1),
                        sliderInput("Du_Bewaesserung",
                                    HTML( "<b>Düngung und Bewässerung(€/ha):</b>"),
                                    min = 10000,
                                    max = 15000,
                                    value = c(11500,12000),
                                    step = 100),
                        sliderInput("Heizmaterial",
                                    HTML( "<b>Heizmaterial (70 %Holzhackschnitzel, 30 %Erdgas) (€/ha):</b>"),
                                    min = 80000,
                                    max = 100000,
                                    value = c(95000,97000),
                                    step = 1000),
                        sliderInput("Strom",
                                    HTML( "<b>Strom(€/ha):</b>"),
                                    min = 10000,
                                    max = 50000,
                                    value = c(23000,24000),
                                    step = 1000),
                        sliderInput("Matten_Folien",
                                    HTML( "<b>Matten und Folien(€/ha):</b>"),
                                    min = 1500,
                                    max = 4000,
                                    value =  c(2200,2400),
                                    step=100),
                        sliderInput("CO2",
                                    HTML( "<b>C02(€/ha):</b>"),
                                    min = 10000,
                                    max = 50000,
                                    value =  c(25000,35000),
                                    step=1000),
                        sliderInput("Pflanzenschutz",
                                    HTML( "<b>Pflanzenschutz(€/ha):</b>"),
                                    min = 1000,
                                    max = 10000,
                                    value =  c(5500,6500),
                                    step=100),
                        sliderInput("Verpackung",
                                    HTML("<b>Verpackung(€/ha):</b>"),
                                    min = 40000,
                                    max = 80000,
                                    value =  c(63000,64000),
                                    step=1000),
                        sliderInput("Transport",
                                    HTML("<b>Transport(€/ha):</b>"),
                                    min = 15000,
                                    max = 50000,
                                    value =  c(29000,30000),
                                    step=1000),
                        sliderInput("Re_Biowaste",
                                    HTML("<b>Reduzierte Entsorgung (%) :</b>
                                          Wie viel Prozent der Biowaste durch Priming reduzieren können"),
                                    min = 1,
                                    max = 50,
                                    value =  c(1,5),
                                    step=1),
                        sliderInput("Entsorgung_Menge",
                                    HTML( "<b>Entsorgung (t/ha):</b>"),
                                    min = 10,
                                    max = 100,
                                    value =  c(30,38),
                                    step=1),
                        sliderInput("Entsorgung_Preis",
                                    HTML("<b>Entsorgungspreis(€/t):</b>"),
                                    min = 10,
                                    max = 100,
                                    value =  c(35,40),
                                    step=5),
                        sliderInput("Vermarktungsgebuehr",
                                    HTML("<b>Vermarktungsgebühren (3 %) (€/ha):</b>"),
                                    min = 10000,
                                    max = 50000,
                                    value =  c(20500,30000),
                                    step=100),
                        sliderInput("sonstige_Betriebsmittel",
                                    HTML("<b>Sonstige Betriebsmittel(€/ha):</b>"),
                                    min = 100,
                                    max = 10000,
                                    value =  c(7900,8000),
                                    step=100),  
                        h4( strong('2. 2. Arbeitserledigungskosten')),
                        sliderInput("fix_Lohnkosten",
                                    HTML( "<b>Fixe Lohnkosten(€/ha):</b>"),
                                    min = 10000,
                                    max = 130000,
                                    value =  c(80000,87000),
                                    step=1000),
                        
                        sliderInput("variable_Kosten",
                                    HTML("<b>Variable Lohnkosten(€/ha):</b>"),
                                    min = 10000,
                                    max = 130000,
                                    value =  c(80000,87000),
                                    step=1000),
                        sliderInput("Dienstleistungen",
                                    HTML("<b>Dienstleistungen(€/ha):</b>"),
                                    min = 2000,
                                    max = 10000,
                                    value =  c(6500,6800),
                                    step=100)
                        ),
                      p(),
                      h4(strong('sonstige Variable')),
                       sidebarPanel(width = 12,
                                    style = "height: 25vh; overflow-y: auto;", #height in %
                        
                        sliderInput("discount_rate",
                                    HTML("<b>Discount rate (%):</b>"),
                                    min = 1,
                                    max = 100,
                                    value =  c(1,10),
                                    step=1),
                        sliderInput("n_years",
                                    HTML( "<b>Produktionszeit horizon (years):</b>"),
                                    min = 1,
                                    max = 30,
                                    value =  10,
                                    step=1),
                        sliderInput("var_CV",
                                    HTML("<b>coeff. Variation:</b>"),
                                    min = 1,
                                    max = 100,
                                    value =  c(1,10),
                                    step=1),
                        h4(strong('Simuation')),
                        numericInput("num_simulations", 
                                     HTML( "<b>Simulationsnummer:</b>"), 
                                     min = 1000, 
                                     max = 1000000,
                                     step = 100,
                                     value = 1000)
                       )
                      
               ), # close left column
               column(5, 
                        p(),
                        h3( strong('Ergebnisse')),
                        h5(strong("1. The Net Present Value (NPV)")),
                        plotOutput("distPlot1",height = "250px",
                                   width = "95%"),
                        p ("Der NPV (Kapitalwert) gibt die „Häufigkeit“ 
                           an, dass jedes Ergebnis der Verteilung realisiert wurde, 
                           als das Modell simuliert wurde."),
                        # h5("2.Title"),
                        # plotOutput("distPlot2",height = "250px",
                        #            width = "95%"),
                        # p("explanation"), 
                        h5(strong("2. Cash flow")),
                        plotOutput("distPlot3",height = "250px",
                                   width = "95%"),
                        p("Der Cashflow ist eine Reihe von Geldbeträgen, d
                          ie entweder negativ (z. B. anfängliche Investitionskosten von 
                          Maßnahmen) oder positiv (z. B. zusätzlicher Umsatz, der durch 
                          Maßnahmen in einem bestimmten Jahr erzielt wird) über einen 
                          bestimmten Zeitraum sind.
                          In der Abbildung wird die Unsicherheit 
                          des Cashflows durch Quantile um den Median herum dargestellt.")
                        
                        
                             
               ) # close the right side column
             ), #close FluidRow

             fluidRow(
               sidebarPanel(width = 12,
                            fluidRow( h3( strong('Sonstiges')),
                                      textAreaInput(width = "100%", "Verbesserung", 
                                                    "Gibt es noch fehlende Variable? Was könnte es in diesem Simulationsmodel verbessert werden?")))
               
             ),
             #Button which appends row to the existing dataframe
             actionButton("submit", "Speichern"),
             
             #Button to save the file
             downloadButton('downloadData', 'Download')
    )) #TabPanel and TapSetPanel closed

) # UI closed

# Define server logic required to draw a histogram
server <- function(input, output) {
  library(decisionSupport)
  library(ggplot2)
  
  output$distPlot1 <- renderPlot({
    
    input_estimates <- data.frame(variable = c("Ertrag","Tomaten_Preis","Priming_Effect",
                                               "Jungpflanzen_Menge","Jungpflanzen_Preis",
                                               "Priming_Jungpflanzen","Du_Bewaesserung",
                                               "Heizmaterial",
                                               "Strom","Matten_Folien","CO2",
                                               "Pflanzenschutz","Verpackung","Transport",
                                               "Entsorgung_Menge", "Entsorgung_Preis","Re_Biowaste","Vermarktungsgebuehr",
                                               "sonstige_Betriebsmittel",
                                               "fix_Lohnkosten","variable_Kosten",
                                               "Dienstleistungen",
                                               "discount_rate","n_years",
                                               "var_CV"),
                                  lower = c(min(input$Ertrag), min(input$Tomaten_Preis),min(input$Priming_Effect),
                                            min(input$Jungpflanzen_Menge), min(input$Jungpflanzen_Preis),min(input$Priming_Jungpflanzen),
                                            min(input$Du_Bewaesserung), min(input$Heizmaterial),min(input$Strom),
                                            min(input$Matten_Folien), min(input$CO2),min(input$Pflanzenschutz),
                                            min(input$Verpackung), min(input$Transport),min(input$Entsorgung_Menge),min(input$Entsorgung_Preis),min(input$Re_Biowaste),
                                            min(input$Vermarktungsgebuehr), min(input$sonstige_Betriebsmittel),min(input$fix_Lohnkosten),
                                            min(input$variable_Kosten), min(input$Dienstleistungen),min(input$discount_rate),
                                            min(input$n_years),min(input$var_CV)),
                                  median = NA,
                                  upper = c(max(input$Ertrag), max(input$Tomaten_Preis),max(input$Priming_Effect),
                                            max(input$Jungpflanzen_Menge), max(input$Jungpflanzen_Preis),max(input$Priming_Jungpflanzen),
                                            max(input$Du_Bewaesserung), max(input$Heizmaterial),max(input$Strom),
                                            max(input$Matten_Folien), max(input$CO2),max(input$Pflanzenschutz),
                                            max(input$Verpackung), max(input$Transport),max(input$Entsorgung_Menge),max(input$Entsorgung_Preis),max(input$Re_Biowaste),
                                            max(input$Vermarktungsgebuehr), max(input$sonstige_Betriebsmittel),max(input$fix_Lohnkosten),
                                            max(input$variable_Kosten), max(input$Dienstleistungen),max(input$discount_rate),
                                            max(input$n_years),max(input$var_CV)),
                                  distribution = c("posnorm", "posnorm", "posnorm", "posnorm",
                                                   "posnorm", "posnorm","posnorm", "posnorm",
                                                   "posnorm", "posnorm", "posnorm", "posnorm",
                                                   "posnorm", "posnorm", "posnorm", "posnorm","posnorm",
                                                   "posnorm", "posnorm", "posnorm", "posnorm",
                                                   "posnorm",  "posnorm","const","posnorm"),
                                  label = c("Ertrag (kg/m2)","Tomaten Preis (€/kg)","Effekte von Priming auf Ertrag(%)",
                                            "Jungpflanzen (Pflanzen/ha)","Jungpflanzen Preis (€/Pflanzen)","Primed Jungpflanzen (%)",
                                            "Düngung und Bewässerung(€/ha)",
                                            "Heizmaterial (70 %Holzhackschnitzel, 30 %Erdgas) (€/ha)",
                                            "Strom(€/ha)","Matten und Folien(€/ha)","C02(€/ha)",
                                            "Pflanzenschutz(€/ha)","Verpackung(€/ha)","Transport(€/ha)",
                                            "Entsorgung (t/ha)","Entsorgungspreis(€/t)","Reduzierte Entsorgung (%)",
                                            "Vermarktungsgebühren (3 %) (€/ha)",
                                            "Sonstige Betriebsmittel(€/ha)",
                                            "Fixe Lohnkosten(€/ha)","Variable Lohnkosten(€/ha)",
                                            "Dienstleistungen(€/ha)",
                                            "Discount rate (%)","Production time horizon (years)",
                                            "coeff. Variation"))
    
    
    
    
    model_function <- function(){
      #Leistung
      Ertragsmenge <-vv(Ertrag, var_CV, n_years)
      Tomatenpreis <- vv(Tomaten_Preis, var_CV, n_years)
      Priming_effect1 <- vv(Priming_Effect, var_CV, n_years)
      
      
      Leistung <- Ertragsmenge*Tomatenpreis*10000
      Primed_Leistung <- Ertragsmenge*(1 + Priming_effect1) * Tomatenpreis* 10000
      
      
      
      #Ausgabe
      #Direkt Kosten 
      Jungpflanzenmenge <- vv(Jungpflanzen_Menge, var_CV, n_years)
      Jungpflanzenpreis <- vv(Jungpflanzen_Preis, var_CV, n_years)
      Primed_Jungpflanzenpreis <- vv(Priming_Jungpflanzen, var_CV, n_years)
      Heizmaterial1 <- vv(Heizmaterial, var_CV, n_years)
      Strom1 <- vv(Strom, var_CV, n_years)
      Matten_Folien1 <- vv(Matten_Folien, var_CV, n_years)
      Du_Bewaesserung1 <- vv(Du_Bewaesserung, var_CV, n_years)
      CO21 <- vv(CO2, var_CV, n_years)
      Pflanzenschutz1 <- vv(Pflanzenschutz, var_CV, n_years)
      Verpackung1 <- vv(Verpackung, var_CV, n_years)
      Transport1 <- vv(Transport, var_CV, n_years)
      Entsorgungsmenge <- vv(Entsorgung_Menge, var_CV, n_years)
      Entsorgungspreis <- vv(Entsorgung_Preis, var_CV, n_years)
      Vermarktungsgebuehr1 <- vv(Vermarktungsgebuehr, var_CV, n_years)
      sonstige_Betriebsmittel1 <- vv(sonstige_Betriebsmittel, var_CV, n_years)
      ReBiowaste <- vv(Re_Biowaste, var_CV, n_years)
      
      
      Direkt_Kosten_Priming <- Heizmaterial1 + Strom1 + (Jungpflanzenmenge*Jungpflanzenpreis/1000)*(1 + Primed_Jungpflanzenpreis) + Matten_Folien1 + Du_Bewaesserung1 + 
        CO21 + Pflanzenschutz1 + Verpackung1 + Transport1 + (Entsorgungsmenge*Entsorgungspreis)*(1-ReBiowaste) + Vermarktungsgebuehr1 + 
        sonstige_Betriebsmittel1 
      Direkt_Kosten_ohne_Priming <- Heizmaterial1 + Strom1 + (Jungpflanzenmenge*Jungpflanzenpreis/1000) + Matten_Folien1 + Du_Bewaesserung1 + 
        CO21 + Pflanzenschutz1 + Verpackung1 + Transport1 + (Entsorgungsmenge*Entsorgungspreis) + Vermarktungsgebuehr1 + 
        sonstige_Betriebsmittel1
      
      # Arbeitserledigungskosten
      variable_Kosten1 <- vv(variable_Kosten, var_CV, n_years)
      Dienstleistungen1 <- vv(Dienstleistungen, var_CV, n_years)
      fix_Lohnkosten1 <- vv(fix_Lohnkosten, var_CV, n_years)
      
      Arbeitserledigungskosten <- variable_Kosten1 + Dienstleistungen1 + fix_Lohnkosten1
      
      
      
      ## Kosten für Tomatenanbau ##
      
      Kosten_mit_Priming <-
        Direkt_Kosten_Priming + Arbeitserledigungskosten
      
      Kosten_ohne_Priming <- Direkt_Kosten_ohne_Priming + Arbeitserledigungskosten
      
      
      ## Total_ Gewinn_Kosten ##
      
      
      net_mit_Priming <- Primed_Leistung - Kosten_mit_Priming
      result_mit_Priming <- net_mit_Priming
      net_ohne_Priming <- Leistung - Kosten_ohne_Priming
      result_ohne_Priming <- net_ohne_Priming
      
      
      #NPV calculate
      
      NPV_mit_Priming <-
        discount(result_mit_Priming, discount_rate, calculate_NPV = T)
      
      
      NPV_ohne_Priming <-
        discount(result_ohne_Priming, discount_rate, calculate_NPV = T)
      
      
      return(list(Priming_NPV =  NPV_mit_Priming,
                  No_Priming_NPV = NPV_ohne_Priming, 
                  NPV_imle_Priming =  NPV_mit_Priming - NPV_ohne_Priming, 
                  Cashflow_mit_Priming =  result_mit_Priming - result_ohne_Priming)) 
    }
    
    # Run the Monte Carlo simulation using the model function
    mcSimulation_results1 <- mcSimulation(estimate = as.estimate(input_estimates),
                                          model_function = model_function,
                                          numberOfModelRuns = input$num_simulations,
                                          functionSyntax = "plainNames")
    
    
    plot1 <-  plot_distributions(mcSimulation_object = mcSimulation_results1,
                                 vars = "NPV_imle_Priming",
                                 method = "boxplot_density",
                                 colors = "mediumseagreen",
                                 x_axis_name = "Outcome distribution",
                                 old_names = "NPV_imle_Priming",
                                 new_names = "NPV with Priming")
  
    
    plot1 
    
    
    
    
    
  })
  
  
  output$distPlot2 <- renderPlot({
    
    input_estimates <- data.frame(variable = c("Ertrag","Tomaten_Preis","Priming_Effect",
                                               "Jungpflanzen_Menge","Jungpflanzen_Preis",
                                               "Priming_Jungpflanzen","Du_Bewaesserung",
                                               "Heizmaterial",
                                               "Strom","Matten_Folien","CO2",
                                               "Pflanzenschutz","Verpackung","Transport",
                                               "Entsorgung_Menge", "Entsorgung_Preis","Re_Biowaste","Vermarktungsgebuehr",
                                               "sonstige_Betriebsmittel",
                                               "fix_Lohnkosten","variable_Kosten",
                                               "Dienstleistungen",
                                               "discount_rate","n_years",
                                               "var_CV"),
                                  lower = c(min(input$Ertrag), min(input$Tomaten_Preis),min(input$Priming_Effect),
                                            min(input$Jungpflanzen_Menge), min(input$Jungpflanzen_Preis),min(input$Priming_Jungpflanzen),
                                            min(input$Du_Bewaesserung), min(input$Heizmaterial),min(input$Strom),
                                            min(input$Matten_Folien), min(input$CO2),min(input$Pflanzenschutz),
                                            min(input$Verpackung), min(input$Transport),min(input$Entsorgung_Menge),min(input$Entsorgung_Preis),min(input$Re_Biowaste),
                                            min(input$Vermarktungsgebuehr), min(input$sonstige_Betriebsmittel),min(input$fix_Lohnkosten),
                                            min(input$variable_Kosten), min(input$Dienstleistungen),min(input$discount_rate),
                                            min(input$n_years),min(input$var_CV)),
                                  median = NA,
                                  upper = c(max(input$Ertrag), max(input$Tomaten_Preis),max(input$Priming_Effect),
                                            max(input$Jungpflanzen_Menge), max(input$Jungpflanzen_Preis),max(input$Priming_Jungpflanzen),
                                            max(input$Du_Bewaesserung), max(input$Heizmaterial),max(input$Strom),
                                            max(input$Matten_Folien), max(input$CO2),max(input$Pflanzenschutz),
                                            max(input$Verpackung), max(input$Transport),max(input$Entsorgung_Menge),max(input$Entsorgung_Preis),max(input$Re_Biowaste),
                                            max(input$Vermarktungsgebuehr), max(input$sonstige_Betriebsmittel),max(input$fix_Lohnkosten),
                                            max(input$variable_Kosten), max(input$Dienstleistungen),max(input$discount_rate),
                                            max(input$n_years),max(input$var_CV)),
                                  distribution = c("posnorm", "posnorm", "posnorm", "posnorm",
                                                   "posnorm", "posnorm","posnorm", "posnorm",
                                                   "posnorm", "posnorm", "posnorm", "posnorm",
                                                   "posnorm", "posnorm", "posnorm", "posnorm","posnorm",
                                                   "posnorm", "posnorm", "posnorm", "posnorm",
                                                   "posnorm",  "posnorm","const","posnorm"),
                                  label = c("Ertrag (kg/m2)","Tomaten Preis (€/kg)","Effekte von Priming auf Ertrag(%)",
                                            "Jungpflanzen (Pflanzen/ha)","Jungpflanzen Preis (€/Pflanzen)","Primed Jungpflanzen (%)",
                                            "Düngung und Bewässerung(€/ha)",
                                            "Heizmaterial (70 %Holzhackschnitzel, 30 %Erdgas) (€/ha)",
                                            "Strom(€/ha)","Matten und Folien(€/ha)","C02(€/ha)",
                                            "Pflanzenschutz(€/ha)","Verpackung(€/ha)","Transport(€/ha)",
                                            "Entsorgung (t/ha)","Entsorgungspreis(€/t)","Reduzierte Entsorgung (%)",
                                            "Vermarktungsgebühren (3 %) (€/ha)",
                                            "Sonstige Betriebsmittel(€/ha)",
                                            "Fixe Lohnkosten(€/ha)","Variable Lohnkosten(€/ha)",
                                            "Dienstleistungen(€/ha)",
                                            "Discount rate (%)","Production time horizon (years)",
                                            "coeff. Variation"))
    
    
    
    
    model_function <- function(){
      #Leistung
      Ertragsmenge <-vv(Ertrag, var_CV, n_years)
      Tomatenpreis <- vv(Tomaten_Preis, var_CV, n_years)
      Priming_effect1 <- vv(Priming_Effect, var_CV, n_years)
      
      
      Leistung <- Ertragsmenge*Tomatenpreis*10000
      Primed_Leistung <- Ertragsmenge*(1 + Priming_effect1) * Tomatenpreis* 10000
      
      
      
      #Ausgabe
      #Direkt Kosten 
      Jungpflanzenmenge <- vv(Jungpflanzen_Menge, var_CV, n_years)
      Jungpflanzenpreis <- vv(Jungpflanzen_Preis, var_CV, n_years)
      Primed_Jungpflanzenpreis <- vv(Priming_Jungpflanzen, var_CV, n_years)
      Heizmaterial1 <- vv(Heizmaterial, var_CV, n_years)
      Strom1 <- vv(Strom, var_CV, n_years)
      Matten_Folien1 <- vv(Matten_Folien, var_CV, n_years)
      Du_Bewaesserung1 <- vv(Du_Bewaesserung, var_CV, n_years)
      CO21 <- vv(CO2, var_CV, n_years)
      Pflanzenschutz1 <- vv(Pflanzenschutz, var_CV, n_years)
      Verpackung1 <- vv(Verpackung, var_CV, n_years)
      Transport1 <- vv(Transport, var_CV, n_years)
      Entsorgungsmenge <- vv(Entsorgung_Menge, var_CV, n_years)
      Entsorgungspreis <- vv(Entsorgung_Preis, var_CV, n_years)
      Vermarktungsgebuehr1 <- vv(Vermarktungsgebuehr, var_CV, n_years)
      sonstige_Betriebsmittel1 <- vv(sonstige_Betriebsmittel, var_CV, n_years)
      ReBiowaste <- vv(Re_Biowaste, var_CV, n_years)
      
      
      Direkt_Kosten_Priming <- Heizmaterial1 + Strom1 + (Jungpflanzenmenge*Jungpflanzenpreis/1000)*(1 + Primed_Jungpflanzenpreis) + Matten_Folien1 + Du_Bewaesserung1 + 
        CO21 + Pflanzenschutz1 + Verpackung1 + Transport1 + (Entsorgungsmenge*Entsorgungspreis)*(1-ReBiowaste) + Vermarktungsgebuehr1 + 
        sonstige_Betriebsmittel1 
      Direkt_Kosten_ohne_Priming <- Heizmaterial1 + Strom1 + (Jungpflanzenmenge*Jungpflanzenpreis/1000) + Matten_Folien1 + Du_Bewaesserung1 + 
        CO21 + Pflanzenschutz1 + Verpackung1 + Transport1 + (Entsorgungsmenge*Entsorgungspreis) + Vermarktungsgebuehr1 + 
        sonstige_Betriebsmittel1
      
      # Arbeitserledigungskosten
      variable_Kosten1 <- vv(variable_Kosten, var_CV, n_years)
      Dienstleistungen1 <- vv(Dienstleistungen, var_CV, n_years)
      fix_Lohnkosten1 <- vv(fix_Lohnkosten, var_CV, n_years)
      
      Arbeitserledigungskosten <- variable_Kosten1 + Dienstleistungen1 + fix_Lohnkosten1
      
      
      
      ## Kosten für Tomatenanbau ##
      
      Kosten_mit_Priming <-
        Direkt_Kosten_Priming + Arbeitserledigungskosten
      
      Kosten_ohne_Priming <- Direkt_Kosten_ohne_Priming + Arbeitserledigungskosten
      
      
      ## Total_ Gewinn_Kosten ##
      
      
      net_mit_Priming <- Primed_Leistung - Kosten_mit_Priming
      result_mit_Priming <- net_mit_Priming
      net_ohne_Priming <- Leistung - Kosten_ohne_Priming
      result_ohne_Priming <- net_ohne_Priming
      
      
      #NPV calculate
      
      NPV_mit_Priming <-
        discount(result_mit_Priming, discount_rate, calculate_NPV = T)
      
      
      NPV_ohne_Priming <-
        discount(result_ohne_Priming, discount_rate, calculate_NPV = T)
      
      
      return(list(Priming_NPV =  NPV_mit_Priming,
                  No_Priming_NPV = NPV_ohne_Priming, 
                  NPV_imle_Priming =  NPV_mit_Priming - NPV_ohne_Priming, 
                  Cashflow_mit_Priming =  result_mit_Priming - result_ohne_Priming)) 
    }
    
    # Run the Monte Carlo simulation using the model function
    mcSimulation_results1 <- mcSimulation(estimate = as.estimate(input_estimates),
                                          model_function = model_function,
                                          numberOfModelRuns = input$num_simulations,
                                          functionSyntax = "plainNames")
    
    
    
    ### Need code below if I want to combine many plots in a tabPanel ###
    
    plot2 <-   plot_distributions(mcSimulation_object = mcSimulation_results1,
                                  vars = c("Priming_NPV", "No_Priming_NPV"),
                                  method = 'smooth_simple_overlay',
                                  colors = c("tomato","wheat1"),
                                  base_size = 16,
                                  )
    plot2
    

    
  })
  
  
  output$distPlot3 <- renderPlot({
    
    input_estimates <- data.frame(variable = c("Ertrag","Tomaten_Preis","Priming_Effect",
                                               "Jungpflanzen_Menge","Jungpflanzen_Preis",
                                               "Priming_Jungpflanzen","Du_Bewaesserung",
                                               "Heizmaterial",
                                               "Strom","Matten_Folien","CO2",
                                               "Pflanzenschutz","Verpackung","Transport",
                                               "Entsorgung_Menge", "Entsorgung_Preis","Re_Biowaste","Vermarktungsgebuehr",
                                               "sonstige_Betriebsmittel",
                                               "fix_Lohnkosten","variable_Kosten",
                                               "Dienstleistungen",
                                               "discount_rate","n_years",
                                               "var_CV"),
                                  lower = c(min(input$Ertrag), min(input$Tomaten_Preis),min(input$Priming_Effect),
                                            min(input$Jungpflanzen_Menge), min(input$Jungpflanzen_Preis),min(input$Priming_Jungpflanzen),
                                            min(input$Du_Bewaesserung), min(input$Heizmaterial),min(input$Strom),
                                            min(input$Matten_Folien), min(input$CO2),min(input$Pflanzenschutz),
                                            min(input$Verpackung), min(input$Transport),min(input$Entsorgung_Menge),min(input$Entsorgung_Preis),min(input$Re_Biowaste),
                                            min(input$Vermarktungsgebuehr), min(input$sonstige_Betriebsmittel),min(input$fix_Lohnkosten),
                                            min(input$variable_Kosten), min(input$Dienstleistungen),min(input$discount_rate),
                                            min(input$n_years),min(input$var_CV)),
                                  median = NA,
                                  upper = c(max(input$Ertrag), max(input$Tomaten_Preis),max(input$Priming_Effect),
                                            max(input$Jungpflanzen_Menge), max(input$Jungpflanzen_Preis),max(input$Priming_Jungpflanzen),
                                            max(input$Du_Bewaesserung), max(input$Heizmaterial),max(input$Strom),
                                            max(input$Matten_Folien), max(input$CO2),max(input$Pflanzenschutz),
                                            max(input$Verpackung), max(input$Transport),max(input$Entsorgung_Menge),max(input$Entsorgung_Preis),max(input$Re_Biowaste),
                                            max(input$Vermarktungsgebuehr), max(input$sonstige_Betriebsmittel),max(input$fix_Lohnkosten),
                                            max(input$variable_Kosten), max(input$Dienstleistungen),max(input$discount_rate),
                                            max(input$n_years),max(input$var_CV)),
                                  distribution = c("posnorm", "posnorm", "posnorm", "posnorm",
                                                   "posnorm", "posnorm","posnorm", "posnorm",
                                                   "posnorm", "posnorm", "posnorm", "posnorm",
                                                   "posnorm", "posnorm", "posnorm", "posnorm","posnorm",
                                                   "posnorm", "posnorm", "posnorm", "posnorm",
                                                   "posnorm",  "posnorm","const","posnorm"),
                                  label = c("Ertrag (kg/m2)","Tomaten Preis (€/kg)","Effekte von Priming auf Ertrag(%)",
                                            "Jungpflanzen (Pflanzen/ha)","Jungpflanzen Preis (€/Pflanzen)","Primed Jungpflanzen (%)",
                                            "Düngung und Bewässerung(€/ha)",
                                            "Heizmaterial (70 %Holzhackschnitzel, 30 %Erdgas) (€/ha)",
                                            "Strom(€/ha)","Matten und Folien(€/ha)","C02(€/ha)",
                                            "Pflanzenschutz(€/ha)","Verpackung(€/ha)","Transport(€/ha)",
                                            "Entsorgung (t/ha)","Entsorgungspreis(€/t)","Reduzierte Entsorgung (%)",
                                            "Vermarktungsgebühren (3 %) (€/ha)",
                                            "Sonstige Betriebsmittel(€/ha)",
                                            "Fixe Lohnkosten(€/ha)","Variable Lohnkosten(€/ha)",
                                            "Dienstleistungen(€/ha)",
                                            "Discount rate (%)","Production time horizon (years)",
                                            "coeff. Variation"))
    
    
    
    
    model_function <- function(){
      #Leistung
      Ertragsmenge <-vv(Ertrag, var_CV, n_years)
      Tomatenpreis <- vv(Tomaten_Preis, var_CV, n_years)
      Priming_effect1 <- vv(Priming_Effect, var_CV, n_years)
      
      
      Leistung <- Ertragsmenge*Tomatenpreis*10000
      Primed_Leistung <- Ertragsmenge*(1 + Priming_effect1) * Tomatenpreis* 10000
      
      
      
      #Ausgabe
      #Direkt Kosten 
      Jungpflanzenmenge <- vv(Jungpflanzen_Menge, var_CV, n_years)
      Jungpflanzenpreis <- vv(Jungpflanzen_Preis, var_CV, n_years)
      Primed_Jungpflanzenpreis <- vv(Priming_Jungpflanzen, var_CV, n_years)
      Heizmaterial1 <- vv(Heizmaterial, var_CV, n_years)
      Strom1 <- vv(Strom, var_CV, n_years)
      Matten_Folien1 <- vv(Matten_Folien, var_CV, n_years)
      Du_Bewaesserung1 <- vv(Du_Bewaesserung, var_CV, n_years)
      CO21 <- vv(CO2, var_CV, n_years)
      Pflanzenschutz1 <- vv(Pflanzenschutz, var_CV, n_years)
      Verpackung1 <- vv(Verpackung, var_CV, n_years)
      Transport1 <- vv(Transport, var_CV, n_years)
      Entsorgungsmenge <- vv(Entsorgung_Menge, var_CV, n_years)
      Entsorgungspreis <- vv(Entsorgung_Preis, var_CV, n_years)
      Vermarktungsgebuehr1 <- vv(Vermarktungsgebuehr, var_CV, n_years)
      sonstige_Betriebsmittel1 <- vv(sonstige_Betriebsmittel, var_CV, n_years)
      ReBiowaste <- vv(Re_Biowaste, var_CV, n_years)
      
      
      Direkt_Kosten_Priming <- Heizmaterial1 + Strom1 + (Jungpflanzenmenge*Jungpflanzenpreis/1000)*(1 + Primed_Jungpflanzenpreis) + Matten_Folien1 + Du_Bewaesserung1 + 
        CO21 + Pflanzenschutz1 + Verpackung1 + Transport1 + (Entsorgungsmenge*Entsorgungspreis)*(1-ReBiowaste) + Vermarktungsgebuehr1 + 
        sonstige_Betriebsmittel1 
      Direkt_Kosten_ohne_Priming <- Heizmaterial1 + Strom1 + (Jungpflanzenmenge*Jungpflanzenpreis/1000) + Matten_Folien1 + Du_Bewaesserung1 + 
        CO21 + Pflanzenschutz1 + Verpackung1 + Transport1 + (Entsorgungsmenge*Entsorgungspreis) + Vermarktungsgebuehr1 + 
        sonstige_Betriebsmittel1
      
      # Arbeitserledigungskosten
      variable_Kosten1 <- vv(variable_Kosten, var_CV, n_years)
      Dienstleistungen1 <- vv(Dienstleistungen, var_CV, n_years)
      fix_Lohnkosten1 <- vv(fix_Lohnkosten, var_CV, n_years)
      
      Arbeitserledigungskosten <- variable_Kosten1 + Dienstleistungen1 + fix_Lohnkosten1
      
      
      
      ## Kosten für Tomatenanbau ##
      
      Kosten_mit_Priming <-
        Direkt_Kosten_Priming + Arbeitserledigungskosten
      
      Kosten_ohne_Priming <- Direkt_Kosten_ohne_Priming + Arbeitserledigungskosten
      
      
      ## Total_ Gewinn_Kosten ##
      
      
      net_mit_Priming <- Primed_Leistung - Kosten_mit_Priming
      result_mit_Priming <- net_mit_Priming
      net_ohne_Priming <- Leistung - Kosten_ohne_Priming
      result_ohne_Priming <- net_ohne_Priming
      
      
      #NPV calculate
      
      NPV_mit_Priming <-
        discount(result_mit_Priming, discount_rate, calculate_NPV = T)
      
      
      NPV_ohne_Priming <-
        discount(result_ohne_Priming, discount_rate, calculate_NPV = T)
      
      
      return(list(Priming_NPV =  NPV_mit_Priming,
                  No_Priming_NPV = NPV_ohne_Priming, 
                  NPV_imle_Priming =  NPV_mit_Priming - NPV_ohne_Priming, 
                  Cashflow_mit_Priming =  result_mit_Priming - result_ohne_Priming)) 
    }
    
    # Run the Monte Carlo simulation using the model function
    mcSimulation_results1 <- mcSimulation(estimate = as.estimate(input_estimates),
                                          model_function = model_function,
                                          numberOfModelRuns = input$num_simulations,
                                          functionSyntax = "plainNames")
    
    
    
    plot3 <- plot_cashflow(mcSimulation_object = mcSimulation_results1,
                           cashflow_var_name = "Cashflow_mit_Priming",
                           x_axis_name = "year",
                           y_axis_name = "Cashflow in Euro",
                           color_25_75 = "lightblue3",
                           color_5_95 = "aliceblue",
                           color_median = "midnightblue",
                           base_size = 16)
    plot3
    
    
  })
  
  
  Data <- data.frame()
  Results <- reactive(data.frame(variable = c("Datum", "Betrieb", "name", 
                                              "Ertrag","Tomaten_Preis","Priming_Effect",
                                              "Jungpflanzen_Menge","Jungpflanzen_Preis",
                                              "Priming_Jungpflanzen","Du_Bewaesserung",
                                              "Heizmaterial",
                                              "Strom","Matten_Folien","CO2",
                                              "Pflanzenschutz","Verpackung","Transport",
                                              "Entsorgung_Menge", "Entsorgung_Preis","Re_Biowaste","Vermarktungsgebuehr",
                                              "sonstige_Betriebsmittel",
                                              "fix_Lohnkosten","variable_Kosten",
                                              "Dienstleistungen",
                                              "discount_rate","n_years",
                                              "var_CV", "Verbesserung", "Download_Date"),
                                 lower = c(input$Datum,input$Betrieb, input$name,
                                           min(input$Ertrag), min(input$Tomaten_Preis),min(input$Priming_Effect),
                                           min(input$Jungpflanzen_Menge), min(input$Jungpflanzen_Preis),min(input$Priming_Jungpflanzen),
                                           min(input$Du_Bewaesserung), min(input$Heizmaterial),min(input$Strom),
                                           min(input$Matten_Folien), min(input$CO2),min(input$Pflanzenschutz),
                                           min(input$Verpackung), min(input$Transport),min(input$Entsorgung_Menge),min(input$Entsorgung_Preis),min(input$Re_Biowaste),
                                           min(input$Vermarktungsgebuehr), min(input$sonstige_Betriebsmittel),min(input$fix_Lohnkosten),
                                           min(input$variable_Kosten), min(input$Dienstleistungen),min(input$discount_rate),
                                           min(input$n_years),min(input$var_CV),
                                           NA, Sys.Date()),
                                 median = NA,
                                 upper = c(NA, NA, NA,
                                           max(input$Ertrag), max(input$Tomaten_Preis),max(input$Priming_Effect),
                                           max(input$Jungpflanzen_Menge), max(input$Jungpflanzen_Preis),max(input$Priming_Jungpflanzen),
                                           max(input$Du_Bewaesserung), max(input$Heizmaterial),max(input$Strom),
                                           max(input$Matten_Folien), max(input$CO2),max(input$Pflanzenschutz),
                                           max(input$Verpackung), max(input$Transport),max(input$Entsorgung_Menge),max(input$Entsorgung_Preis),max(input$Re_Biowaste),
                                           max(input$Vermarktungsgebuehr), max(input$sonstige_Betriebsmittel),max(input$fix_Lohnkosten),
                                           max(input$variable_Kosten), max(input$Dienstleistungen),max(input$discount_rate),
                                           max(input$n_years),max(input$var_CV),
                                           NA,NA),
                                 distribution = c(NA,NA,NA,
                                                  "posnorm", "posnorm", "posnorm", "posnorm",
                                                  "posnorm", "posnorm","posnorm", "posnorm",
                                                  "posnorm", "posnorm", "posnorm", "posnorm",
                                                  "posnorm", "posnorm", "posnorm", "posnorm","posnorm",
                                                  "posnorm", "posnorm", "posnorm", "posnorm",
                                                  "posnorm",  "posnorm","const","posnorm",NA,NA),
                                 label = c(NA,NA,NA,
                                           "Ertrag (kg/m2)","Tomaten Preis (€/kg)","Effekte von Priming auf Ertrag(%)",
                                           "Jungpflanzen (Pflanzen/ha)","Jungpflanzen Preis (€/Pflanzen)","Primed Jungpflanzen (%)",
                                           "Düngung und Bewässerung(€/ha)",
                                           "Heizmaterial (70 %Holzhackschnitzel, 30 %Erdgas) (€/ha)",
                                           "Strom(€/ha)","Matten und Folien(€/ha)","C02(€/ha)",
                                           "Pflanzenschutz(€/ha)","Verpackung(€/ha)","Transport(€/ha)",
                                           "Entsorgung (t/ha)","Entsorgungspreis(€/t)","Reduzierte Entsorgung (%)",
                                           "Vermarktungsgebühren (3 %) (€/ha)",
                                           "Sonstige Betriebsmittel(€/ha)",
                                           "Fixe Lohnkosten(€/ha)","Variable Lohnkosten(€/ha)",
                                           "Dienstleistungen(€/ha)",
                                           "Discount rate (%)","Production time horizon (years)",
                                           "coeff. Variation", 
                                           input$Verbesserung,NA)))
  
  
  
  #To append the row and display in the table when the submit button is clicked
  observeEvent(input$submit,{
    #Append the row in the dataframe
    Data <<- rbind(Data,Results()) 
    #Display the output in the table
    output$table <- renderTable(Data)
  })
  
  
  #####     Function 1, Create a data download option ####
  
  output$downloadData <- downloadHandler(            # Create the download file name
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(Data, file)                     # put Data() into the download file
    })                          
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
