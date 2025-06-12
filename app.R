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
library(writexl)


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
      .app-header {
      background-color: #ffffff; /* Choose your color */
      padding: 10px; /* Optional: Adds some padding around the title */
      border-radius: 5px; /* Optional: Rounds the corners */
      }
    .tabPanel {
      background-color: #EFF4EF; /* Choose your color */
      padding: 10px; /* Optional: Adds some padding around the title */
      border-radius: 5px; /* Optional: Rounds the corners */
    }
    /* color change for bar */
     .irs-bar,.irs-from, .irs-to, .irs-bar-edge{
        background: #ff6347 !important; /* tomato #ff6347 handle (thumb) */
        border: 1px solid #ff6347;
        border-top:  #ff6347;
    border-bottom: #ff6347;
        
      }
      .irs-line {
        background: #f8f9fa !important; /* Light gray for unselected range */
        border: 1px solid #ff6347;
      }

                    "))
  ),
  # Application title
  titlePanel(
    div(class = "app-header",
        # Logos column
        fluidRow(column(width = 2,
                        align = "right",
                        tags$a(href = "https://www.hortisustain.de/projekte/boel/hortiprimed", # Add URL here
                               tags$img(src = "Logo/hortiprimed_logo.png", height = "90px"))),
                 column(width = 7,
                        align = "center",
                        h1(class = "app-title",
                           "Optimal Priming")),
                 column(width = 3,
                        align = "left",
                        tags$a(href = "https://www.gartenbauwissenschaft.uni-bonn.de/", # Add URL here
                               tags$img(src = "Logo/Bonn.png", height = "90px"))),
                 
                 windowTitle = "MyPage")
    )
  ),
  #theme = bs_theme(version = 5, bootswatch = "minty"), 
  tabsetPanel(               
    
    #English tabPanel ####
    tabPanel("Tomato (EN)", h2("MonteCarlo Simulation with two tomato varieties"),class= "tabPanel",
             fluidRow(
               column(width =4,textInput("Datum_1", "Date")),
               column(width =4,textInput("Betrieb_1", "Operation (optional)")),
               column(width =4,textInput("name_1", "Name (optional)"))
             ),
             fluidRow(
               sidebarPanel(width = 12,
                            fluidRow( h3( strong('What is Optimal Priming?')),
                                      HTML("
                                      <p>The <strong>Optimal Priming</strong> app helps tomato farmers compare the economic performance of two tomato varieties, even when future outcomes are uncertain.</p>
                                      <p>Farmers enter their own data, and the app runs <strong>10,000 simulations</strong> using a method called <em>Monte Carlo simulation</em>. 
                                      This method randomly tests many possible future scenarios to help understand risks and chances.</p>
                                      <p>Based on these simulations, the app gives personalized results showing:</p>
                                      <ul>
                                      <li><strong>Net Present Value (NPV)</strong> – a measure of long-term profitability</li>
                                      <li><strong>Cash flow</strong> – how money comes in and goes out during the production period</li>
                                      </ul>
                                      <p>The app was developed as part of the <strong>HortiPrimed</strong> project, supported by Germany’s <strong>Federal Ministry of Food and Agriculture (BMEL)</strong>.</p>
                                      "),
                                      h3( strong('How to use Optimal Priming')),
                                      h5 (HTML("<b>1.</b> Please enter your estimated and <b> annual</b> values.")),
                                      h5 (HTML("<b>2.</b> Click  <b>'Download'</b> to save your results as a CSV file on your computer or laptop.")),
                                      h5 (HTML("<b>3.</b> Please send your CSV file to <b>'s62smoon@uni-bonn.de'</b>.
                                               If you have any questions, please contact the email address provided."))
                                      
                            ))),
             
             fluidRow(
               sidebarPanel(width = 6, 
                            
                            h3(strong('Variety A')),
                            textInput("1_Sorte_1", 
                                      "Which tomato variety is this?"),
                            sliderInput("a_Ertrag",
                                        "Yield (kg/m²)",
                                        min = 0,
                                        max = 50,
                                        value = c(15, 25),
                                        step = 1), 
                            sliderInput("a_Tomaten_Preis",
                                        "Tomato price (€/kg)",
                                        min = 0.1,
                                        max = 5,
                                        value = c(0.5, 3),
                                        step = 0.1),
                            sliderInput("a_Saatgut",
                                        "Seed cost (€/m²)",
                                        min = 0,
                                        max = 5,
                                        value = c(1.5, 3),
                                        step = 0.1),
                            sliderInput("a_Jungepflanzen",
                                        "Young plant cost (€/m²)",
                                        min = 2,
                                        max = 7,
                                        value = c(2.5, 4.5),
                                        step = 0.1)
               ),
               sidebarPanel(width = 6,
                            h3(strong('Variety B')),
                            textInput("2_Sorte_1", 
                                      "Which tomato variety is this?"),
                            sliderInput("b_Ertrag",
                                        "Yield (kg/m²)",
                                        min = 0,
                                        max = 50,
                                        value = c(15, 25),
                                        step = 1), 
                            sliderInput("b_Tomaten_Preis",
                                        "Tomato price (€/kg)",
                                        min = 0.1,
                                        max = 5,
                                        value = c(0.5, 3),
                                        step = 0.1),
                            sliderInput("b_Saatgut",
                                        "Seed cost (€/m²)",
                                        min = 0,
                                        max = 5,
                                        value = c(1.5, 3),
                                        step = 0.1),
                            sliderInput("b_Jungepflanzen",
                                        "Young plant cost (€/m²)",
                                        min = 2,
                                        max = 7,
                                        value = c(2.5, 4.5),
                                        step = 0.1)
               )),
             fluidRow(
               
               sidebarPanel(width = 6,
                            style = "height: 155vh; overflow-y: auto;", #height in % 
                            
                            h3(strong('2. Costs')),
                            h4(strong('2.1. Direct Costs')),
                            
                            sliderInput("Substrat",
                                        "Substrate (€/m²): If substrates are steamed, 
                                        please enter the combined cost of substrate and steaming.",
                                        min = 3,
                                        max = 15,
                                        value = c(5,8),
                                        step = 1),
                            textInput("Heizmaterial_1", 
                                      "What type of heating material do you use for your tomato production?"),
                            sliderInput("Energie_therm",
                                        "Thermal energy (€/m²)",
                                        min = 1,
                                        max = 20,
                                        value = c(4,8.5),
                                        step = 0.5),
                            sliderInput("Energie_elek",
                                        "Electric energy (€/m²)",
                                        min = 1,
                                        max = 10,
                                        value = c(1.5,3.5),
                                        step = 0.5),
                            sliderInput("CO2_H2O_Due",
                                        "CO₂, water, and fertilizer supply (€/m²)",
                                        min = 0.1,
                                        max = 5,
                                        value =  c(1.5,2.1),
                                        step=0.1),
                            sliderInput("Kordel",
                                        "Twine (€/m²)",
                                        min = 0.1,
                                        max = 3,
                                        value =  c(0.55,0.95),
                                        step=0.05),
                            sliderInput("Hummel_Nutzlinge",
                                        "Bumblebees and beneficial insects (€/m²)",
                                        min = 0.01,
                                        max = 1,
                                        value =  c(0.05,0.1),
                                        step=0.01),
                            sliderInput("PSM_chem",
                                        "Chemical plant protection products (€/m²)",
                                        min = 0.01,
                                        max = 2,
                                        value =  c(0.25,0.35),
                                        step=0.01),
                            sliderInput("Heackseln_Entsorgung",
                                        "Shredding and disposal (€/m²)",
                                        min = 0.01,
                                        max = 2,
                                        value =  c(0.3, 0.45 ),
                                        step=0.01),
                            sliderInput("Desinfektion",
                                        "Disinfection (€/kg)",
                                        min = 0.01,
                                        max = 1,
                                        value =  c(0.01,0.05),
                                        step=0.01),
                            sliderInput("Versicherung",
                                        "Insurance (€/m²)",
                                        min = 0.01,
                                        max = 3,
                                        value =  c(0.55,0.85),
                                        step=0.01),  
                            h4( strong('2.2. Labor Costs')),
                            sliderInput("Arbeit_all",
                                        "General: clearing and cleaning (€/m²)",
                                        min = 1,
                                        max = 20,
                                        value =  c(2,5),
                                        step=0.5),
                            
                            sliderInput("Arbeit_pf",
                                        "Caring: twisting, leaf removal, handling shoots (€/m²)",
                                        min = 1,
                                        max = 30,
                                        value =  c(6,10),
                                        step=0.5),
                            sliderInput("Arbeit_ernte",
                                        "Harvesting and picking (€/m²)",
                                        min = 1,
                                        max = 30,
                                        value =  c(10,20),
                                        step=0.5),
                            h4(strong('Other variable')),
                            sliderInput("Kapitaldienst",
                                        "principal and interest payments (€/m²)",
                                        min = 1,
                                        max = 50,
                                        value =  c(10,15),
                                        step=1),
                            
                            sliderInput("n_years_c",
                                        "Production period (years)",
                                        min = 1,
                                        max = 10,
                                        value =  5,
                                        step=1)
               ),
               column(width = 6, align = "left",
                      mainPanel(width = 11,
                                fluidRow( h3( strong('Results')),
                                          p('These results are based on 10,000 simulations using the data you provided.'),
                                          br(),
                                          h4("1. Distribution of Net Present Value (NPV)"),
                                          plotOutput("distPlot1",height = "350px",
                                                     width = "105%"),
                                          p ("The graph shows the distribution of Net Present Value (NPV) observed in 10,000 model simulations.
                                          The curve represents the estimated probability density of various NPV outcomes and illustrates
                                          the relative likelihood of each result. Our decision analysis provides this probabilistic NPV distribution
                                          as well as projected annual cash flows. NPV itself represents the discounted balance of the expected inflows
                                             and outflows."),
                                          downloadButton("save_plot1", "Save Plot"),
                                          br(),
                                          br(),
                                          h4("2. Cashflow"),
                                          plotOutput("distPlot3",height = "300px",
                                                     width = "105%"),
                                          p(),
                                          HTML('
                                          <p>Cash flow means the money earned or spent over time, measured in €/m².</p>
                                          <p>A <strong>positive cash flow</strong> means the first tomato variety made more money than the second.</p>
                                          <p>A <strong>negative cash flow</strong> means the second variety did better.</p>
                                          <p>The graph shows how much the cash flow might go up or down, and how uncertain those outcomes are.</p>
                                          <p>It uses <em>quantiles</em> to show the range of values around the average result.</p>
                                               '),
                                          downloadButton("save_plot3", "Save Plot"),
                                          br(),
                                          br(),
                                          tableOutput("cashflow_means"),
                                          p("The table shows the average annual cash flow [€/m²], e.g., “Cash Flow 1” for the first year,
                                            “Cash Flow 2” for years 1 to 2, and so on."),
                                          downloadButton("save_table3", "Save table"),
                                ))
                      
                      
               )),
             fluidRow(
               sidebarPanel(width = 12,
                            fluidRow( h3( strong('Other')),
                                      textAreaInput(width = "100%", 
                                                    height ='100px',
                                                    "Verbesserung_1", 
                                                    "Are there any missing variables? What could be improved in this simulation model?")),
                            fluidRow(align = "left",
                                     
                                     #save & download file####
                                     # Single button for saving and downloading
                                     h5( strong('Please download your file.')),      
                                     downloadButton('saveDownload', 'Download'))
               )),
             fluidRow(column(width = 2,
             ),
             column(width = 7,  # Center alignment logic with empty space between logos
                    align = "center",
                    ""
             ),      
             column( width = 3,
                     align = "right",
                     tags$a(tags$img(src = "Logo/fund.png", height = "90px"))
             ))
             
    ),
    #SidePanel closed
    
    
    
    
    
    # #first German tabPanel ####
    # tabPanel("Tomate (DE)", h2("MonteCarlo Simulation mit zwei verschiedenen Tomatensorten"),class= "tabPanel",
    #          fluidRow(
    #            column(width =4,textInput("Datum_1", "Datum")),
    #            column(width =4,textInput("Betrieb_1", "Betrieb (optional)")),
    #            column(width =4,textInput("name_1", "Name (optional)"))
    #          ),
    #          fluidRow(
    #            sidebarPanel(width = 12,
    #                         fluidRow( h3( strong('Was ist Optimal Priming?')),
    #                             h5(HTML("Die interaktive App Optimal Priming ermöglicht es Tomatenlandwirten, 
    #                             die wirtschaftliche Leistungsfähigkeit zweier Tomatensorten unter Unsicherheit zu bewerten.")),
    #                             h5(HTML("Durch eigene Betriebsdaten und 10.000 Monte-Carlo-Simulationen erhalten Nutzer 
    #                             individuelle Ergebnisse für Kapitalwert (NPV) und Cashflow.")),
    #                             h5(HTML("Die App wurde im Rahmen des HortiPrimed-Projekts entwickelt, das vom Bundesministerium für 
    #                             Landwirtschaft und Ernährung (BMEL) gefördert wird.Sie unterstützt sowohl Landwirte bei der Entscheidungsfindung 
    #                             als auch Forschende bei dee Bewertung der finanziellen Tragfähigkeit von Innovationen.")),
    #                           h3( strong('Anwendung des Models')),
    #                                   h4 (HTML("<b>1.</b> Bitte geben Sie Ihre geschätzten und <b>Jährlichen</b> Kennzahlen ein.")),
    #                                   h4 (HTML("<b>2.</b> Klicken Sie auf <b>'Download'</b>, damit Ihre Ergebnisse als CSV-Datei 
    #                                            auf Ihrem Computer oder Laptop heruntergeladen werden.")),
    #                                   h4 (HTML("<b>3.</b> Bitte senden Sie Ihre CSV-Datei an <b>'s62smoon@uni-bonn.de'</b>. 
    #                                            Bei Fragen wenden Sie sich bitte an die genannte E-Mail-Adresse."))
    #                                   
    #                         ))),
    #          
    #          fluidRow(
    #            sidebarPanel(width = 6, 
    #                         
    #                         h3(strong('Sorte A')),
    #                         textInput("1_Sorte_1", 
    #                                   "Welche Tomatensorte ist das?"),
    #                         sliderInput("a_Ertrag",
    #                                     "Ertrag (kg/m2)",
    #                                     min = 0,
    #                                     max = 50,
    #                                     value = c(15, 25),
    #                                     step = 1), 
    #                         sliderInput("a_Tomaten_Preis",
    #                                     "Tomatenpreis (€/kg)",
    #                                     min = 0.1,
    #                                     max = 5,
    #                                     value = c(0.5, 3),
    #                                     step = 0.1),
    #                         sliderInput("a_Saatgut",
    #                                     "Saatgut Preis (€/m²)",
    #                                     min = 0,
    #                                     max = 5,
    #                                     value = c(1.5, 3),
    #                                     step = 0.1),
    #                         sliderInput("a_Jungepflanzen",
    #                                     "Jungpflanzen Preis (€/m²)",
    #                                     min = 2,
    #                                     max = 7,
    #                                     value = c(2.5, 4.5),
    #                                     step = 0.1)
    #            ),
    #            sidebarPanel(width = 6,
    #                         h3(strong('Sorte B')),
    #                         textInput("2_Sorte_1", 
    #                                   "Welche Tomatensorte ist das?"),
    #                         sliderInput("b_Ertrag",
    #                                     "Ertrag (kg/m2)",
    #                                     min = 0,
    #                                     max = 50,
    #                                     value = c(15, 25),
    #                                     step = 1), 
    #                         sliderInput("b_Tomaten_Preis",
    #                                     "Tomatenpreis (€/kg)",
    #                                     min = 0.1,
    #                                     max = 5,
    #                                     value = c(0.5, 3),
    #                                     step = 0.1),
    #                         sliderInput("b_Saatgut",
    #                                     "Saatgut Preis (€/m²)",
    #                                     min = 0,
    #                                     max = 5,
    #                                     value = c(1.5, 3),
    #                                     step = 0.1),
    #                         sliderInput("b_Jungepflanzen",
    #                                     "Jungpflanzen Preis (€/m²)",
    #                                     min = 2,
    #                                     max = 7,
    #                                     value = c(2.5, 4.5),
    #                                     step = 0.1)
    #            )),
    #          fluidRow(
    #            
    #            sidebarPanel(width = 6,
    #                         style = "height: 155vh; overflow-y: auto;", #height in % 
    #                         
    #                         h3(strong('2. Kosten')),
    #                         h4(strong('2.1. Direktkosten')),
    #                         
    #                         sliderInput("Substrat",
    #                                     "Substrat (€/m²): Falls Substrate gedämpft werden, 
    #                                     geben Sie bitte die Substratkosten und Dampfkosten zusammen an.",
    #                                     min = 3,
    #                                     max = 15,
    #                                     value = c(5,8),
    #                                     step = 1),
    #                         textInput("Heizmaterial_1", 
    #                                   "Welche Heizmaterial benutzen Sie für Ihre Tomaten produktion?"),
    #                         sliderInput("Energie_therm",
    #                                     "Thermische Energie (€/m²)",
    #                                     min = 1,
    #                                     max = 20,
    #                                     value = c(4,8.5),
    #                                     step = 0.5),
    #                         sliderInput("Energie_elek",
    #                                     "Elektrische Energie (€/m²)",
    #                                     min = 1,
    #                                     max = 10,
    #                                     value = c(1.5,3.5),
    #                                     step = 0.5),
    #                         sliderInput("CO2_H2O_Due",
    #                                     "CO₂-, Wasser- und Düngemittelfluss(€/m²)",
    #                                     min = 0.1,
    #                                     max = 5,
    #                                     value =  c(1.5,2.1),
    #                                     step=0.1),
    #                         sliderInput("Kordel",
    #                                     "Kordel(€/m²)",
    #                                     min = 0.1,
    #                                     max = 3,
    #                                     value =  c(0.55,0.95),
    #                                     step=0.05),
    #                         sliderInput("Hummel_Nutzlinge",
    #                                     "Hummel und Nutzlingen (€/m²)",
    #                                     min = 0.01,
    #                                     max = 1,
    #                                     value =  c(0.05,0.1),
    #                                     step=0.01),
    #                         sliderInput("PSM_chem",
    #                                     "Chemisches Pflanzenschutzmittel(€/m²)",
    #                                     min = 0.01,
    #                                     max = 2,
    #                                     value =  c(0.25,0.35),
    #                                     step=0.01),
    #                         sliderInput("Heackseln_Entsorgung",
    #                                     "Häckseln und Entsorgung (€/m²)",
    #                                     min = 0.01,
    #                                     max = 2,
    #                                     value =  c(0.3, 0.45 ),
    #                                     step=0.01),
    #                         sliderInput("Desinfektion",
    #                                     "Desinfektion (€/kg)",
    #                                     min = 0.01,
    #                                     max = 1,
    #                                     value =  c(0.01,0.05),
    #                                     step=0.01),
    #                         sliderInput("Versicherung",
    #                                     "Versicherung (€/m²)",
    #                                     min = 0.01,
    #                                     max = 3,
    #                                     value =  c(0.55,0.85),
    #                                     step=0.01),  
    #                         h4( strong('2.2 Arbeitserledigungskosten')),
    #                         sliderInput("Arbeit_all",
    #                                     "Allgemeine Arbeiten: Räumen und Reinigen (€/m²)",
    #                                     min = 1,
    #                                     max = 20,
    #                                     value =  c(2,5),
    #                                     step=0.5),
    #                         
    #                         sliderInput("Arbeit_pf",
    #                                     "Arbeiten für die Pflege: Drehen, Blätter entfernen, Triebe bearbeiten (€/m²)",
    #                                     min = 1,
    #                                     max = 30,
    #                                     value =  c(6,10),
    #                                     step=0.5),
    #                         sliderInput("Arbeit_ernte",
    #                                     "Ernte und Pflücken (€/m²)",
    #                                     min = 1,
    #                                     max = 30,
    #                                     value =  c(10,20),
    #                                     step=0.5),
    #                         h4(strong('sonstige Variable')),
    #                         sliderInput("Kapitaldienst",
    #                                     "Kapitaldienst (€/m²)",
    #                                     min = 1,
    #                                     max = 50,
    #                                     value =  c(10,15),
    #                                     step=1),
    #                         
    #                         sliderInput("n_years_c",
    #                                     "Produktionszeitraum (Jahre)",
    #                                     min = 1,
    #                                     max = 10,
    #                                     value =  5,
    #                                     step=1)
    #            ),
    #            column(width = 6, align = "left",
    #                   mainPanel(width = 11,
    #                             fluidRow( h3( strong('Ergebnisse')),
    #                                       p('Diese Ergebnisse kommen aus 10.000 Simulationen mit Ihren gegebenen Daten.'),
    #                                       br(),
    #                                       h4("1. Verteilung des Kapitalwerts (NPVe)"),
    #                                       plotOutput("distPlot1",height = "350px",
    #                                                  width = "105%"),
    #                                       p ("Die Grafik zeigt die Verteilung des Kapitalwerts (NPV), die in 10.000 Modellsimulationen beobachtet wurde. 
    #                                          Die Kurve stellt die geschätzte Wahrscheinlichkeitsdichte verschiedener Kapitalwerte dar und zeigt 
    #                                          die relative Wahrscheinlichkeit jedes Ergebnisses an. Unsere Entscheidungsanalyse liefert diese 
    #                                          probabilistische Kapitalwertverteilung sowie prognostizierte jährliche Cashflows. Der Kapitalwert selbst 
    #                                          repräsentiert die diskontierte Saldo aus dem Barwert der erwarteten Einzahlungen und Auszahlungen."),
    #                                       downloadButton("save_plot1", "Save Plot"),
    #                                       br(),
    #                                       br(),
    #                                       h4("2. Cashflow"),
    #                                       plotOutput("distPlot3",height = "300px",
    #                                                  width = "105%"),
    #                                       p(),
    #                                       p("Der Cashflow ist eine Reihe von Geldbeträgen in €/m², die über einen bestimmten Zeitraum 
    #                                       entweder negativ oder positiv ausfallen können. Der Cashflow wird als positiv betrachtet, 
    #                                       wenn die erste Sorte einen höheren Umsatz erzielt als die zweite. Ein negativer Cashflow 
    #                                       zeigt das Gegenteil. In der Abbildung wird die Unsicherheit des Cashflows durch Quantile um 
    #                                       den Median herum visualisiert."),
    #                                       downloadButton("save_plot3", "Save Plot"),
    #                                       br(),
    #                                       br(),
    #                                       tableOutput("cashflow_means"),
    #                                       p("Die Tabelle zeigt den durchschnittlichen Cashflow [€/m²] pro Jahr, z. B. „Cashflow 1“ 
    #                                         für das 1. Jahr, „Cashflow 2“ für die Jahre 1 bis 2 usw."),
    #                                       downloadButton("save_table3", "Save table"),
    #                             ))
    #                   
    #                   
    #            )),
    #          fluidRow(
    #            sidebarPanel(width = 12,
    #                         fluidRow( h3( strong('Sonstiges')),
    #                                   textAreaInput(width = "100%", 
    #                                                 height ='100px',
    #                                                 "Verbesserung_1", 
    #                                                 "Gibt es noch fehlende Variable? Was könnte es in diesem Simulationsmodel verbessert werden?")),
    #                         fluidRow(align = "left",
    #                                  
    #  #save & download file####
    #                                  # Single button for saving and downloading
    #                                  h5( strong('Bitte laden Sie Ihre Datei herunter.')),      
    #                                  downloadButton('saveDownload', 'Download'))
    #            )),
    #          fluidRow(column(width = 2,
    #          ),
    #          column(width = 7,  # Center alignment logic with empty space between logos
    #                 align = "center",
    #                 ""
    #          ),      
    #          column( width = 3,
    #                  align = "right",
    #                  tags$a(tags$img(src = "Logo/fund.png", height = "90px"))
    #          ))
    #          
    # ),
    # #SidePanel closed
    # 
    # 
    # 
    
    
    # Tabelle tabPanel ####
    # tabPanel("Tabelle",
    #          h3("Eingegebene Daten"),
    #          class= "tabPanel",
    #          fluidRow(
    #            tableOutput("simple_table")
    #          )
    #          
    # )#finish second tabPanel
    
    
  ))#fluidPage & Tabpanel close



# Define server logic required to draw a histogram
#server ####

server <- function(input, output) {
  library(decisionSupport)
  library(ggplot2)
  
  input_estimates <- reactive({
    
    # variables: input ID
    variables <- names(input)[!grepl("_1$", names(input))]
    
    
    lower_values <- sapply(variables, function(var) {
      value <- input[[var]]
      
      # Apply logic based on the slider ranges
      if (length(value) == 1) {
        as.numeric(value)
      } else if (grepl("_t$", var)) {  # Check if variable name ends with "_t"
        as.numeric(value[1])  / 100  # Special logic for sliders with names ending in "_t"
      } else if (grepl("_1$", var)) {
        NULL  # Default behavior
      } else {
        as.numeric(value[1])   # Default behavior
      }
    })
    
    
    upper_values <- sapply(variables, function(var) {
      value <- input[[var]]
      
      # Apply logic based on the slider ranges
      if (length(value) == 1) {
        as.numeric(value)
      } else if (grepl("_t$", var)) {  # Check if variable name ends with "_t"
        as.numeric(value[2]) / 100  # Special logic for sliders with names ending in "_t"
      } else if (grepl("_1$", var)) {
        NULL   # Default behavior
      }else {
        as.numeric(value[2])  # Default behavior
      }
    })
    
    # distributions    
    distributions <- sapply(variables, function(var) {
      value <- input[[var]]
      
      if (grepl("_t$", var)) {
        "tnorm_0_1"  # Special logic for variables ending with "_t"
      } else if (grepl("_c$", var)) {  # Check if variable name ends with "_t"
        "const"  # Special logic for sliders with names ending in "_t"
      } else if (grepl("_1$", var)) {
        as.character(value)   # Default behavior
      } else {
        "posnorm"  # Default behavior
      }
    })
    
    
    
    
    # Create a data frame from the results
    data.frame(
      variable = variables,
      lower = lower_values, 
      upper = upper_values, 
      distribution = distributions, 
      stringsAsFactors = FALSE)
    
    
  })
  
  # Define the additional_variables data frame
  additional_variables <- data.frame(
    variable = c("discount_rate", "var_CV_Preis", "var_CV_Yield"),
    lower = c(2, 34, 50),
    upper = c(5, 35, 55),
    distribution = c("posnorm", "posnorm", "posnorm"),
    stringsAsFactors = FALSE
  )
  
  results <- reactive({
    combined <- rbind(input_estimates(), additional_variables)
    row.names(combined) <- NULL
    combined
    
  })
  
  
  
  
  Tomato_function <- function(x, varnames){
    
    
    #Leistung
    Yield_1 <- vv(a_Ertrag, var_CV_Yield, n_years_c)
    Yield_2 <- vv(b_Ertrag, var_CV_Yield, n_years_c)
    
    
    TomatenPreis_1 <-vv(a_Tomaten_Preis, var_CV_Preis, n_years_c)
    TomatenPreis_2 <-vv(b_Tomaten_Preis, var_CV_Preis, n_years_c)
    
    # Leistung
    Leistung_1 <- Yield_1+TomatenPreis_1
    Leistung_2 <- Yield_2 + TomatenPreis_2
    
    
    #Ausgabe
    #Direkt Kosten & Arbeitskosten
    Kosten_1 <- a_Saatgut + a_Jungepflanzen + 
      Substrat + Energie_therm + Energie_elek + CO2_H2O_Due + Kordel +
      Hummel_Nutzlinge + PSM_chem + Heackseln_Entsorgung + Desinfektion +
      Versicherung + Arbeit_all + Arbeit_pf + Arbeit_ernte + Kapitaldienst
    
    Kosten_2<- b_Saatgut + b_Jungepflanzen + 
      Substrat + Energie_therm + Energie_elek + CO2_H2O_Due + Kordel +
      Hummel_Nutzlinge + PSM_chem + Heackseln_Entsorgung + Desinfektion +
      Versicherung + Arbeit_all + Arbeit_pf + Arbeit_ernte + Kapitaldienst
    
    
    ## Total_ Gewinn_Kosten ##
    
    
    Result_1 <- Leistung_1 - Kosten_1
    Result_2 <- Leistung_2 - Kosten_2
    
    
    
    #NPV calculate
    
    NPV_1 <- discount(Result_1, discount_rate, calculate_NPV = T)
    NPV_2 <- discount(Result_2, discount_rate, calculate_NPV = T)
    
    Cashflow_1 <- discount(Result_1, discount_rate, calculate_NPV = F)
    Cashflow_2 <- discount(Result_2, discount_rate, calculate_NPV = F)
    
    Cashflow <- Cashflow_1 - Cashflow_2
    
    NPV_1NPV_2 = NPV_1 - NPV_2
    
    return(list(NPV_1 = NPV_1,
                NPV_2 = NPV_2, 
                NPV_1NPV_2 = NPV_1NPV_2,
                Cashflow = Cashflow
                
                
    )) 
  }
  
  tomato_simulation <- reactive({
    
    mcSimulation(
      estimate = as.estimate(results()),  # Pass the modified object
      model_function = Tomato_function,
      numberOfModelRuns = 10000, # Run 10,000 simulations
      functionSyntax = "plainNames"
    )
  })
  
  ### Define reactive expressions for each plot
  plot1 <- reactive({
    decisionSupport::plot_distributions(
      mcSimulation_object = tomato_simulation(), 
      vars = c("NPV_1", 
               "NPV_2"),
      old_names = c("NPV_1", "NPV_2"),
      new_names = c("NPV Variety A", "NPV Variety B"),
      method = 'smooth_simple_overlay', 
      colors = c("tomato","wheat1"),
      base_size = 13, 
      x_axis_name = "NPV Result [€/m²]"
    )
  })  
  
  plot2 <- reactive({
    decisionSupport::plot_distributions(
      mcSimulation_object = tomato_simulation(), 
      vars = "NPV_1NPV_2",
      method = "boxplot_density",
      colors = "mediumseagreen",
      x_axis_name = "Outcome distribution",
      old_names = "NPV_1NPV_2",
      new_names = "NPV Produktion")
  })  
  
  plot3 <- reactive({
    decisionSupport::plot_cashflow(
      mcSimulation_object = tomato_simulation(),
      cashflow_var_name = "Cashflow",
      x_axis_name = "year",
      y_axis_name = "Cashflow [€/m²]",
      color_25_75 = "lightblue3",
      color_5_95 = "aliceblue",
      color_median = "midnightblue",
      base_size = 16
    )
  })
  
  
  
  
  
  ### Render the plots using the reactive expressions
  output$distPlot1 <- renderPlot({
    plot1()
  })
  output$distPlot2 <- renderPlot({
    plot2()
  })
  output$distPlot3 <- renderPlot({
    plot3()
  })
  
  ### Helper function to create download handlers
  createDownloadHandler <- function(plot_reactive, filename_prefix) {
    downloadHandler(
      filename = function() {
        paste(filename_prefix, input$project_name, format( Sys.Date(), "%Y-%m-%d"), ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plot_reactive(), device = "png")
      }
    )
  }
  
  
  ### Create download handlers for each plot
  output$save_plot1 <- createDownloadHandler(plot1, "Plot_comparison_outcome")
  output$save_plot2 <- createDownloadHandler(plot2, "Plot_comparison_outcome")
  output$save_plot3 <- createDownloadHandler(plot3, "Plot_Cashflow")
  

  # Display data frame 
  # Basic table output
  output$simple_table <- renderTable({
    input_estimates_table()
  })
  
  # Calculate the mean for each Cashflow column
  cashflow_means <- reactive({
    data <- tomato_simulation()
    
    # Use `sapply` to compute the mean for each column
    means <- sapply(data$y[, grepl("^Cashflow", names(data$y))], mean)
    
    
    # Convert to a data frame for display
    data.frame("Cashflow" = names(means), Mean = round(means, 2))
  })
  
  # Render the table of means in the UI
  output$cashflow_means <- renderTable({
    cashflow_means()
  })
  
  output$save_table3 <- downloadHandler(            # Create the download file name
    filename = function() {
      paste("Cashflow_table-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      
      
      #Display the output in the table
      output$table <- renderTable(cashflow_means())
      write_xlsx(cashflow_means(), file)                   # put Data() into the download file
    }) 
  
  
  #Create a data save and download option ####
  
  input_estimates_table <- reactive({
    
    # variables: input ID
    variables <- names(input)
    
    
    lower_values <- sapply(variables, function(var) {
      value <- input[[var]]
      
      # Apply logic based on the slider ranges
      if (length(value) == 1) {
        as.numeric(value)
      } else if (grepl("_t$", var)) {  # Check if variable name ends with "_t"
        as.numeric(value[1])  / 100  # Special logic for sliders with names ending in "_t"
      } else if (grepl("_1$", var)) {
        NULL  # Default behavior
      } else {
        as.numeric(value[1])   # Default behavior
      }
    })
    
    
    upper_values <- sapply(variables, function(var) {
      value <- input[[var]]
      
      # Apply logic based on the slider ranges
      if (length(value) == 1) {
        as.numeric(value)
      } else if (grepl("_t$", var)) {  # Check if variable name ends with "_t"
        as.numeric(value[2]) / 100  # Special logic for sliders with names ending in "_t"
      } else if (grepl("_1$", var)) {
        NULL   # Default behavior
      }else {
        as.numeric(value[2])  # Default behavior
      }
    })
    
    # distributions    
    distributions <- sapply(variables, function(var) {
      value <- input[[var]]
      
      if (grepl("_t$", var)) {
        "tnorm_0_1"  # Special logic for variables ending with "_t"
      } else if (grepl("_c$", var)) {  # Check if variable name ends with "_t"
        "const"  # Special logic for sliders with names ending in "_t"
      } else if (grepl("_1$", var)) {
        as.character(value)   # Default behavior
      } else {
        "posnorm"  # Default behavior
      }
    })
    
    
    
    # Create a data frame from the results
    data.frame(
      variable = variables,
      lower = lower_values, 
      upper = upper_values, 
      distribution = distributions, 
      stringsAsFactors = FALSE)
  })
  
  
  # Display data frame 
  # Basic table output
  output$simple_table <- renderTable({
    input_estimates_table()
  })
  
  output$saveDownload <- downloadHandler(            # Create the download file name
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      
      
      #Display the output in the table
      output$table <- renderTable(input_estimates_table())
      write.csv(input_estimates_table(), file)                   # put Data() into the download file
    })                          
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
