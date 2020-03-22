if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")


# create Shiny ui
ui <- navbarPage(theme = shinytheme("cyborg"), collapsible = TRUE,
                 "COVID-19", id="nav",
                 
                 tabPanel("Global",
                          div(class="outer",
                              tags$head(includeCSS("styles.css")),
                              leafletOutput("mymap", width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default",
                                            top = 80, left = 20, width = 250, fixed=TRUE,
                                            draggable = TRUE, height = "auto",
                                            
                                            h3(textOutput("reactive_case_count"), align = "right"),
                                            h4(textOutput("reactive_death_count"), align = "right"),
                                            span(h4(textOutput("reactive_recovered_count"), align = "right"), style="color:#006d2c"),
                                            span(h4(textOutput("reactive_active_count"), align = "right"), style="color:#cc4c02"),
                                            h6(textOutput("clean_date_reactive"), align = "right"),
                                            h6(textOutput("reactive_country_count"), align = "right"),
                                            tags$i(h6("Updated once daily. For more regular updates, refer to: ", tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins COVID-19 dashboard"))),
                                            plotOutput("epi_curve", height="130px", width="100%"),
                                            plotOutput("cumulative_plot", height="130px", width="100%"),
                                            span(h6(textOutput("reactive_case_count_China"), align = "right"), style="color:#cc4c02"),
                                            span(h6(textOutput("reactive_case_count_row"), align = "right"), style="color:#662506"),
                                            # span(("Circles show confirmed cases for Covid, SARS, and Ebola, and estimated deaths for H1N1."),align = "left", style = "font-size:80%"),
                                            
                                            sliderInput("plot_date",
                                                        label = h5("Select mapping date"),
                                                        min = as.Date(cv_min_date,"%Y-%m-%d"),
                                                        max = as.Date(current_date,"%Y-%m-%d"),
                                                        value = as.Date(current_date),
                                                        timeFormat = "%d %b", 
                                                        animate=animationOptions(interval = 2000, loop = FALSE))
                              ),
                              
                              absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                            tags$a(href='https://www.lshtm.ac.uk', tags$img(src='lshtm_dark.png',height='40',width='80'))),
                              
                              absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                            actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                                         onclick = sprintf("window.open('%s')", 
                                                                           "https://twitter.com/intent/tweet?text=%20@LSHTM_Vaccines%20outbreak%20mapper&url=https://bit.ly/2uBvnds&hashtags=coronavirus")))
                              
                              
                          )
                 ),
                 
                 tabPanel("Análisis gráfico",
                          
                          sidebarLayout(
                            sidebarPanel(
                              
                              pickerInput("outcome_select", "Outcome:",   
                                          choices = c("Cases", "Deaths", "Recovered"), 
                                          selected = c("Cases"),
                                          multiple = FALSE),
                              
                              pickerInput("country_select", "Country:",   
                                          choices = as.character(cv_today[order(-cv_today$cases),]$country), 
                                          options = list(`actions-box` = TRUE),
                                          selected = cv_today$country,
                                          multiple = TRUE), 
                              
                              "Para seleccionar la información deseada y el país de interés utilice el menú 
                              desplegable que aparece al posicionarse en el botón correspondiente"
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Barras", plotlyOutput("country_plot")),
                                tabPanel("Acumulado", plotlyOutput("country_plot_cumulative")),
                                tabPanel("Acumulado (log10)", plotlyOutput("country_plot_cumulative_log"))
                              )
                            )
                          )
                 ),
                 
                 tabPanel("Centroamérica"),
                 
                 
                 tabPanel("Costa Rica"),
                 
                 tabPanel("Sobre este sitio",
                          tags$div(
                            tags$h4("Last update"), 
                            h6(paste0(update)),
                            "This site is updated once daily. At this time of rapid escalation of the COVID-19 pandemic, the following resources offer the latest numbers of known cases:",tags$br(),
                            tags$a(href="https://experience.arcgis.com/experience/685d0ace521648f8a5beeeee1b9125cd", "WHO COVID-19 dashboard"),tags$br(),
                            tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins University COVID-19 dashboard"),tags$br(),
                            "The aim of this site is to complement the above resources by providing several interactive features not currently available elsewhere, including the timeline function, 
                            the ability to overlay past outbreaks, and an emphasis on normalised counts (per 100,000 individuals).",tags$br(),
                            tags$br(),tags$h4("Background"), 
                            "In December 2019, cases of severe respiratory illness began to be reported across the city of Wuhan in China. 
                            These were caused by a new type of coronavirus, and the disease is now commonly referred to as COVID-19.
                            The number of COVID-19 cases started to escalate more quickly in mid-January and the virus soon spread beyond China's borders. 
                            This story has been rapidly evolving ever since, and each day we are faced by worrying headlines regarding the current state of the outbreak.",
                            tags$br(),tags$br(),
                            "In isolation, these headlines can be hard to interpret. 
                            How fast is the virus spreading? Are efforts to control the disease working? How does the situation compare with previous epidemics?
                            This site is updated daily based on data published by Johns Hopkins University. 
                            By looking beyond the headlines, we hope it is possible to get a deeper understanding of this unfolding pandemic.",
                            tags$br(),tags$br(),
                            "An article discussing this site was published in ",tags$a(href="https://theconversation.com/coronavirus-outbreak-a-new-mapping-tool-that-lets-you-scroll-through-timeline-131422", "The Conversation. "),
                            "The map was also featured on the BBC World Service program",tags$a(href="https://www.bbc.co.uk/programmes/w3csym33", "Science in Action."),
                            tags$br(),tags$br(),tags$h4("Code"),
                            "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/Joshuasalazar1699/nCoV_tracker", "Github."),
                            tags$br(),tags$br(),tags$h4("Sources"),
                            tags$b("2019-COVID cases: "), tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", "Johns Hopkins Center for Systems Science and Engineering github page,")," with additional information from the ",tags$a(href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports", "WHO's COVID-19 situation reports."),
                            " In previous versions of this site (up to 17th March 2020), updates were based solely on the WHO's situation reports.",tags$br(),
                            tags$b("2003-SARS cases: "), tags$a(href="https://www.who.int/csr/sars/country/en/", "WHO situation reports"),tags$br(),
                            tags$b("2009-H1N1 confirmed deaths: "), tags$a(href="https://www.who.int/csr/disease/swineflu/updates/en/", "WHO situation reports"),tags$br(),
                            tags$b("2009-H1N1 projected deaths: "), "Model estimates from ", tags$a(href="https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1001558", "GLaMOR Project"),tags$br(),
                            tags$b("2009-H1N1 cases: "), tags$a(href="https://www.cdc.gov/flu/pandemic-resources/2009-h1n1-pandemic.html", "CDC"),tags$br(),
                            tags$b("2009-H1N1 case fatality rate: "), "a systematic review by ", tags$a(href="https://www.ncbi.nlm.nih.gov/pubmed/24045719", "Wong et al (2009)"), "identified 
                            substantial variation in case fatality rate estimates for the H1N1 pandemic. However, most were in the range of 10 to 100 per 100,000 symptomatic cases (0.01 to 0.1%).
                            The upper limit of this range is used for illustrative purposes in the Outbreak comarisons tab.",tags$br(),
                            tags$b("2014-Ebola cases: "), tags$a(href="https://www.cdc.gov/flu/pandemic-resources/2009-h1n1-pandemic.html", "CDC"),tags$br(),
                            tags$b("Country mapping coordinates: "), tags$a(href="https://gist.github.com/tadast/8827699", "Github"),tags$br(),
                            tags$br(),tags$br(),tags$h4("Authors"),
                            "Dr Edward Parker, The Vaccine Centre, London School of Hygiene & Tropical Medicine",tags$br(),
                            "Quentin Leclerc, Department of Infectious Disease Epidemiology, London School of Hygiene & Tropical Medicine",tags$br(),
                            tags$br(),tags$br(),tags$h4("Contact"),
                            "edward.parker@lshtm.ac.uk",tags$br(),tags$br(),
                            tags$img(src = "vac_dark.png", width = "150px", height = "75px"), tags$img(src = "lshtm_dark.png", width = "150px", height = "75px")
                          )
                 ),
                 
                 tabPanel("Data",
                          numericInput("maxrows", "Rows to show", 25),
                          verbatimTextOutput("rawtable"),
                          downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
                          "Adapted from timeline data published by ", tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", 
                                                                             "Johns Hopkins Center for Systems Science and Engineering.")
                 )
)

server = function(input, output) {

# covid tab 
output$clean_date_reactive <- renderText({
  format(as.POSIXct(input$plot_date),"%d %B %Y")
})

reactive_db = reactive({
  cv_cases %>% filter(date == input$plot_date)
})

reactive_db_last24h = reactive({
  cv_cases %>% filter(date == input$plot_date & new_cases>0)
})

reactive_db_large = reactive({
  large_countries = reactive_db() %>% filter(alpha3 %in% worldcountry$id)
  large_countries = large_countries[order(large_countries$alpha3),]
  large_countries
})

reactive_db_large_last24h = reactive({
  large_countries = reactive_db_last24h() %>% filter(alpha3 %in% worldcountry$id)
  large_countries = large_countries[order(large_countries$alpha3),]
  large_countries
})

reactive_polygons = reactive({
  worldcountry[worldcountry$id %in% reactive_db_large()$alpha3, ]
})

reactive_polygons_last24h = reactive({
  worldcountry[worldcountry$id %in% reactive_db_large_last24h()$alpha3, ]
})

output$reactive_case_count <- renderText({
  paste0(formatC(sum(reactive_db()$cases), big.mark=","), " cases")
})

output$reactive_death_count <- renderText({
  paste0(formatC(sum(reactive_db()$death), big.mark=","), " deaths")
})

output$reactive_recovered_count <- renderText({
  paste0(formatC(sum(reactive_db()$recovered), big.mark=","), " recovered")
})

output$reactive_active_count <- renderText({
  paste0(formatC(sum(reactive_db()$active_cases), big.mark=","), " active cases")
})

output$reactive_case_count_China <- renderText({
  paste0("Mainland China: ", formatC(sum(subset(reactive_db(), country=="Mainland China")$cases), big.mark=",")," (",
         formatC((cv_aggregated %>% filter(date == input$plot_date & region=="Mainland China"))$new, big.mark=",")," new)")
})

output$reactive_case_count_row <- renderText({
  paste0("Other: ", formatC(sum(subset(reactive_db(), country!="Mainland China")$cases), big.mark=",")," (",
         formatC((cv_aggregated %>% filter(date == input$plot_date & region=="Other"))$new, big.mark=",")," new)")
})

output$reactive_country_count <- renderText({
  paste0(nrow(subset(reactive_db(), country!="Diamond Princess Cruise Ship")), " countries/regions affected")
})

output$reactive_new_cases_24h <- renderText({
  paste0((cv_aggregated %>% filter(date == input$plot_date & region=="Global"))$new, " new in last 24h")
})

output$mymap <- renderLeaflet({ 
  basemap
})

observeEvent(input$plot_date, {
  leafletProxy("mymap") %>% 
    clearShapes() %>%
    addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive_db_large()$activeper100k)) %>% #group = "2019-COVID (cumulative)",
    #label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed COVID cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", reactive_db_large()$country, reactive_db_large()$cases, reactive_db_large()$deaths, reactive_db_large()$recovered, reactive_db_large()$per100k) %>% lapply(htmltools::HTML),
    #labelOptions = labelOptions(
    #             style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
    #             textsize = "15px", direction = "auto")) %>%
    
    #addLegend(pal = cv_pal(reactive_db_large()$per100k), values = ~reactive_db_large()$per100k, position = "topright") %>%
    
    addCircles(data = reactive_db_last24h(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/4)*2.5e4*penalty, 
               fillOpacity = 0.1, color = covid_col, group = "2019-COVID (new)",
               label = sprintf("<strong>%s (cases in past 24h)</strong><br/>Confirmed COVID cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", reactive_db_last24h()$country, reactive_db_last24h()$new_cases, reactive_db_last24h()$new_deaths, reactive_db_last24h()$new_recovered, reactive_db_last24h()$newper100k) %>% lapply(htmltools::HTML),
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                 textsize = "15px", direction = "auto")) %>%
    
    addCircles(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4)*2.5e4*penalty, 
               fillOpacity = 0.1, color = covid_col, group = "2019-COVID (cumulative)",
               label = sprintf("<strong>%s (cumulative cases)</strong><br/>Confirmed COVID cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", reactive_db()$country, reactive_db()$cases, reactive_db()$deaths,reactive_db()$recovered, reactive_db()$per100k) %>% lapply(htmltools::HTML),
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                 textsize = "15px", direction = "auto")) %>%
    
    addCircles(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(active_cases)^(1/4)*2.5e4*penalty, 
               fillOpacity = 0.1, color = covid_col, group = "2019-COVID (active)",
               label = sprintf("<strong>%s (active cases)</strong><br/>Confirmed COVID cases: %g<br/>Cases per 100,000: %g<br/><i><small>Excludes individuals known to have<br/>recovered or died.</small></i>", reactive_db()$country, reactive_db()$active_cases, reactive_db()$activeper100k) %>% lapply(htmltools::HTML),
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                 textsize = "15px", direction = "auto"))
    
  
    
  
})

output$cumulative_plot <- renderPlot({
  cumulative_plot(cv_aggregated, input$plot_date)
})

output$epi_curve <- renderPlot({
  new_cases_plot(cv_aggregated, input$plot_date)
})



# create dataframe with selected countries
country_reactive_db = reactive({
  if (input$outcome_select=="Cases") { 
    cv_cases$outcome = cv_cases$cases
    cv_cases$new_outcome = cv_cases$new_cases
  }
  if (input$outcome_select=="Deaths") { 
    cv_cases$outcome = cv_cases$deaths 
    cv_cases$new_outcome = cv_cases$new_deaths 
  }
  if (input$outcome_select=="Recovered") { 
    cv_cases$outcome = cv_cases$recovered 
    cv_cases$new_outcome = cv_cases$new_recovered
  }
  cv_cases %>% filter(country %in% input$country_select)
})

# country-specific plots
output$country_plot <- renderPlotly({
  country_cases_plot(country_reactive_db())
})

# country-specific plots
output$country_plot_cumulative <- renderPlotly({
  country_cases_cumulative(country_reactive_db())
})

# country-specific plots
output$country_plot_cumulative_log <- renderPlotly({
  country_cases_cumulative_log(country_reactive_db())
})

# output to download data
output$downloadCsv <- downloadHandler(
  filename = function() {
    paste("COVID_data_", cv_today$date[1], ".csv", sep="")
  },
  content = function(file) {
    write.csv(cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                    recovered, new_recovered, active_cases, 
                                    per100k, newper100k, activeper100k)), file)
  }
)

output$rawtable <- renderPrint({
  orig <- options(width = 1000)
  print(tail(cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                   recovered, new_recovered, active_cases, 
                                   per100k, newper100k, activeper100k)), input$maxrows), row.names = FALSE)
  options(orig)
})

}


shinyApp(ui, server)
