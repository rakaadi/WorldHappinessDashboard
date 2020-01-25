# Load the library
library(shinydashboard)
library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(DT)
library(rbokeh)

# Load the dataset
Happiness <- read_csv("Happiness_.csv")

as.numeric(unique(Happiness$Year)) -> Years

# The dashboard
  # Define the ui
ui <- dashboardPage(
  dashboardHeader(title = "World Happiness Report",
                  titleWidth = 250),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard", lib = "glyphicon"), startExpanded = FALSE,
        menuSubItem("Reporting Year", tabName = "reportYear"),
          sliderInput("Year",
                      label = NULL,
                      min = min(Years),
                      max = max(Years),
                      value = c(min(Years), max(Years)),
                      sep = ""),
        menuSubItem("Select Country", tabName = "country"),
          selectInput(inputId = "Countries", 
                      label = NULL,
                      choices = str_sort(unique(Happiness$Country))
                      )
      ),
      
      menuItem("Table", tabName = "table", icon = icon("th", lib = "glyphicon")),
      
      menuItem("Learn More", tabName = "more", icon = icon('book', lib = "glyphicon"))
      
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content, placed under sub-tab country
      tabItem(tabName = "country",
              fluidRow(
                box(title = "Happiness Score", status = "primary",
                    collapsible = TRUE, solidHeader = TRUE,
                    rbokehOutput("scorePlot"), width = 6),
                box(title = "Scores", status = "warning",
                    collapsible = TRUE, solidHeader = TRUE,
                    rbokehOutput("otherPlot"), width = 6)
              ),
              
              fluidRow(
                tabBox(width = 12, height = 100,
                       tabPanel("About", "All of this metrics describe the extent to which these factors contribute in evaluating the happiness in each country"),
                       tabPanel("Happiness Rank", "Rank of the country based on the Happiness Score."),
                       tabPanel("Score", 'A metric measured by asking the sampled people the question: "How would you rate your happiness on a scale of 0 to 10 where 10 is the happiest"'),
                       tabPanel("Economy", "The extent to which GDP contributes to the calculation of the Happiness Score"),
                       tabPanel("Social Support", "The extent to which Family contributes to the calculation of the Happiness Score."),
                       tabPanel("Health", "The extent to which Life expectancy contributed to the calculation of the Happiness Score."),
                       tabPanel("Freedom", "The extent to which Freedom to make life choices contributed to the calculation of the Happiness Score."),
                       tabPanel("Generosity", "The extent to which Generosity contributed to the calculation of the Happiness Score."),
                       tabPanel("Perception of Corruption", "The extent to which trust (Government Corruption) contributes to Happiness Score.")
                       )
              ),
              
              fluidRow(
                dataTableOutput("happytable")
              )
      ),
      
      # Second tab content
      tabItem(tabName = "table",
              h2("World Happiness Report 2015-2019"),
              dataTableOutput("datatable")
              ),
      
      # Third tab contents
      tabItem(tabName = "more",
              h2("To learn more about World Happiness Report visit the places below"),
              fluidRow(
                infoBox("The Original Dataset", icon = icon("th-large", lib = "glyphicon"), color = "navy",
                        "Can be accessed here",
                        tags$a(href = "https://www.kaggle.com/unsdsn/world-happiness", 
                               "Datasets", target = "_blank")),
                infoBox("World Happiness Report", icon = icon("bookmark", lib = "glyphicon"), color = "olive",
                        "Visit SDSN to learn more",
                        tags$a(href = "https://resources.unsdsn.org/happiness7753ddc5", 
                               "Report", target = "_blank")),
                infoBox("Github", icon = icon("github"), color = "black",
                        "Link for this dashboard",
                        tags$a(href = "https://github.com/rakaadi/WorldHappinessDashboard",
                               "Dashboard", target = "_blank"))
              ),
              fluidRow(
                box(title = "TODO", status = "danger", width = 4,
                    collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
                    "- Improve data visualization.", br(),
                    "- Visual Update to the dashboard.", br(),
                    "- Update dashboard when new report issued.")
              )
      )
    )
  )
)

  # Define the server logic
server <- function(input, output, session) {
  
  # generate subset data based on input
  happy <- reactive({
    req(input$Countries)
    # reactive dataframe creation
    Happiness %>%
      filter(Country %in% input$Countries) %>%
      select(-c(Country, Region))
  })
  
  output$scorePlot <- renderRbokeh({
    reportYear <- seq(min(input$Year), max(input$Year))
    
    # draw the line plot with the specified year from slider input
    figure(width = 625, title = input$Countries,
           tools = c("pan", "box_zoom", "resize", "reset", "save")) %>% 
      ly_points(reportYear, HappinessScore, data = happy(),
                hover = c(Year, HappinessRank, HappinessScore),
                size = 16) %>%
      x_axis(label = "Year", desired_num_ticks = 5) %>%
      y_axis(label = "Score")
  })
  
  output$otherPlot <- renderRbokeh({
    happy() %>%
      tidyr::pivot_longer(cols = c("Economy(GDPperCapita)",
                                   "SocialSupport",
                                   "Health(LifeExpectancy)",
                                   "Freedom",
                                   "Generosity",
                                   "PerceptionsofCorruption"),
                          names_to = "Category",
                          values_to = "Scores") -> hap
    
    figure(width = 625, legend_location = NULL,
           tools = c("pan", "box_zoom", "resize", "reset", "save")) %>%
      ly_points(Year, Scores, data = hap,
                color = Category, glyph = Category,
                hover = c(Category, Scores)) %>%
      x_axis(desired_num_ticks = 5)
  })
  
  output$happytable <- renderDataTable(
    happy(), 
    options = list(dom = 't'), 
    extensions = 'Responsive',
    colnames = c('Happiness Rank' = 2,
                 'Score' = 3, 
                 'Economy' = 4,
                 'Social Support' = 5,
                 'Health' = 6,
                 'Perception of Corruption' = 9),
    rownames = FALSE
  )
  
  output$datatable <- renderDataTable(
    datatable(Happiness, extensions = c('Scroller','Buttons', 'FixedColumns'),
              options = list(deferRender = TRUE, 
                             scrollY = 400, 
                             scroller = TRUE,
                             dom = 'Bfrtip',
                             buttons = list('print', 
                                            list(extend = 'collection', 
                                                 buttons = c('csv', 'excel', 'pdf'), 
                                                 text = 'Download')
                                            ),
                             scrollX = TRUE, fixedColumns = TRUE
                             )
                )
  )
  
}

shinyApp(ui, server)