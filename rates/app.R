library(shiny)
library(shinyWidgets)
library(ggplot2)

rates_all <- read.table("allrates.csv", header=TRUE, sep=",")

variables <- c('Moisture'="moisture", 'Nitrate'="no3",
  'Ammonium'="nh4", 'Year'="year", 'Crop'="type3", 'Crop Type'="type",
  "pH"="pH_DI", "Gross DNRA"="dnra", "Gross Mineralization"="min", "Gross Nitrification"="nitr",
  "N2O Flux"="n2oavg")

# Define server logic ----
server <- function(input, output) {
  
  dataset <- reactive({
    rates_all[sample(nrow(rates_all), input$sampleSize),]
  })
  
  output$plot <- renderPlot({
    
    p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point() +
      labs(x = names(variables[which(variables == input$x)]), 
           y = names(variables[which(variables == input$y)])) +
      
      theme(axis.text=element_text(size=16),
            axis.title=element_text(size=20, face="bold"),
            legend.text=element_text(size=18),
            legend.title=element_text(size=20)) +
      
      guides(color= guide_legend(title=names(variables[which(variables == input$color)]))) +
      
      theme(panel.background = element_rect(fill="#f5f5f5")) + #, color="green",
      #                                       size=2, linetype="solid"), 
      #       panel.grid.major = element_line(size=2, linetype="solid",
      #                                       color="black"),
      #       panel.grid.minor = element_line(size=2, color="black")) + 
      
      theme(plot.background = element_rect(fill="#f5f5f5")) + #, linetype="solid", color="red")) +
      theme(legend.background=element_rect(fill="#f5f5f5")) 
      

    
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    # if (input$jitter)
    #   p <- p + geom_jitter()
    if (input$smooth)
      p <- p + geom_smooth()
    
    print(p)
    
  }, height=569)
  
}

dataset <- rates_all

ui <- fluidPage(
  
  setBackgroundColor(color="#303030"),
  
  h1(id="big-heading", "Rates Explorer"),
  tags$style(HTML("#big-heading{color: #d3d3d3;}")),

  
  #titlePanel("Rates Explorer"),
  
  sidebarPanel(
  
    width=3,  
 
    
    selectInput('x', 'Horizontal Axis', c('Moisture'="moisture", 'Nitrate'="no3",
                'Ammonium'="nh4", 'Year'="year", 'Crop'="type3", 'Crop Type'="type2",
                "pH"="pH_DI", "Gross DNRA"="dnra", "Gross Mineralization"="min", "Gross Nitrification"="nitr",
                "N2O Flux"="n2oavg"), selected="Moisture"),
    selectInput('y', 'Vertical Axis', c('Moisture'="moisture", 'Nitrate'="no3",
                'Ammonium'="nh4", 'Year'="year", 'Crop'="type3", 'Management Type'="type2",
               "pH"="pH_DI", "Gross DNRA"="dnra", "Gross Mineralization"="min", "Gross Nitrification"="nitr",
                   "N2O Flux"="n2oavg"), selected="dnra"),
    selectInput('color', 'Color', c('Moisture'="moisture", 'Nitrate'="no3",
                                    'Ammonium'="nh4", 'Year'="year", 'Crop'="type3", 'Crop Type'="type2",
                                    "pH"="pH_DI", "Gross DNRA"="dnra", "Gross Mineralization"="min", "Gross Nitrification"="nitr",
                                    "N2O Flux"="n2oavg"), selected="no3"),
    
    # checkboxInput('jitter', 'Jitter'),
    checkboxInput('smooth', 'Smooth'),
    
    selectInput('facet_row', 'Facet Row', c(Select='.', c('Year'="year", 'Crop'="type3", 'Crop Type'="type"))),
    selectInput('facet_col', 'Facet Column', c(Select='.', c('Year'="year", 'Crop'="type3", 'Crop Type'="type"))),
    
    sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dataset),
                value=min(1000, nrow(dataset)), step=50, round=0)
    
  ),
  
  mainPanel(
    plotOutput('plot')
  )
)

shinyApp(ui = ui, server = server)

