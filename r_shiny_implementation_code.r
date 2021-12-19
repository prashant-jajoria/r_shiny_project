# importing packages for plots
library(ggplot2)
library(shiny)
library(leaflet)

# package used to set current working directory
library(rstudioapi)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

# Set the working directory
#setwd("D:\\Data\\Monash - Master of Data Science\\Semester 1 - March 2020\\FIT5147-Data Exploration and Visualization\\Programming Exercise 2 - R")

# Read the data formated csv file : 'assignment-02-data-formated.csv' and store in variable named 'data'
data <- read.csv("dataset.csv")

# Remove % from 'value' column
data$value <- substr(data$value,1,4)

# Convert 'value' column to numeric
data$value <- as.numeric(data$value)

# Plotting the Data using ggplot. Used visual elements and faceting for better understanding of the plot
plot <- ggplot(data, aes(year, value)) + geom_point(aes(shape = coralType, color = location)) + facet_grid(coralType~location)

# Adding line smoothing to the plot. Method = 'lm' specifies it fits a Linear model
finalPlot <- plot + geom_smooth(method="lm")

# Used to return color for different sites
getColor <- function(data) {
  sapply(data$location, function(location) {
    if(location == "site01") {
      "black"
    } else if(location == "site02") {
      "red"
    } 
    else if(location == "site03") {
      "blue"
    }
    else if(location == "site04") {
      "green"
    }
    else if(location == "site05") {
      "white"
    }
    else if(location == "site06") {
      "pink"
    }
    else if(location == "site07") {
      "orange"
    }
    else if(location == "site08") {
      "grey"
    }
  })
}

# Stores the list of icons for different sites
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'white',
  library = 'ion',
  markerColor = getColor(data)
)



ui <- fluidPage(
  
  # Application title
  headerPanel("Effect of Bleaching on Corals in Great Barrier Reef"),
  
  sidebarLayout(
    sidebarPanel(
      # adding radio button for 'Coral Type'
      radioButtons("coralType", "Coral Type", 
                    c("Soft Coral" = "Soft Coral",
                      "Hard Coral" = "Hard Coral",
                      "Sea Fans" = "Sea Fans",
                      "Sea Pens" = "Sea Pens")),
      # adding radio button for 'Smoother Type'
      radioButtons("smootherType", "Smoother Type", 
                   c("lm" = "lm",
                     "glm" = "glm",
                     "gam" = "gam",
                     "loess" = "loess"))
    ),
    mainPanel(
      h3(textOutput("caption")),
      plotOutput("finalPlot"),
      leafletOutput("mymap")
     
    )
  )
)
server <- function(input, output, session) {

  output$caption <- reactiveText(function() {
    paste("Coral Type : ", input$coralType)
  })
  
  output$mymap <- renderLeaflet({ # create leaflet map
    leaflet(data=data) %>% 
      addTiles() %>%
        addAwesomeMarkers(~longitude, ~latitude,icon=icons, label=~as.character(location)) 
  })
  
  output$finalPlot <- reactivePlot(function() {
    
    # check for the coralType
    if (input$coralType == "Soft Coral") {
      coralData <- subset(data, coralType == "soft corals", select = c(value,year,coralType,location))
    }
    else if(input$coralType == "Hard Coral"){
      coralData <- subset(data, coralType == "hard corals", select = c(value,year,coralType,location))
    }
    else if(input$coralType == "Sea Fans"){
      coralData <- subset(data, coralType == "sea fans", select = c(value,year,coralType,location))
    }
    else if(input$coralType == "Sea Pens"){
      coralData <- subset(data, coralType == "sea pens", select = c(value,year,coralType,location))
    }
    
    # Adding Visual Indicator i.e same color for different sites on Map and static plot. 
    p <- ggplot(coralData, aes(value, year)) + geom_point(aes(shape = coralType, color = location)) + scale_color_manual(values = c("site01" = "black", "site02" = "red", "site03" = "blue", "site04" = "green", "site05" = "white", "site06" = "pink", "site07" = "orange", "site08" = "grey")) + facet_wrap(~location,nrow = 3) + geom_smooth(method=input$smootherType)
    
    print(p)
  })
  
  
}

shinyApp(ui, server)
