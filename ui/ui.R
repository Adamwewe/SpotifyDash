library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Classifying the Popularity of tracks on Spotify"),

    # Description
    p("This interactive app affords the user with the ability to specify the categorical outcome variable 
      and fine tune four classifiers accordingly all while comparing the predictions to the observations.
      The aforementioned features are all available in Spotify colors and allow the user to see if a song's loudness, 
      danceability, and speechiness affect its popularity."),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(

            # selection for x-variable
            selectInput(inputId = "var1Input",
                        label = "Choose x-variable:",
                        choices = c( "Danceability", "Loudness (dB)" = "Loudness..dB.","Liveness",
                                    "Speechiness"),
                        selected = "Danceability"),

            # selection for y-variable
            selectInput(inputId = "var2Input",
                        label = "Choose y-variable:",
                        choices = c( "Danceability", "Loudness (dB)" = "Loudness..dB.","Liveness",
                                     "Speechiness"),
                        selected = "Liveness"),
            
            # selection for classification method
            radioButtons("graph_class", "Classification method:",
                         choices = c("Logistic Regression", "KNN", "Classification Tree", "Random Forest"),
                         selected = "Logistic Regression"),
            
            # slider input for Decision threshold Popularity
              ## The user is responsible for selecting at which score can a song
              ## be considered popular
              ## The min/max slider values were adjusted according to the allowed
              ## splits (R would give an error at a too high or too small split)
            sliderInput("popInput", "Decision Threshold Popularity", value = 60, min = 46, max = 75, step=1),
            
            # slider input for number of k nearest neighbors.
            sliderInput("kInput", "# Nearest Neighbors in KNN", value = 7, min = 2, max = 20),
            
            # slider input for number of trees
            sliderInput("ntreeInput", "# Trees in Random Forest", value = 500, min = 10, max = 10000, step=100),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            # creating two tabs, one for the observation, and one for the prediction/classification
            tabsetPanel(type = "tabs",
                        tabPanel("Observation", plotOutput("obsPlot"), htmlOutput("dataInfo")),
                        tabPanel("Prediction/Classification", plotOutput("predPlot"), verbatimTextOutput("cm"), 
                                 tableOutput("modelres"), htmlOutput("statout"))
                        )
        )
    )
))


