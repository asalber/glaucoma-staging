library(shiny)
# Load packages
library(tidyverse)
require(MASS)

# Data loading
data <- read.csv(file="data/data-preprocessed-clusters.csv", header=T, sep=",")
# Filter eyes with glaucoma
data.glaucoma <- data[data$Stage != "Healthy", ]
# Remove empty levels
data.glaucoma <- droplevels(data.glaucoma)

# Compute principal components 
result.pca <- prcomp(data.glaucoma[, c("BMO.G", "BMO.TI", "Rim3.5.G", "Rim3.5.TI")], center = F, scale = F)
# Add principal components coordinates to a data frame  
data.glaucoma.pca <- as.data.frame(result.pca$x)
# Add cluster to principal components data frame
data.glaucoma.pca$Stage <- data.glaucoma$Stage

# Plot clusters on the two first principal components
clustersPlot <- ggplot(data.glaucoma.pca, aes(x = PC1, y = PC2, col=Stage)) + 
    geom_point() +
    ggtitle("Position of the eye on the two first principal components plane")

# Linear discriminant analysis (without healthy eyes)
lda.glaucoma <- lda(Stage~., data=data.glaucoma.pca)

# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Glaucoma staging system"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar panel for inputs
        sidebarPanel(
            h3("Eye measures"),
            # Include clarifying text
            helpText("Enter the raw retinal measures."),
            helpText("The age of the patient and the BMO area are required to standardize data."),
            # Input fields for retinal measures
            numericInput('age', 'AGE', 0),
            numericInput('bmoarea', 'BMO area', 0),
            numericInput('bmog', 'BMO.G', 0),
            numericInput('bmoti', 'BMO.TI', 0),
            numericInput('rim35g', 'RIM3.5.G', 0),
            numericInput('rim35ti', 'RIM3.5.TI', 0), 
            
            # Input: actionButton() to defer the rendering of output ----       
            # until the user explicitly clicks the button (rather than
            # doing it immediately when inputs change). This is useful if
            # the computations required to render output are inordinately
            # time-consuming.
            actionButton("prediction", "Predict")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            h3("Eye position on the Glaucoma clusters"),
            plotOutput('clustersPlot'),
            htmlOutput("prediction"),
            tableOutput("probabilities")
        )
    ),
    
    HTML("More information about this Glaucoma Staging Sytem on <a href='http://aprendeconalf.es/glaucoma-staging/'>http://aprendeconalf.es/glaucoma-staging/</a>")
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
    # Create a reactive data frame with the new eye data
    newdata <- reactive({
        input$prediction
        isolate({
            dataeye <- data.frame(Age = input$age, BMO.Area = input$bmoarea, BMO.G = input$bmog, BMO.TI = input$bmoti, Rim3.5.G = input$rim35g, Rim3.5.TI = input$rim35ti)
            # Data standarization. The width of the retinal nerve fiber layer depens on age and BMO area. We apply a standarization regression model given by the OCT company.
            # Load the coefficients of the regression model
            std.coef <- read.csv(file="data/standardization-table.csv", encoding = "UTF-8", header=T, row.names=1, sep=",")
            age.mean <- 52.17
            bmo.area.mean <- 1.781
            for (i in colnames(dataeye)[-1:-2]){
                dataeye[[i]] <- (dataeye[[i]]-std.coef[i,"Average"]-std.coef[i,"Slope.age"]*(dataeye$Age-age.mean)-std.coef[i,"Slope.bmo.area"]*(dataeye$BMO.Area-bmo.area.mean))/std.coef[i,"Stdev"]
            }
            as.data.frame(predict(result.pca, dataeye))
        })
    })
    # Plot the new eye on the principal components
    output$clustersPlot <- renderPlot(
        if (input$prediction) {
            clustersPlot + geom_point(data = newdata(), col = 'red', size = 4)
        } else {
            clustersPlot
        }
    )
    prediction <- reactive({
        predict(lda.glaucoma, newdata())
    })
    # Render the prediction for the new eye
    output$prediction <- renderUI(
        if (input$prediction) {
            HTML(paste(h3("Stage prediction:", prediction()$class), h4("Probabilities for glaucoma stages")))
        } else {
            return(NULL)
        }
    )
    # Render the probabilities of every glaucoma stage
    output$probabilities <- renderTable(
        if (input$prediction) {
            prediction()$posterior
        } else {
            return(NULL)
        }
    )
}

# Create Shiny app ----
shinyApp(ui, server)