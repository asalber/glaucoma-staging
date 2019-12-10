library(shiny)
# Load packages
library(tidyverse)
library(MASS)

# Data loading
data <- read.csv(file="data/data-preprocessed-clusters.csv", header=T, sep=",")
# Recode Stages to "Glaucoma"
data$Diagnose <- recode_factor(data$Stage, .default = "Glaucoma", Healthy = "Healthy")

# CLASSIFICATION MODEL FOR UNDIAGNOSED PATIENTS
# Compute principal components 
pca <- prcomp(data[, c("BMO.G", "BMO.TI", "Rim3.5.G", "Rim3.5.TI")], center = F, scale = F)
# Add principal components coordinates to a data frame  
data.pca <- as.data.frame(pca$x)
# Add cluster to principal components data frame
data.pca$Diagnose <- data$Diagnose

# Plot glaucoma clusters on the two first principal components
clusters.plot <- ggplot(data.pca, aes(x = PC1, y = PC2, col=Diagnose)) + 
    geom_point() +
    ggtitle("Position of the eye on the two first principal components plane")

# Linear discriminant analysis (without healthy eyes)
lda <- lda(Diagnose~., data=data.pca)

# CLASSIFICATION MODEL FOR GLAUCOMA PATIENTS
# Filter eyes with glaucoma
data.glaucoma <- data[data$Stage != "Healthy", ]
# Remove empty levels
data.glaucoma <- droplevels(data.glaucoma)

# Compute principal components 
pca.glaucoma <- prcomp(data.glaucoma[, c("BMO.G", "BMO.TI", "Rim3.5.G", "Rim3.5.TI")], center = F, scale = F)
# Add principal components coordinates to a data frame  
data.glaucoma.pca <- as.data.frame(pca.glaucoma$x)
# Add cluster to principal components data frame
data.glaucoma.pca$Stage <- data.glaucoma$Stage

# Plot glaucoma clusters on the two first principal components
clusters.glaucoma.plot <- ggplot(data.glaucoma.pca, aes(x = PC1, y = PC2, col=Stage)) + 
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
            radioButtons("diagnosed", "Have you been diagnosed with glaucoma?", choices = c("Yes", "No"), inline = T),
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
            plotOutput("clusters.plot"),
            htmlOutput("clusters.prediction"),
            tableOutput("clusters.probabilities")
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
            if (input$diagnosed == "Yes") {
                as.data.frame(predict(pca.glaucoma, dataeye))
            } else {
                as.data.frame(predict(pca, dataeye))
            }
        })
    })
    
    # Plot the new eye on the principal components
    output$clusters.plot <- renderPlot(
        if (input$diagnosed == "Yes") {
            if (input$prediction) {
                clusters.glaucoma.plot + geom_point(data = newdata(), col = 'green', size = 4)
            } else {
                clusters.glaucoma.plot
            }
        } else {
            if (input$prediction) {
                clusters.plot + geom_point(data = newdata(), col = 'green', size = 4)
            } else {
                clusters.plot
            }
        }
    )
    # Compute the predictions
    prediction <- reactive({
        if (input$diagnosed == "Yes") {
            predict(lda.glaucoma, newdata())
        } else {
            predict(lda, newdata())
        }
    })
    # Render the prediction for the new eye
    output$clusters.prediction <- renderUI(
        if (input$prediction) {
            HTML(paste(h3(span("Stage prediction:", prediction()$class, style = "color: red")), h4("Probabilities for glaucoma stages")))
        } else {
            return(NULL)
        }
    )
    # Render the probabilities of every glaucoma stage
    output$clusters.probabilities <- renderTable(
        if (input$prediction) {
            prediction()$posterior
        } else {
            return(NULL)
        }
    )
}

# Create Shiny app ----
shinyApp(ui, server)