library(shiny)
library(randomForest)
library(caret)

# Load trained model
rf_modelheart <- readRDS("C:/Users/kheey/OneDrive/Documents/rf_model_heart.rds")

ui <- fluidPage(
  titlePanel("Heart Disease Prediction App for 20 and above"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("Age.Range", "Age Range:",
                  choices = c("Young Adulthood (20-39)", 
                              "Mid Adulthood (40-64)", 
                              "Senior Citizen (65+)")),
      selectInput("Gender", "Gender:", choices = c("Male","Female")),
      
      numericInput("Cholesterol", "Cholesterol Value:", value = 0),
      numericInput("Blood.Pressure", "Blood Pressure:", value = 0),
      numericInput("Heart.Rate", "Heart Rate:", value = 0),
      
      selectInput("Smoking", "Type of Smoker:", choices = c("Never","Former","Current")),
      selectInput("Alcohol.Intake", "Alcohol Intake:", choices = c("None","Moderate","Heavy")),
      numericInput("Exercise.Hours", "How many hours do you exercise a week:", 
                   min = 0, max = 169, value = 0),
      selectInput("Family.History", "Family history of heart disease:", choices = c("No","Yes")),
      selectInput("Diabetes", "Do you have diabetes:", choices = c("No","Yes")),
      selectInput("Obesity", "Are you obese:", choices = c("No","Yes")),
      selectInput("Stress.Level", "Stress level (0–10):", choices = as.character(0:10)),
      numericInput("Blood.Sugar", "Blood sugar level:", value = 0),
      selectInput("Exercise.Induced.Angina", "Chest pain when exercising:", choices = c("No","Yes")),
      conditionalPanel(
        condition = "input['Exercise.Induced.Angina']  == 'Yes'",
        selectInput("Chest.Pain.Type", "Type of chest pain:", 
                    choices = c("Asymptomatic","Atypical Angina","Non-anginal Pain","Typical Angina"))
      ),
      actionButton("predictBtn", "Predict Heart Disease")
    ),
    mainPanel(
      textOutput("prediction")
    )
  )
)
server <- function(input, output) {
  observeEvent(input$predictBtn, {
    # Collect inputs into a data frame
    newdata <- data.frame(
      Age.Range = input$Age.Range,
      Gender = input$Gender,
      Cholesterol = input$Cholesterol,
      Blood.Pressure = input$Blood.Pressure,
      Heart.Rate = input$Heart.Rate,
      Smoking = input$Smoking,
      Alcohol.Intake = input$Alcohol.Intake,
      Exercise.Hours = input$Exercise.Hours,
      Family.History = input$Family.History,
      Diabetes = input$Diabetes,
      Obesity = input$Obesity,
      Stress.Level = input$Stress.Level,
      Blood.Sugar = input$Blood.Sugar,
      Exercise.Induced.Angina = input$Exercise.Induced.Angina,
      Chest.Pain.Type = input$Chest.Pain.Type,
      stringsAsFactors = FALSE
    )
    # Predict outcome (0/1) using Random Forest
    pred <- predict(rf_modelheart, newdata, type = "class")
    
    output$prediction <- renderText({
      if(pred == 1) {
        "Prediction: Has Heart Disease"
      } else {
        "Prediction: Does Not Have Heart Disease"
      }
    })
  })
}
shinyApp(ui = ui, server = server)
               
               