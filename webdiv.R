

library(shiny)
library(randomForest)
library(caret)

rf_model <- readRDS("C:/Users/kheey/OneDrive/Documents/rf_model.rds")
class(rf_model)

#title
ui <- fluidPage(
  titlePanel("Divorce Prediction App (FOR PRACTICE PURPOSES / 0-No ,1-Yes)"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("age_at_marriage", "Age of Marriage:", "",value=18,min=18,max=100),
      numericInput("marriage_duration_years", "Years of Marriage:", "",min=0,max=100),
      textInput("num_children", "Number of Children:", ""),
      
      selectInput("education_level", "Education Level:",
                  choices = c("No Formal Education","High School", "Bachelor", "Master", "PhD")),
      selectInput("employment_status", "Employment Status:",
                  choices = c("Full-Time","Homemaker", "Part-Time", "Unemployed")),
      
      textInput("combined_income", "Combined Income in (k):", ""),
      selectInput("religious_compatibility", "Religion Status:",
                  choices = c("Not Religious","Different Religion", "Same Religion")),
      
      selectInput("cultural_background_match", "Same Cultural Background (0/1):",choices = c("0","1")),
      selectInput("communication_score", "Communication Score (0–10):", "",choices = c("0","1","2","3","4","5","6","7","8","9","10")),
      textInput("conflict_frequency", "Conflict Frequency per Month:", ""),
      
      selectInput("conflict_resolution_style", "Conflict Resolution Style:",
                  choices = c("Aggressive","Avoidant","Collaborative","Passive")),
      
      selectInput("financial_stress_level", "Financial Stress Level (0–10):", "",choices = c("0","1","2","3","4","5","6","7","8","9","10")),
      selectInput("mental_health_issues", "Presence of Mental Health Issues (0/1):", choices = c("0","1")),
      selectInput("infidelity_occurred", "Presence of Infidelity (0/1):", choices = c("0","1")),
      selectInput("counseling_attended", "Couples Counseling (0/1):", choices = c("0","1")),
      selectInput("social_support", "Social Support Score (0–10):", "",choices = c("0","1","2","3","4","5","6","7","8","9","10")),
      textInput("shared_hobbies_count", "No of Shared Hobbies:", ""),
      selectInput("marriage_type", "Marriage Type:",choices = c("Love","Arranged","Others")),
      
      selectInput("pre_marital_cohabitation", "Pre-Marital Cohabitation (0/1):",choices = c("0","1")),
      selectInput("domestic_violence_history", "Presence of Domestic Violence (0/1):",choices = c("0","1")),
      selectInput("trust_score", "Trust Score (0–10):", "",choices = c("0","1","2","3","4","5","6","7","8","9","10")),
      
      actionButton("predictBtn", "Predict Divorce")
    ),
    
    mainPanel(
      textOutput("prediction")
    )
  )
)
#logic
server <- function(input, output) {
  observeEvent(input$predictBtn, {
    output$prediction <- renderText({
      age_at_marriage<-as.integer(input$age_at_marriage)
      marriage_duration_years<-as.integer(input$marriage_duration_years)
      validate(need(marriage_duration_years < age_at_marriage,"Invalid Scenario: marriage duration cannot exceed age at marriage."))
      # Collect inputs into a data frame
      newdata <- data.frame(
        age_at_marriage = age_at_marriage,
        marriage_duration_years = marriage_duration_years,
        num_children = as.integer(input$num_children),
        education_level = as.character(input$education_level, levels = levels(train$education_level)),
        employment_status = as.character(input$employment_status, levels = levels(train$employment_status)),
        combined_income = as.integer(input$combined_income),
        religious_compatibility = as.character(input$religious_compatibility, levels = levels(train$religious_compatibility)),
        cultural_background_match = as.integer(input$cultural_background_match),
        communication_score = as.integer(input$communication_score),
        conflict_frequency = as.integer(input$conflict_frequency),
        conflict_resolution_style = as.character(input$conflict_resolution_style, levels = levels(train$conflict_resolution_style)),
        financial_stress_level = as.integer(input$financial_stress_level),
        mental_health_issues = as.integer(input$mental_health_issues),
        infidelity_occurred = as.integer(input$infidelity_occurred),
        counseling_attended = as.integer(input$counseling_attended),
        social_support = as.integer(input$social_support),
        shared_hobbies_count=as.integer(input$shared_hobbies_count),
        marriage_type = as.character(input$marriage_type, levels = levels(train$marriage_type)),
        pre_marital_cohabitation = as.integer(input$pre_marital_cohabitation),
        domestic_violence_history = as.integer(input$domestic_violence_history),
        trust_score = as.integer(input$trust_score),
        stringsAsFactors= FALSE
      )
      
      # Predict outcome (0/1) using Random Forest
      print(input$marriage_duration)
      pred <- predict(rf_model, newdata, type = "class")
      if(pred == 1) {
          "Prediction: Divorce Likely"
      } else {
          "Prediction: Divorce Unlikely"
      }
    })
  })
}

shinyApp(ui = ui, server = server)

