
#PIMA INDIAN DIABETES DATASET ANALYSIS


library(shiny)
library(ggplot2)
library(shinydashboard)
df=read.csv("diabetes.csv")
df$Glucose[df$Glucose==0]=mean(df$Glucose)
df$BMI[df$BMI==0]=mean(df$BMI)
df$BloodPressure[df$BloodPressure==0]=mean(df$BloodPressure)
df$SkinThickness[df$SkinThickness==0]=mean(df$SkinThickness)
df$Insulin[df$Insulin==0]=mean(df$Insulin)


calculate_proportions <- function() {
  total_rows <- nrow(df)
  diabetes_counts <- table(df$Outcome)
  
  proportion_no_diabetes <- diabetes_counts["0"] / total_rows
  proportion_diabetes <- diabetes_counts["1"] / total_rows
  return(c(proportion_no_diabetes, proportion_diabetes))
}


univariate_analysis_plot<-function(variable,bin){
 ggplot(df,aes(x=df[,variable],fill=factor(Outcome)))+geom_histogram(position="stack",col="black",bins=bin)+labs(x = variable,fill="Diabetes")+scale_fill_discrete(labels=c("No Diabetes","Diabetes"))+ggtitle(paste("Histogram of",variable,"by Outcome"))+theme_minimal()
}

bivariate_analysis_plot<-function(x_variable,y_variable){
  ggplot(df,aes(x = df[,x_variable], y = df[,y_variable],col=factor(Outcome)))+labs(x = x_variable, y = y_variable,col="Diabetes")+geom_point()+facet_wrap(~Outcome)+scale_color_discrete(labels=c("No Diabetes","Diabetes"))+ggtitle(paste("Scatter Plot of",x_variable,"vs",y_variable,"by Outcome"))+theme_minimal()
                                }
ui <- dashboardPage(
  dashboardHeader(title = "PIMA Indian Diabetes Analysis Dashboard",titleWidth = 700),skin="purple",
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "Home",icon = icon("home")),
      menuItem("Univariate Analysis", tabName = "univariate_analysis",icon = icon("chart-line")),
      menuItem("Bivariate Analysis", tabName = "bivariate_analysis",icon = icon("chart-area"))
    )
  ),
  dashboardBody(tags$head(
    tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }'))),
  
    tabItems(
            tabItem(tabName = "Home",
              fluidRow(
                box(
                  h5(strong("Divyanshi Kumari MDS202322")),
                  h2( strong("Introduction")),
                  h4("Type-2 Diabetes in women is a chronic metabolic condition characterised by elevated blood sugar levels resulting from insulin resistance and insufficient production of insulin from pancreas. The PIMA Indian Population particularly, the PIMA Indians of Arizona has one of the highest reported prevalence rates of type-2 diabetes in the world. The relationship between PIMA Indian Women and diabetes has been a focus of research due to this alarming rates of diabetes within this community."),
                  br(),
                  h4("The PIMA Indian Diabetes dataset is named after the PIMA people, a group of Native Americans living in United States, particularly Arizona.The dataset is focused on studying and understanding risk factors and characteristics associated with the development of type-2 diabetes among PIMA women."),
                  h4("link to the dataset:",a("https://www.kaggle.com/datasets/uciml/pima-indians-diabetes-database")),
                  img(src = "https://wordpresscmsprodstor.blob.core.windows.net/wp-cms/2021/11/44b.webp", style = "max-width: 60%; height: auto;display: block; margin-left: auto; margin-right: auto;"),
                  width = 18
                ),
                box(
                  h4("Statistics"),
                  uiOutput("small_boxes"),  
                  width = 12
                ),
                
              )
      ),
      # Univariate Analysis Tab
      tabItem(tabName = "univariate_analysis",
              fluidRow(
                box(
                  h2( strong("Univariate Analysis")),
                  "This section provides a visual representation of the univariate relationships of features in the dataset with respect to the outcome",
                  selectInput("univariate_variable", "Select Features for Analysis", choices = c("Age","Pregnancies","Glucose","DiabetesPedigreeFunction")),
                  plotOutput("univariate_plot"),sliderInput("bins", "Select Number of Bins:", min = 1, max = 100, value = 10),
                  width=18
                  
                )
              )
      ),
      # Bivariate Analysis Tab
      tabItem(tabName = "bivariate_analysis",
              fluidRow(
                box(
                  h2( strong("Bivariate Analysis")),
                  "This section provides a visual representation of the bivariate relationships between features in the dataset with respect to Outcome",
                  selectInput("x_variable", "Select X Variable", choices = c("Insulin","Glucose","BMI")),
                  selectInput("y_variable", "Select Y Variable", choices = c("Insulin","Glucose","BMI")),
                  plotOutput("bivariate_plot"),
                  width=18
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  
  output$univariate_plot <- renderPlot({
    univariate_analysis_plot(input$univariate_variable,input$bins)
  })
 
  output$bivariate_plot <- renderPlot({
    bivariate_analysis_plot(input$x_variable, input$y_variable)
  })
  
  output$small_boxes <- renderUI({
    median_age <- median(df$Age)
    mean_pregnancies <- mean(df$Pregnancies)
    proportions <- calculate_proportions()
    
    box1 <- valueBox(median_age, "Median Age", icon = icon("users"),color="purple")
    box2 <- valueBox(round(mean_pregnancies,2), "Mean Pregnancies", icon = icon("heartbeat"),color="purple")
    box3 <- valueBox(paste0(round(100 * proportions[1], 2), "%"), "Proportion No Diabetes", icon = icon("thumbs-up"),color="purple")
    box4 <- valueBox(paste0(round(100 * proportions[2], 2), "%"), "Proportion Diabetes", icon = icon("thumbs-down"),color="purple")
    
    fluidRow(box1, box2, box3, box4)
  })
 
}
shinyApp(ui, server)
