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
      menuItem("Dataset", tabName = "dataset", icon = icon("database")),
      menuItem("Univariate Analysis", tabName = "univariate_analysis",icon = icon("chart-line")),
      menuItem("Bivariate Analysis", tabName = "bivariate_analysis",icon = icon("chart-area")),
      menuItem("Conclusion", tabName = "conclusion",icon = icon("list-check"))
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
                  h5(strong("Divyanshi Kumari MDS202322 divyanshi.mds2023@cmi.ac.in")),
                  h2( strong("Introduction")),
                  h4("Type-2 Diabetes in women is a chronic metabolic condition characterised by elevated blood sugar levels resulting from insulin resistance and insufficient production of insulin from pancreas. The PIMA Indian Population particularly, the PIMA Indians of Arizona has one of the highest reported prevalence rates of type-2 diabetes in the world. The relationship between PIMA Indian Women and diabetes has been a focus of research due to this alarming rates of diabetes within this community."),
                  br(),
                  h4("The PIMA Indian Diabetes dataset is named after the PIMA people, a group of Native Americans living in United States, particularly Arizona.The dataset is focused on studying and understanding risk factors and characteristics associated with the development of type-2 diabetes among PIMA women."),
                  h4("link to the dataset:",a("https://www.kaggle.com/datasets/uciml/pima-indians-diabetes-database")),
                  img(src = "https://wordpresscmsprodstor.blob.core.windows.net/wp-cms/2021/11/44b.webp", style = "max-width: 40%; height: 20%;display: block; margin-left: auto; margin-right: auto;"),
                  width = 15
                ),
                box(
                  h4("Statistics"),
                  uiOutput("small_boxes"),  
                  width = 10
                ),
                
              )
      ),
      tabItem(tabName = "dataset",
              fluidRow(
                box(
                  title = "Dataset Preview",
                  h4( strong("Age"), "Age of the individual"),
                  h4(strong("Pregnancies"), "Number of times Pregnant"),
                  h4(strong("Glucose") ,"Plasma glucose concentration a 2 hours in an oral glucose tolerance test"),
                  h4(strong("Blood Pressure"), " Diastolic Blood Pressure (mm Hg)"),
                  h4(strong("Skin Thickness"), " Triceps skin fold thickness (mm)"),
                  h4(strong("Insulin"), " 2-Hour serum insulin (mu U/ml)"),
                  h4(strong("BMI"), "Body Mass Index"),
                  h4(strong("Diabetes Pedigree Function"),"a measure of the diabetes heredity risk "),
                  h4(strong("Outcome"), "0: NO Diabetes, 1: Diabetes"),
                  tableOutput("dataset_preview"),  
                  width = 18
                )
                )
              ),
                
      # Univariate Analysis Tab
      tabItem(tabName = "univariate_analysis",
              fluidRow(
                box(
                  h2( strong("Univariate Analysis")),
                  "This section provides a visual representation of the significant univariate relationships of features in the dataset with respect to the outcome",
                  selectInput("univariate_variable", "Select Features for Analysis", choices = c("Age","Pregnancies","Glucose","DiabetesPedigreeFunction")),
                  plotOutput("univariate_plot"),sliderInput("bins", "Select Number of Bins:", min = 1, max = 100, value = 10),
                  verbatimTextOutput("explanation_uni"),
                  width=18
                  
                )
              )
    
      ),
      # Bivariate Analysis Tab
      tabItem(tabName = "bivariate_analysis",
              fluidRow(
                box(
                  h2( strong("Bivariate Analysis")),
                  "This section provides a visual representation of the significant bivariate relationships between features in the dataset with respect to Outcome",
                  selectInput("y_variable", "Select Y Variable", choices = c("Glucose","Insulin","BMI")),
                  selectInput("x_variable", "Select X Variable", choices = c("Insulin","BMI","Glucose")),
                  plotOutput("bivariate_plot"),
                  verbatimTextOutput("explanation_bi"),
                  width=18
                )
              )
      ),
      #Conclusion
      tabItem(tabName="conclusion",
              fluidRow(
                box(
                  h2(strong("Conclusion")),
                  h4(strong("*")," Multiple Pregnancies may be associated with higher risks of type-2 diabetes in female"),
                  h4(strong("*")," There might be a positive association between increasing ages and higher risks of diabetes"),
                  h4(strong("*"),"Higher diabetes pedigree function may be associated with higher risks of
Diabetes thus unfolding the potential role of genetic traits in development of Diabetes."),
                  h4(strong("*"),"A substantial distinction is evident between median Glucose levels for diabetic and non
diabetic female. It may suggest that elevated glucose levels are associated with the presence of diabetes."),
                  h4(strong("*"),"Increasing BMI and elevated Glucose levels can be associated with higher risks of
Diabetes. "),
                  width=20
                )
              )
      )
              ))
    )

  

server <- function(input, output) {
  output$explanation_uni<-renderText({
    ch=input$univariate_variable
    if(ch=='Age'){
      detail="With increasing Age, we see proportion of diabetic female increasing"
    }
  else if(ch=='Pregnancies'){
    detail="With increasing number of Pregnancies, proportion of diabetic females are increasing"
  }
    else if(ch=='Glucose'){
      detail="Glucose levels for Diabetic group is considerably higher than that of No Diabetes Group"
    }
    else if(ch=='DiabetesPedigreeFunction'){
     detail="Although the data points in higher range is quite relatively small, there is a noteworthy increase in
      the proportion of patients suffering from diabetes within this subset."
    }
    return(detail)
    width=15
  }
  )
  
  output$dataset_preview <- renderTable({
    head(df, 10)  
  })
  
  output$explanation_bi<-renderText({
    a1=input$x_variable
    a2=input$y_variable
    if(a1=='Insulin' & a2=='Glucose'){
      detail="Elevated levels of glucose in the blood, may stimulate the release of insulin to regulate 
      and lower blood glucose levels.In case of diabetic patients, this release of insulin is not enough. 
      The graph shows that  for the same insulin level the glucose levels might be high for diabetic as compared to non-diabetics.
      However, the graph is not enough to  affirm any exact positive/negative relationships."
    }
    else if (a1=='BMI'& a2=='Glucose'){
      detail="There is a positive association between an increase in Body Mass Index (BMI) and higher glucose
levels which may suggest increasing risks of Diabetes"
    }
    else if (a2=='Insulin' & a1=='Glucose'){
      detail="Elevated levels of glucose in the blood, may stimulate the release of insulin to regulate
      and lower blood glucose levels.In case of diabetic patients, this release of insulin is not enough. 
      The graph shows that  for the same insulin level the glucose levels might be high for diabetic as compared to non-diabetics.
      However, the graph is not enough to  affirm any exact positive/negative relationships."
    }
    else if (a2=='BMI'& a1=='Glucose'){
      detail="There is a positive association between an increase in Body Mass Index (BMI) and higher glucose
levels which may suggest increasing risks of Diabetes"
    }
    else if (a1 != a2){ detail="Cannot afirm anything exactly via graphs about their relationship"}
    else{detail="Make a valid choice"}
    return (detail)
    width=15
  }
  )
  
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
    
    box1 <- valueBox(median_age, "Median Age", icon = icon("users"),color="purple",width=3)
    box2 <- valueBox(round(mean_pregnancies,2), "Mean Pregnancies", icon = icon("heartbeat"),color="purple",width=3)
    box3 <- valueBox(paste0(round(100 * proportions[1], 2), "%"), " No Diabetes", icon = icon("thumbs-up"),color="purple",width=3)
    box4 <- valueBox(paste0(round(100 * proportions[2], 2), "%"), " Diabetes", icon = icon("thumbs-down"),color="purple",,width=3)
    
    fluidRow(box1, box2, box3, box4)
  })
 
}
shinyApp(ui, server)
