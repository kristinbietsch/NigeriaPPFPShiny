library(rsconnect)
library(shiny)
library(dplyr)
#library(DT)
library(ggplot2)
library(tidyr)
library(stringr)


# Read in data
data <- read.csv("data/StateLevelPPFP.csv")
data.long <- data %>% gather(Month, Value, Month1:Month12)
data.long$Month <- as.numeric(str_remove_all(data.long$Month, "[Month]"))
data.long$Value <-  data.long$Value*100





# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel(h1("Post Partum Family Planning by Month in Nigeria")),
  
      sidebarLayout(
        sidebarPanel(img(src = "logo_150_trans1.png"),
                     h3("Choose States") ,
                     checkboxGroupInput("State",label = "State", 
                                        choices = list("Sokoto"="Sokoto",
                                                       "Zamfara"="Zamfara",
                                                       "Katsina"="Katsina",
                                                       "Jigawa"="Jigawa",
                                                       "Kano"="Kano",
                                                       "Kaduna"="Kaduna",
                                                       "Kebbi"="Kebbi",
                                                       "Oyo"="Oyo",
                                                       "Osun"="Osun",
                                                       "Ekiti"="Ekiti",
                                                       "Ondo"="Ondo",
                                                       "Lagos"="Lagos",
                                                       "Ogun"="Ogun")),
                     h3("Choose Years") ,
                     checkboxGroupInput("Year",label = "Year", 
                                        choices = list("2013"="2013",
                                                       "2018"="2018"))
                     
                     ),
        mainPanel(plotOutput("plot1")
        )
        
      
      
    )
)


# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  filterData = reactiveVal(data.long %>% mutate(key = 1:nrow(data.long)))

  filtered_df <- reactive({
    
    res <- filterData()  %>% filter(State %in% input$State | is.null(input$State))
    res <- res  %>% filter(Year %in% input$Year | is.null(input$Year))
    
    res
    
  })
  
  output$plot1<-renderPlot({
    
    ggplot(filtered_df(), aes(x=Month, y=Value, color=State, linetype=as.factor(Year)))+
      geom_line() +
      facet_grid(cols = vars(Delivery)) +
      scale_linetype_manual(values=c("2018"="solid", "2013"="longdash"))+
      labs(title="Monthly PPFP in Nigeria", x="Months since Birth", y="Percent", color="State", linetype="Year")+
      theme_bw()+
      theme(text=element_text(size=20))
    
  }, height = 600,width = 1000)
  
  
}


shinyApp(ui = ui, server = server)