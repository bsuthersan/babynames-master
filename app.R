library(shiny)
library(tidyverse)
library(plotly)
library(shinythemes)

###Read in data and round proportions

mydata <- read_csv("~/Downloads/babynames.csv")
mydata$prop <- round(mydata$prop, 5)

##UI

ui <- fluidPage(theme= shinytheme("united"),
   titlePanel("Baby names"),
   sidebarLayout(
      sidebarPanel(
        radioButtons("gender",label="Choose a gender", choices=c("Male","Female")),
        selectInput("numorprop", choices=c("Number","Proportion"), label="Choose data display (number or proportion)"),
         textInput("textname", label="Type in a name (watch your spelling!)", value="James"),
         textInput("comparisonname1", label="Compare with")
      ),
      mainPanel(
        plotlyOutput("namePlot"),
        h3("Meaning of name"),
        htmlOutput("name1meaning")
      )
   )
)


server <- function(input, output) {
  
##Creating reactive datasets
  
##1. For single name graph
  
mydata1 <- reactive({
  if (input$gender=="Female") {
  mydata %>%
    filter(name==input$textname,
           sex=="F")
  }
  else{
    mydata %>%
      filter(name==input$textname,
             sex=="M")
  }
})

##2. For double name graph

mydata2 <- reactive({
  if (input$gender=="Female") {
  mydata %>%
    filter(name==input$textname | name==input$comparisonname1) %>%
      filter(sex=="F")
  }
  else {
    mydata %>%
      filter(name==input$textname | name==input$comparisonname1) %>%
      filter(sex=="M")
  }
})

##For the name meanings

mydata3 <- reactive({
  mydata %>%
    filter(name==input$textname) %>%
    slice(1, )
})

mydata4 <- reactive({
  mydata %>%
    filter(name==input$comparisonname1) %>%
    slice(1, )
})


##Graphing the plot

   output$namePlot <- renderPlotly({
     if (input$numorprop=="Proportion" & 
         input$comparisonname1=="") {
     a <- ggplot(mydata1(), aes(year, prop, group=1, text=paste("Name:", name, "<br>", "Year:", year, "</br>", "Proportion:", prop))) +
       geom_line(color="steelblue") +
       theme_minimal() +
       xlab("Year") +
       ylab("")
     ggplotly(a, tooltip = c("text"))
     }
     else{
       if (input$numorprop=="Number" & 
           input$comparisonname1=="") {
      b <-  ggplot(mydata1(), aes(year, n, group=1, text=paste("Name:", name, "<br>", "Year:", year, "</br>", "Number:", n))) +
         geom_line(color="steelblue") +
        theme_minimal() +
        xlab("Year") +
        ylab("")
      ggplotly(b, tooltip = c("text"))
       }
       else{
         if (input$comparisonname1!="" & 
             input$numorprop=="Number") {
           c <- ggplot(mydata2(), aes(year, n, group=name, text=paste("Name:", name, "<br>", "Year:", year, "</br>", "Number:", n))) +
             geom_line(aes(color=name)) +
             theme_minimal() +
             scale_color_manual(values=c("steelblue", "#FFC433")) +
             theme(legend.title = element_blank()) +
             xlab("Year") +
             ylab("")
           ggplotly(c, tooltip = c("text"))
         }
         else{
           if (input$comparisonname1!="" & 
               input$numorprop=="Proportion") {
            d <- ggplot(mydata2(), aes(year, prop, group=name, text=paste("Name:", name, "<br>", "Year:", year, "</br>", "Proportion:", prop))) +
               geom_line(aes(color=name)) +
              theme_minimal() +
              xlab("Year") +
              ylab("") +
              scale_color_manual(values=c("steelblue", "#FFC433")) +
              theme(legend.title = element_blank())
            ggplotly(d, tooltip = c("text"))
           }
           else{NULL}
       }
       }
     }
   })
   
##Text output - name meanings   

   output$name1meaning <- renderText({
     if(input$comparisonname1=="") {
     paste0("<b>", mydata3()$name, "</b>", ": ", mydata3()$Meaning) }
     else {
       (paste0("<b>", mydata3()$name, "</b>", ": ", mydata3()$Meaning, "<br>", "<b>", mydata4()$name, "</b>", ": ", mydata4()$Meaning)) }
})
     }

# Run the application 
shinyApp(ui = ui, server = server)

