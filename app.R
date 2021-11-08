library(shiny)
library(tidyverse)
library(plotly)
library(shinythemes)
library(ukbabynames)

##Read in the babynames data from the ukbabynames package

mydata <-ukbabynames %>%
  select(year, name,n, sex) %>%
  group_by(year, sex) %>%
  add_count(name = 'Total') %>%
  ungroup() %>%
  group_by(name, year, sex) %>%
  summarise(n = sum(n),
            Total = sum(Total)) %>%
  ungroup() %>%
  mutate(prop = round(n/Total,3)) %>%
  filter(between(year, 2000, 2019)) -> mydata



###Read in data from uk baby names package

namemeanings <- read_csv("https://raw.githubusercontent.com/bsuthersan/babynames/master/namemeanings.csv")

#Process and merge

namemeanings <- namemeanings[ ,1:2]
colnames(namemeanings) <- c("Name", "Meaning")

namemeanings <- namemeanings %>%
  mutate(Name = str_to_title(Name))

mydata <- mydata %>%
  left_join(namemeanings, by = c("name"="Name"))

#Create a new variable, proportions, by year

  
  


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

