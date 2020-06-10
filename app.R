#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(httr)
library(tidyverse)
library(jsonlite)
library(ggplot2)
library(DT)
library(shiny)
library(shinyjs)
r = GET("https://api.hearthstonejson.com/v1/48705/enUS/cards.json")

stop_for_status(r)
json <- content(r, as = "text", encoding = "UTF-8")
cards <- fromJSON(json, flatten = TRUE)
url =  str_glue("https://art.hearthstonejson.com/v1/render/latest/enUS/256x/{nu}.png" ,
                nu = cards$id)
cards = cards %>% mutate(img = url)
deck = cards %>% select(id,
                        attack,
                        cardClass,
                        type,
                        cost,
                        health,
                        name,
                        rarity,
                        img,
                        artist) %>% filter(type == "MINION") %>% select(-type) 

var = names(deck)
var = var[!var %in% c("attack", "cost", "health", "img","name","id")]
sr = unique(cards$rarity)
sr = sr[!is.na(sr)]
sw = unique(cards$cardClass)
sw = sw[!is.na(sw)]
sa = unique(cards$artist)
sa = sa[!is.na(sa)]

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Heartstone Data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      shinyjs::useShinyjs(),
      id = "ask",
      radioButtons(
        inputId = "vars",
        label = "Job",
        choices = c("Data manipulation", "Data Visualization")
      ),
      
      checkboxGroupInput(
        inputId = "choices",
        label = "Variables",
        choices = var
      ),
      
      conditionalPanel(
        "input.choices.includes('rarity')",
        selectInput(
          inputId = "rarity",
          label = "Rareness",
          choices = toupper(c("-", sr)),
          selected = "-"
        )
      ),
      
      conditionalPanel(
        "input.choices.includes('cardClass')",
        selectInput(
          inputId = "class",
          label = "Card Class",
          choices = c("-", sw),
          selected = "-"
        )
      ),
      
      conditionalPanel(
        "input.choices.includes('artist')",
        selectInput(
          inputId = "artist",
          label = "Artist",
          choices = c("-", sa),
          selected = "-"
        )
      ),
      
      
      actionButton(inputId = "go", label = "Go"),
      actionButton(inputId = "hid", label = "Cover"),
      actionButton("reset", "reset")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(hidden(
      div(
        id = "answer1",
        
        dataTableOutput("tableone"),
        dataTableOutput("tabletwo"),
        dataTableOutput("tablethree"),
        imageOutput("img1"),
        imageOutput("img2"),
        imageOutput("img3"),
        imageOutput("img4"),
        imageOutput("img5"),
        imageOutput("img6"),
        imageOutput("img7"),
        imageOutput("img8"),
        imageOutput("img9"),
        imageOutput("img10")
      )
    ),
    
    hidden(
      div(
        id = "answer2",
        verbatimTextOutput("summary"),
        plotOutput("plot1"),
        plotOutput("plot2"),
        plotOutput("plot3"),
        
      )
    ))
  ))

# Define server logic required to draw a histogram

server <- function(input, output) {
  observeEvent(input$go,
               {
                 if (input$vars == 'Data manipulation')
                 {
                   if (input$class != "-" | input$artist != "-" | input$rarity != "-")
                   {
                     stats = reactive({
                       if (input$class != '-')
                       {
                         health = deck %>% group_by_at(input$choices) %>% filter(cardClass == input$class) %>%  arrange(desc(health)) %>% head(10)
                         attack = deck %>% group_by_at(input$choices) %>% filter(cardClass ==
                                                                                   input$class) %>%  arrange(desc(attack)) %>% head(10)
                         cost = deck %>% group_by_at(input$choices) %>% filter(cardClass ==
                                                                                 input$class) %>% arrange(desc(cost)) %>% head(10)
                       }
                       
                       if (input$rarity != '-')
                       {
                         if (exists("health") & exists("attack") & exists("cost"))
                         {
                           health = health %>% filter(rarity == input$rarity)
                           attack = attack %>% filter(rarity == input$rarity)
                           cost = cost %>% filter(rarity == input$rarity)
                         }
                         
                         else
                         {
                           health = deck %>% group_by_at(input$choices) %>% filter(rarity == input$rarity) %>% arrange(desc(health)) %>% head(10)
                           attack = deck %>% group_by_at(input$choices) %>% filter(rarity ==
                                                                                     input$rarity) %>% arrange(desc(attack)) %>% head(10)
                           cost = deck %>% group_by_at(input$choices) %>% filter(rarity ==
                                                                                   input$rarity) %>% arrange(desc(cost)) %>% head(10)
                         }
                       }
                       
                       if (input$artist != '-')
                       {
                         if (exists("health") & exists("attack") & exists("cost"))
                         {
                           health = health %>% filter(artist == input$artist)
                           attack = attack %>% filter(artist == input$artist)
                           cost = cost %>% filter(artist == input$artist)
                         }
                         
                         
                         else
                         {
                           health = deck %>% group_by_at(input$choices) %>% filter(artist == input$artist) %>% arrange(desc(health)) %>% head(10)
                           attack = deck %>% group_by_at(input$choices) %>% filter(artist == input$artist) %>% arrange(desc(attack)) %>% head(10)
                           cost = deck %>% group_by_at(input$choices) %>% filter(artist == input$artist) %>% arrange(desc(cost)) %>% head(10)
                         }
                       }
                       
                       a_list = list(health = health,
                                     attack = attack,
                                     cost = cost)
                       a_list
                       
                     })
                   }
                   else
                   {
                     stats = reactive({
                       health = deck %>% group_by_at(input$choices) %>% arrange(desc(health)) %>% head(10)
                       attack = deck %>% group_by_at(input$choices) %>% arrange(desc(attack)) %>% head(10)
                       cost = deck %>% group_by_at(input$choices) %>% arrange(desc(cost)) %>% head(10)
                       
                       a_list = list(health = health,
                                     attack = attack,
                                     cost = cost)
                       a_list
                     })
                   }
                   
                   data = stats()
                   title = reactive({
                     
                     w = ""
                     n = ""
                     if ("rarity" %in% input$choices & input$rarity != "-")
                     {
                       
                       w = str_c(w," ",input$rarity," ")
                       n = str_c(n, " rarity ")
                     }
                     
                     
                     if ("artist" %in% input$choices &
                         input$artist != "-")
                     {
                       w = str_c(w, " ",input$artist," ")
                       n = str_c(n, " artist ")
                     }
                     if ("cardClass" %in% input$choices &
                         input$class != "-")
                     {
                       w = str_c(w," ",input$class," ")
                       n = str_c(n, " CardClass ")
                     }
                     if (w!="") 
                     
                      { 
                      
                       w = strsplit(w," ") %>% unlist() %>% paste(collapse = " ") 
                       w = str_c("Filter By ", w)
                     }
                     
                     if (n!="")
                     {
                       n = strsplit(n," ") %>% unlist() %>% paste(collapse = " ")
                       n = str_c("Group By ", n,"; ")
                       
                     }
                     cast = str_c(n,w)
                     
                     cast
                     
                   })
                   p = title()
                   output$tableone = renderDataTable({
              
                     data$health
                   }, caption= str_c(p,"\n","Top 10 Health"))
                   output$img1 = renderImage({
                     data$health
                   })
                   
                   output$tabletwo = renderDataTable({
                  
                     data$attack
                   }, caption=str_c(p,"\n","Top 10 Attack"))
                   output$tablethree = renderDataTable({
                     
                     data$cost
                   }, caption = str_c(p,"\n","Top 10 Cost"))
                   show("answer1")
                 }
                 else if (input$vars == "Data Visualization")
                 {
                   stats = reactive({
                     if (is.null(input$rarity))
                     {
                       if (input$rarity != "-")
                         frame = deck %>% filter(rarity == input$rarity) %>% drop_na(cost, health, attack)
                     }
                     
                     
                     
                     if (is.null(input$class))
                     {
                       if (input$class != "-")
                       {
                         if (exists("frame")) {
                           frame = frame %>% filter(cardClass == input$class) %>% drop_na(cost, health, attack)
                         }
                       }
                       
                     }
                     else
                     {
                       frame = deck %>% filter(cardClass == input$class) %>% drop_na(cost, health, attack)
                     }
                    
                     frame
                     
                    
                     
                   })
                   
                   output$summary = renderPrint({
                     summary(stats())
                   })
                   output$plot1 = renderPlot({
                     ggplot(data = stats(), aes(cost, health)) + geom_point() + geom_abline()
                   })
                   
                   output$plot2 = renderPlot({
                     ggplot(data = stats(), aes(attack, health)) + geom_point() + geom_abline()
                   })
                   output$plot3 = renderPlot({
                     ggplot(data = stats(), aes(cost, attack)) + geom_point() + geom_abline()
                   })
                   show("answer2")
                 }
               })
  
  
  observeEvent(input$reset, {
    hide("answer1")
    hide("answer2")
    reset("ask")
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)
