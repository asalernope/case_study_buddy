library(rsconnect)
library(shiny)
library(shinythemes)
library(vroom)
library(tidyverse)
setwd("~/Desktop/app/case_study_buddy")

## Add in the data

popPath <- "https://raw.githubusercontent.com/hadley/mastering-shiny/main/neiss/population.tsv" 
population <- vroom::vroom(popPath) 

prodPath<-"https://raw.githubusercontent.com/hadley/mastering-shiny/main/neiss/products.tsv"
products<- vroom::vroom(prodPath)

## Wouldn't let me just read in the injuries data because it was too big

injuries<-vroom::vroom("injuries.tsv", show_col_types = FALSE)


## setNames so that the codes are associated with the title
prod_codes <- setNames(products$prod_code, products$title)


## UI

ui <- fluidPage( theme = shinytheme("united"),
  titlePanel("Case Study App"),
  fluidRow(
    column(8,align="center",
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title), #have the choices be combined with the set of names
                       width = "100%"
           )
    ),
    
## Adding in the ability to select the number of rows. I used numeric input to create it because allows you to pick a numeric value (pretty self-explanatory)
    column(4, align="center", numericInput("rows", "Number of Rows",
                           min = 1, max = 10, value = 5)), #want to make this adjustable here for the max to go up as high as the max rows but not sure how in this... so i guess I will need to update in the server
 ),

  fluidRow( #creates the tables for each variable
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow( #a row for the plot output called age_sex
    column(2, align="center", selectInput("y", "Y axis", c("rate", "count"))), # allows you to select the input for the y-axis of the plot to either rate or count
    column(10, plotOutput("age_sex"))
  ),
  fluidRow(
    column(2, align="center", actionButton("storyb", "Previous story")),
    column(8, align="center", textOutput("narrative")),
    column(2, align="center", actionButton("story", "Next story")) #add in the action button that changes value by one each time it is pressed

  )
)

# to create the top five in order followed by an 'other' category
count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

## Server

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code)) 
  
# Chapter 10 explained kind of how to do this. I also googled how to do this max rows part since I couldn't figure out how to do something in the UI.... not sure if there is/should be a way?
  
  max_rows <- reactive(
  max(length(unique(selected()$diag)), #unique removes the duplicates and selected pulls from the dataset?
    length(unique(selected()$body_part)),
    length(unique(selected()$location)))
  )
  
#bindEvent and updateNumeric allows me to use the max number of rows as the max input allowed in the number of rows
observeEvent(input$code, {  
    updateNumericInput(session, "rows", max = max_rows())
   })
  
  
table_rows <- reactive(input$rows - 1) #because all the other categories get lumped into other you need the total rows - 1 because those will be put into other
  
# The output for the tables
  output$diag <- renderTable(count_top(selected(), diag, n = table_rows()), width = "100%") #use selected() in count_top for the df; also use table_rows for the updating since it is reactive
  output$body_part <- renderTable(count_top(selected(), body_part, n = table_rows()), width = "100%")
  output$location <- renderTable(count_top(selected(), location, n = table_rows()),width = "100%")

# reactive mutate
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
# plot from base app 
  
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")+
        theme_bw()
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people") + 
        theme_bw()
    }
  }, res=96)
  
# narrative section

  res <- reactiveValues(letterIndex = 1)
  
  observeEvent(input$story,{
    # the if statement is to keep the letterIndex in bounds
    # (between 1 and 26)
    if(res$letterIndex > 25) res$letterIndex <- 1
    else res$letterIndex <- res$letterIndex + 1
  })
  
  observeEvent(input$storyb,{
    if(res$letterIndex < 2) res$letterIndex <- 26
    else res$letterIndex <- res$letterIndex - 1
  })
  
  output$narrative <- renderText({
    selected()$narrative[res$letterIndex]
  })
  
}


shinyApp(ui=ui,server=server)
