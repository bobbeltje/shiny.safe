
library(shiny)
options(shiny.autoload.r=FALSE)

source('R/observeEvent.R', local=TRUE)

foo <- function() stop('bar')
# bar <- function(x=3, y='oi'){
#   x + y
# }

ui <- fluidPage(
  actionButton('a1', 'a1'),
  actionButton('a2', 'a2'),
  actionButton('a3', 'a3'),
  actionButton('a4', 'a4'),
  actionButton('a5', 'a5')
)

server <- function(input, output, session){
  rv <- reactiveValues(x='foo')
  observeEvent(input$a1, {
    stop('foo')
  })
  observeEvent(input$a2, {
    print('oi')
  })
  observeEvent(input$a3, {
    foo()
  })
  observeEvent(input$a4, {
    x <- rv$x
    print(input$a4 + x)
  })
  observeEvent(input$a5, {
    bar()
  })
}

shinyApp(ui, server)
