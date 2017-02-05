#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarPanel(
    numericInput("assets", label = "Enter Number of variants in Experiment", value="3")
  ),
  mainPanel(
    uiOutput("variants"),
    uiOutput("lastVariant"))
)





# Define server logic required to draw a histogram
server <- function(input, output) {
   


  
  output$variants <- renderUI({
    
    
    
    unique_trays <- df %>% group_by(CART.TYPE, CART.ID) %>%  summarize(Cart_expire = min(EXPIRATION)) %>% mutate(UID = paste(CART.TYPE, CART.ID)) 
    
    
    lapply(1:nrow(unique_trays), function(i) {
      
      
      
      list(p(paste0(unique_trays$CART.ID[i], unique_trays$CART.TYPE[i])),
           p(paste0('expiration date is ', unique_trays$Cart_expire)),
           p('click to expand into individual drug expirations and lot numbers'),
           checkboxInput(eval(paste0('showPanel', i)), 'Show panel', FALSE),
           p(quote(paste0('showPanel', i))),
           tags$p(tags$u(h4(paste0("Variant ", i, ":")))),
           textInput(paste0("variant", i), label = "Variant Name", value = paste0("Variant ", i, " name...")), 
           numericInput(paste0("weight", i), label = "Proportion allocated (0 - 100)", value=0)
      )
    }) #end of lapply
  }) # end of renderUI
  
  output$lastVariant <- renderUI({
    numAssets <- as.integer(input$assets)
    for (j in 1:(numAssets-1)){
      if(j==1){x=100}
      x = x - input[[paste0("weight",j)]]
    }
    tagList(
      tags$p(tags$u(h4(paste0("Variant ", numAssets, ":")))),
      textInput(paste0("variantFinal"), label = "Variant Name", value = paste0("Variant ", numAssets, " name...")), 
      tags$p(tags$b("Proportion allocated (0 - 100)")),
      helpText(paste0(x))
    ) #end of tagList
  }) #end of renderUI
} #end of shinyServer



# Run the application 
shinyApp(ui = ui, server = server)
