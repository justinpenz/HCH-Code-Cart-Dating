library(shiny)
library(shinyjs)


shinyApp(
  ui = fluidPage(
    useShinyjs(),
    uiOutput("openclick")), 
    
  server = function(input, output, session) {
   
    unique_trays <- df %>% group_by(CART.TYPE, CART.ID) %>%  summarize(Cart_expire = min(EXPIRATION)) %>% mutate(UID = paste(CART.TYPE, CART.ID)) 
    
    
    output$openclick <- renderUI({
      
      
      
      lapply(1:nrow(unique_trays), function(i) {
        
          onclick(paste0("toggleAdvanced", i), toggle(id = paste0("advanced", i), anim = TRUE))
      
          div(
          a(id = paste0("toggleAdvanced", i), "Show/hide advanced info"),
          hidden(
            div(id = paste0("advanced", i),
                numericInput("age", "Age", 30),
                textInput("company", "Company", "")
            )
          )
        )
      })
  })
  
  
  
    }
)


