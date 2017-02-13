if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinyjs, dplyr)


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
          p(paste0('CART UID:', unique_trays$CART.ID[[i]])),
          p(paste0('expiration date for this cart is ', unique_trays$Cart_expire[[i]])),
          a(id = paste0("toggleAdvanced", i), "Show/hide advanced info"),
          hidden(
            div(id = paste0("advanced", i),
                renderDataTable(df %>% 
                                  filter(CART.TYPE == unique_trays$CART.TYPE[[i]], CART.ID == unique_trays$CART.ID[[i]]) %>% 
                                  select(DRUG, QTY, EXPIRATION, LOT, DAYS.LEFT))
            )
          ),
          tags$hr()
        )
      })
  })
  
  
  
    }
)


