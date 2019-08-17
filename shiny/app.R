library(shiny)
library(tibble)
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(DT)
library(emo) # https://github.com/hadley/emo

dir.create(path = "items", showWarnings = FALSE)

ui <- fluidPage(
  
  titlePanel(title = "Shopping list"),
  
  sidebarLayout(sidebarPanel(
    tags$head(tags$script(src = "enter.js")),
    shiny::uiOutput("add_item"),
    actionButton("add_item_now", "Add item"),
    actionButton("veggie", emo::ji("apple")),
    actionButton("fridge", emo::ji("snowflake")),
    actionButton("bread", emo::ji("bread")),
    actionButton("soap", emo::ji("soap"))
  )
  ,
  mainPanel(shiny::h2("Buy"),
            DT::DTOutput(outputId = "current_list_buy"),
            shiny::h2("Bought"),
            DT::DTOutput(outputId = "current_list_bought"),
            shiny::hr(),
            shinyWidgets::switchInput(inputId = "add_remove_switch",
                                      value = TRUE,
                                      onLabel = "Buy/Bought",
                                      offLabel = "Remove items",
                                      size = "small",
                                      inline = FALSE)
  )
  )
)


server <- function(input, output, session) {
  
  shopping_list <- reactiveValues()
  
  shopping_list$list <- purrr::map_df(.x = list.files(path = "items",
                                                      pattern = "csv",
                                                      full.names = TRUE),
                                      .f = readr::read_csv,
                                      col_types = cols(
                                        Item = col_character(),
                                        Buy = col_logical()
                                      ))
  
  
  
  output$add_item <- shiny::renderUI({
    shiny::textInput(inputId = "add_item",
                     label = "Add item")
  })
  
  shiny::observeEvent(input$add_item_now, {
    item_file_location <- file.path("items", paste0(tolower(stringr::str_squish(stringr::str_replace_all(input$add_item, "[^[:alnum:]]", " "))), ".csv"))
    readr::write_csv(x = tibble(Item = input$add_item, Buy = TRUE), path = item_file_location)
    updateTextInput(session = session, inputId = "add_item", value = "")
  })
  
  shiny::observeEvent(input$bread, {
    updateTextInput(session = session,
                    inputId = "add_item",
                    value = paste(emo::ji("bread"), ""))
  })
  
  shiny::observeEvent(input$veggie, {
    updateTextInput(session = session,
                    inputId = "add_item",
                    value = paste(emo::ji("apple"), ""))
  })
  
  
  shiny::observeEvent(input$fridge, {
    updateTextInput(session = session,
                    inputId = "add_item",
                    value = paste(emo::ji("snowflake"), ""))
  })
  
  shiny::observeEvent(input$soap, {
    updateTextInput(session = session,
                    inputId = "add_item",
                    value = paste(emo::ji("soap"), ""))
  })
  
  shiny::observeEvent(input$canned_food, {
    updateTextInput(session = session,
                    inputId = "add_item",
                    value = paste(emo::ji("canned_food"), ""))
  })
  
  output$current_list_buy <- renderDT(expr =  shopping_list() %>% na.omit() %>% filter(Buy == TRUE) %>% select(Item) %>% dplyr::arrange(Item),
                                      server = TRUE,
                                      options = list(pageLength = 5000,
                                                     rowReorder = TRUE,
                                                     dom = "t"),
                                      rownames= FALSE,
                                      colnames = NULL)
  
  output$current_list_bought <- renderDT(expr =  shopping_list() %>% na.omit() %>% filter(Buy == FALSE) %>% select(Item) %>% dplyr::arrange(Item),
                                         server = TRUE,
                                         options = list(pageLength = 5000,
                                                        rowReorder = TRUE,
                                                        dom = "t"),
                                         rownames= FALSE,
                                         colnames = NULL)
  
  shiny::observeEvent(input$current_list_buy_rows_selected, {
    
    if (is.null(input$current_list_buy_rows_selected)==FALSE) {
      if (length(input$current_list_buy_rows_selected)>0) {
        item_file_location <- file.path("items", paste0(tolower(stringr::str_squish(stringr::str_replace_all(shopping_list() %>% filter(Buy == TRUE) %>% dplyr::arrange(Item) %>% slice(input$current_list_buy_rows_selected) %>% pull(Item), "[^[:alnum:]]", " "))), ".csv"))
        if (input$add_remove_switch==FALSE) {
          unlink(item_file_location)
        } else {
        readr::read_csv(file = item_file_location,
                        col_types = cols(
                          Item = col_character(),
                          Buy = col_logical()
                        )) %>% 
          mutate(Buy = !Buy) %>% 
          write_csv(path = item_file_location)
        }
      }
    }
    
  })
  
  shiny::observeEvent(input$current_list_bought_rows_selected, {
    
    if (is.null(input$current_list_bought_rows_selected)==FALSE) {
      if (length(input$current_list_bought_rows_selected)>0) {
        item_file_location <- file.path("items", paste0(tolower(stringr::str_squish(str_replace_all(shopping_list() %>% filter(Buy == FALSE) %>% dplyr::arrange(Item) %>% slice(input$current_list_bought_rows_selected) %>% pull(Item), "[^[:alnum:]]", " "))), ".csv"))
        if (input$add_remove_switch==FALSE) {
          unlink(item_file_location)
        } else {
          readr::read_csv(file = item_file_location, col_types = cols(
            Item = col_character(),
            Buy = col_logical()
          )) %>% 
            mutate(Buy = !Buy) %>% 
            write_csv(path = item_file_location)
        }
      }
    }
    
  })
  
  shopping_list <- reactivePoll(1000, session,
                                # This function returns the time that any item was last modified
                                checkFunc = function() {
                                  purrr::map(.x = list.files(path = "items", full.names = TRUE), .f = file.mtime)
                                },
                                # This function returns the content of all items
                                valueFunc = function() {
                                  purrr::map_df(.x = list.files(path = "items", pattern = "csv", full.names = TRUE), .f = read_csv, col_types = cols(
                                    Item = col_character(),
                                    Buy = col_logical()
                                  ))
                                }
  )
  
}


shinyApp(ui = ui, server = server)

