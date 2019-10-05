library(shiny)
library(stringr)

texts <- list.files("texts/")
fluidPage(
    tabsetPanel(type = "tabs",
                    tabPanel("texts",
                             fluidRow(
                                 column(5,
                                 selectInput(inputId = "selected_text", 
                                         label = NULL,
                                         choices = texts)),
                                 actionButton(inputId = "annotate_button",
                                              label = "annotate text"),
                                 actionButton(inputId = "save_button",
                                              label = "save text")),
                         column(6,
                                textAreaInput(inputId = "input_text",
                                              label = NULL,
                                              width = "120%",
                                              height = "700px",
                                              value = "")),
                         column(6,
                                htmlOutput("glossed"))),
    tabPanel("dictionary", DT::dataTableOutput("dictionary"))
)) -> ui

server <- function(input, output, session) {
    dictionary <- readr::read_csv("dictionary.csv")
    output$dictionary <- DT::renderDataTable(dictionary, options = list(pageLength = 100, dom = 'ftip'))
    output$glossed <- renderText({
        orig_text <- readLines(paste0("texts/", input$selected_text))
        
        if(input$input_text == ""){
            text <- orig_text
        } else{
            text <- input$input_text
        }
        observeEvent(input$annotate_button, ignoreInit = TRUE, {
            updateTextAreaInput(session, "input_text", value = orig_text)
        })
        text <- str_replace_all(text, "\\.\\.\\.", "…")
        inter <- unlist(str_extract_all(text, "[\\.\\?!…](\n)? ?"))
        inter <- str_remove_all(inter, "\n")
        text <- unlist(str_split(text, "[\\.\\?!…\n] ?"))
        text <- text[text!=""]
        text <- paste0(text, inter)
        paste0(1:length(text), ". ", text, "<br>")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
