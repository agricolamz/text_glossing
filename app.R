library(shiny)
library(stringr)

ui <- fluidPage(
    column(6,
           textAreaInput(inputId = "input_text", label = NULL, width = "100%", height = "700px")
           ),
    column(6,
           htmlOutput("glossed"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$glossed <- renderText({
        orig_text <- input$input_text
        # orig_text <- paste0(stringi::stri_rand_lipsum(5), collapse = "\n")
        text <- str_replace_all(orig_text, "\\.\\.\\.", "…")
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
