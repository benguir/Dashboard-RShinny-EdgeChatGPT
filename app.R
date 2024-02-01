library(shiny)
library(httr)

ui <- fluidPage(
  fluidRow(
    column(5),
    column(1,
           br(),
           img(src = "edge.png", height = 50, width = 50, align = "center")
    ),
    column(1,
           h1("Edge", align = "center")
    )
  ),
  
  # p("", align = "center"),
  
  fluidRow(
    column(4,
           # h3("Configuration"),
           # passwordInput("token_input", " "),
           # selectInput("language_select", "Choisir la langue:",
           #             choices = c("Français", "Anglais", "Espagnol"),
           #             selected = "Français"),
           # selectInput("model_select", "Choisir le model :",
           #             choices = c("gpt-3.5-turbo", "gpt-4"),
           #             selected = "gpt-3.5-turbo"),
           # selectInput("temperature_select", "Choisir la temperature :",
           #             choices = c("0", "1"),
           #             selected = "0")
    ),
    
    column(3,
           fluidRow(
             # h3("Nouveau Message"),
             textInput("input_text", " "),
             # actionButton("submit_btn", "Rechercher"),
             # actionButton("clear_btn", "Effacer")
           ),
           
           fluidRow(
             # h3("Historique de Chat"),
             # verbatimTextOutput("chat_history")
           )
    ),
    column(4,
           fluidRow(
             br(),
             actionButton("submit_btn", "Rechercher"),
             actionButton("clear_btn", "Effacer")
           )
    )
  ),
  fluidRow(
    verbatimTextOutput("chat_history")
  )
)

server <- function(input, output, session) {
  chat_history <- reactiveVal("")  
  token <- reactiveVal("")
  
  observeEvent(input$submit_btn, {
    # token(input$token_input)
    token("TOKEN-ICI")
    
    api_url <- "https://api.openai.com/v1/chat/completions"
    input_text <- input$input_text
    
    new_message <- input_text
    current_history <- chat_history()
    updated_history <- paste(current_history, new_message, sep = "\n")
    chat_history(updated_history)
    
    full_prompt <- paste(updated_history, collapse = "\n")
    
    request_body <- list(
      model = "gpt-3.5-turbo",
      messages = list(
        list(role = "system", content = "La langue de la conversation : Français"),
        list(role = "user", content = full_prompt)
      )
    )
    
    response <- httr::POST(api_url,
                           add_headers(Authorization = paste("Bearer", token())),
                           body = request_body,
                           encode = "json")
    
    result <- httr::content(response, "text", encoding = "UTF-8")
    result_json <- fromJSON(result)
    assistant_response <- result_json$choices$message$content[1]
    
    new_message <- assistant_response
    current_history <- chat_history()
    updated_history <- paste(current_history, new_message, sep = "\n")
    chat_history(updated_history)
    
    output$output_text <- renderText({
      new_message
    })
  })
  
  observeEvent(input$clear_btn, {
    chat_history("")
  })
  
  output$chat_history <- renderText({
    chat_history()
  })
}

shinyApp(ui, server)
