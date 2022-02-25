# load the necessary library
library(tidyverse)
library(tidytext)
library(Rcpp)
library(textdata)
library(rvest)
library(maps)
library(stringr)
library(SnowballC)
library(udpipe)
library(widyr)
library(lubridate)
library(gmodels)
library(shiny)
library(plotly)

# setting theme
theme_set(theme_bw(16))

mytheme <- theme(axis.text=element_text(size=12),
                 axis.title=element_text(size=14,face="bold")) +
  theme_grey(base_size = 10)



# https://nicholustintzaw.shinyapps.io/homework-3-nicholustintzaw/

# load necessary data file 
load("df_combined.rda")

date_list <- unique(df_combined$date_str)

ui <- fluidPage(
  titlePanel(
    div("Welcome to UNHCR Refugee Brief Sentiment Analysis", style = "color: steelblue")
  ),
  fluidRow(
    column(width = 12,
           align = "left", 
           selectInput(inputId = "date",
                       label = "Select issue date",
                       choices = date_list, 
                       selected = NULL)
    ),
  ),
  fluidRow(
    column(width = 6, 
           align = "center", 
           tableOutput("country_list")), 
    column(width = 6, 
           align = "center",
           plotlyOutput("afinn", height = "800"))
  ), 
  fluidRow(
    column(width = 12,
           align = "center",
           plotlyOutput("nrc", height = "800"))
  ),
  fluidRow(
    column(width = 12,
           align = "center",
           plotlyOutput("corr_plot", height = "800"))
  ),
  fluidRow(
    column(width = 12, 
           align = "right", 
           tags$p("Data: UNHCR: The Refugee Brief", style = "color:red"))
  )

)


  
server <- function(input, output){
  
  load("df_combined.rda")

  df <- df_combined
  
  data <- reactive({
    filter(df, date_str == input$date)
  })
  

  output$country_list <- renderTable({
    
    data() %>% 
      filter(!is.na(country)) %>%
      mutate(country = str_to_title(country), 
             topic_country = str_to_title(topic_country)) %>%
      arrange(sentence_id) %>%
      group_by(category, topic_country, country) %>%
      slice(1) %>%
      select(category, topic_country, country) %>%
      slice(1)
    
  })
  
  output$afinn <- renderPlotly({

    afinn_plot <- data() %>%
      group_by(date_str, sentence_id, word_tokens, stem, afinn) %>%
      mutate(country = str_to_title(country), 
             topic_country = str_to_title(topic_country)) %>% 
      slice(1) %>%
      filter(!is.na(afinn) & !is.na(topic_country)) %>%
      group_by(topic_country, stem) %>%
      mutate(n = row_number(), 
             afinn_n_multi = n * afinn) %>%
      slice(1) %>%
      ggplot(aes(afinn_n_multi, topic_country, fill = afinn_n_multi > 0)) +
      geom_col(show.legend = FALSE) + 
      labs(title = "Contribution to Sentiment Values by Issue (by AFINN)",
           x = "Sentiment Value (by AFINN) * Number of Occurances",
           y = "Topic Countries") + mytheme

    subplot(ggplotly(afinn_plot))

  })

  output$nrc <- renderPlotly({

    nrc_plot <- data() %>%
      group_by(date_str, sentence_id, word_tokens, stem, nrc) %>%
      mutate(country = str_to_title(country),
             topic_country = str_to_title(topic_country)) %>%
      slice(1) %>%
      filter(!is.na(nrc) & !is.na(topic_country)) %>%
      ggplot() +
      geom_histogram(aes(nrc, fill = topic_country), stat = "count") +
      scale_x_discrete(guide = guide_axis(angle = 1)) +
      labs(title = "Occurances of NRC Emotion Lexicon by Issue",
           y = "Number of Occurances",
           x = "NRC Word-Emotion Association Lexicon") +
      facet_wrap(facets = vars(topic_country)) +
      coord_flip() +
      mytheme + theme(legend.position = "none")

    subplot(ggplotly(nrc_plot))
  })

  output$corr_plot <- renderPlotly({

    word_cors <- data() %>%
      group_by(stem) %>%
      filter(n() >= 2) %>%
      pairwise_cor(stem, paragraph_id, sort = TRUE) %>%
      mutate(item2 = ifelse(item2 == "burkina", "burkina faso", item2))

    topic_country_vector <- unique(data()$topic_country)

    words_cor <- word_cors %>%
      filter(!is.nan(correlation)) %>%
      filter(item2 %in% topic_country_vector) %>%
      mutate(item2 = toupper(item2)) %>%
      group_by(item2) %>%
      arrange(-correlation) %>%
      filter(row_number() < 5 | row_number() > (n() - 5)) %>%
      ungroup() %>%
      mutate(item1 = reorder(item1, correlation)) %>%
      ggplot(aes(item1, correlation, fill = item2)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ item2, scales = "free") +
      labs(title = "Words that were most correlated with each Country Story",
           y = "Correlation Value",
           x = "4 Most and Less Correlated Words - with the topic country") +
      mytheme + theme(legend.position = "none") + coord_flip()

    subplot(ggplotly(words_cor))

  })

}

shinyApp(ui = ui, server = server)