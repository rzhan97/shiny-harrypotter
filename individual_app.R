library(shiny)
library(shinythemes)
library(wordcloud)
library(memoise)
library(tm)

#========================================global.R=========================================================

role <- readxl::read_xlsx("Characters_2.xlsx")
HP1 <- readxl::read_xlsx("Harry_Potter_1.xlsx")
HP2 <- readxl::read_xlsx("Harry_Potter_2.xlsx")
HP3 <- readxl::read_xlsx("Harry_Potter_3.xlsx")

character1 <- as.data.frame(table(HP1$Character))
newdata1 <- character1$Var1[character1$Freq > 10]

character2 <- as.data.frame(table(HP2$Character))
newdata2 <- character2$Var1[character2$Freq > 10]
newdata2 <- newdata2[2:20]

character3 <- as.data.frame(table(HP3$Character))
newdata3 <- character3$Var1[character3$Freq > 10]
newdata3 <- newdata3[2:length(newdata3)]


#===================================my getTermMatrix Function===========================================================
getTermMatrix1 <- memoise(function(name) {
  
  text <- HP1$Sentence[HP1$Character ==  name]
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but","dont"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

getTermMatrix2 <- memoise(function(name) {
  
  text <- HP2$Sentence[HP2$Character ==  name]
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but","dont"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

getTermMatrix3 <- memoise(function(name) {
  
  text <- HP3$Sentence[HP3$Character ==  name]
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but","dont"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})



#===========================================================Define my UI===========================================================


ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage("Harry Potter World",
             tabPanel("Find a Wizard",
                      sidebarLayout(
                        sidebarPanel(
                          h4("What information do you want to know"),
                          # conditionalPanel('input.dataset === "role"',
                          #   checkboxGroupInput("features","Information:",
                          #                      names(role),selected = names(role))
                          #)         
                          #),
                          selectizeInput("features",label = "Which information you want to know?",
                                         choices = names(role),selected = "Name",
                                         multiple = TRUE),
                        ),
                        mainPanel(
                          tabsetPanel(
                            id = 'dataset',
                            tabPanel("role",DT::dataTableOutput("suggestions"))
                          ))
                      )
             )
             
             
             ,tabPanel("Harry Potter and the Philosopher's Stone", icon = icon("cloud"),
                       sidebarLayout(
                         sidebarPanel(
                           selectizeInput("name1",label = "Select your favorite character from this list.",
                                          choices = newdata1,
                                          selected = "Harry",
                                          multiple = FALSE,options = NULL),
                           actionButton("update1", "Change"),
                           hr(),
                           sliderInput("freq", "Minimum Frequency:",
                                       min = 1,  max = 100, value = 1),
                           sliderInput("max", "Maximum Number of Words:",
                                       min = 1,  max = 500,  value = 25)),
                         mainPanel(plotOutput("plot1")
                         )
                       )
             )             
             ,tabPanel("Harry Potter and the Chamber of Secrets", icon = icon("cloud"),
                       sidebarLayout(
                         sidebarPanel(
                           selectizeInput("name2",label = "Select your favorite character from this list.",
                                          choices = newdata2,
                                          selected = "HARRY",
                                          multiple = FALSE,options = NULL),
                           actionButton("update2", "Change"),
                           hr(),
                           sliderInput("freq", "Minimum Frequency:",
                                       min = 1,  max = 100, value = 1),
                           sliderInput("max", "Maximum Number of Words:",
                                       min = 1,  max = 500,  value = 25)),
                         mainPanel(plotOutput("plot2")
                         )
                       )
             )
             
             ,tabPanel("Harry Potter and the Prisoner of Azkaban", icon = icon("cloud"),
                       sidebarLayout(
                         sidebarPanel(
                           selectizeInput("name3",label = "Select your favorite character from this list.",
                                          choices = newdata3,
                                          selected = "HARRY",
                                          multiple = FALSE,options = NULL),
                           actionButton("update3", "Change"),
                           hr(),
                           sliderInput("freq", "Minimum Frequency:",
                                       min = 1,  max = 100, value = 1),
                           sliderInput("max", "Maximum Number of Words:",
                                       min = 1,  max = 500,  value = 25)),
                         mainPanel(plotOutput("plot3")
                         )
                       )
             )
             
             
  )
)
#==========================================================Define my Server====================================================

server <- function(input, output, session) {
  
  role2 = role[sample(nrow(role), 140), ]
  output$suggestions <- DT::renderDataTable({
    DT::datatable(role2[, input$features, drop = FALSE],)
  })
  
  
  terms1 <- reactive({
    input$update1
    isolate({withProgress({
      setProgress(message = "Processing...")
      getTermMatrix1(input$name1)})
    })
  })
  
  output$plot1 <- renderPlot({
    v1 <- terms1()
    matrix1 <- as.matrix(v1) 
    words1 <- sort(rowSums(matrix1),decreasing=TRUE) 
    df1 <- data.frame(word = names(words1),freq=words1)
    
    wordcloud(words = df1$word, 
              freq = df1$freq, 
              min.freq = input$freq,
              max.words=input$max, 
              random.order=FALSE, 
              rot.per=0.35,
              colors=brewer.pal(8, "Dark2"))
  })
  
  
  
  
  
  
  
  terms2 <- reactive({
    input$update2
    isolate({withProgress({
      setProgress(message = "Processing...")
      getTermMatrix2(input$name2)})
    })
  })
  
  output$plot2 <- renderPlot({
    v2 <- terms2()
    matrix2 <- as.matrix(v2) 
    words2 <- sort(rowSums(matrix2),decreasing=TRUE) 
    df2 <- data.frame(word = names(words2),freq=words2)
    
    wordcloud(words = df2$word, 
              freq = df2$freq, 
              min.freq = input$freq,
              max.words=input$max, 
              random.order=FALSE, 
              rot.per=0.35,
              colors=brewer.pal(8, "Dark2"))
  })
  
  
  
  
  
  
  
  
  terms3 <- reactive({
    input$update3
    isolate({withProgress({
      setProgress(message = "Processing...")
      getTermMatrix3(input$name3)})
    })
  })
  
  
  output$plot3 <- renderPlot({
    v3 <- terms3()
    matrix3 <- as.matrix(v3) 
    words3 <- sort(rowSums(matrix3),decreasing=TRUE) 
    df3 <- data.frame(word = names(words3),freq=words3)
    
    wordcloud(words = df3$word, 
              freq = df3$freq, 
              min.freq = input$freq,
              max.words=input$max, 
              random.order=FALSE, 
              rot.per=0.35,
              colors=brewer.pal(8, "Dark2"))
  })
  
  
  
  ### Session Information
  output$sessionInfo <- renderPrint({
    capture.output(sessionInfo())
  })
}

#==============================================================Call my app=============================================================

shinyApp(ui, server)

