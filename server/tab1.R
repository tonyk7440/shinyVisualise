data_sets <- c("mtcars", "diamonds")

output$choose_dataset <- renderUI({
    selectInput("dataset", "Data set", as.list(data_sets))
})

myData <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                     quote=input$quote)
    data
})
output$contents <- renderTable({
    myData()
})