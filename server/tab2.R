library(shiny)
library(ggplot2)
library(Cairo)
library(DT)

observeEvent(myData(), ({
    output$text1 <- renderText({ 
        colnames <- names(myData())
        paste("You data has ", dim(data)[1], " rows and ", dim(data)[2], " columns.")
    })
    
    output$x_axis <- renderUI({
        # Get the data set with the appropriate name
        colnames <- names(myData())
        
        selectInput("x_axis", "x-axis",  as.list(colnames))
    })
    
    output$y_axis <- renderUI({
        # Get the data set with the appropriate name
        colnames <- names(myData())
        
        selectInput("y_axis", "y-axis",  as.list(colnames))
    })
    
    observe({
        # We'll use the input$controller variable multiple times, so save it as x
        # for convenience.
        t <- "Title"
        x <- input$x_axis
        y <- input$y_axis
        
        # This will change the value of input$inText, based on x
        updateTextInput(session, inputId = "title", value = t)
        updateTextInput(session, inputId = "x_label", value = x)
        updateTextInput(session, inputId = "y_label", value = y)
    })
    # Name of the x, y, and faceting variables
    xvar <- reactive({
        if (input$ggplot_scaletype == "x_factor")
        {
            switch(input$dataset, mtcars = "cyl", diamonds = "cut", grid = "xf")
            
        } else if (input$ggplot_scaletype == "datetime") {
            "datetime"
        } else {
            switch(input$dataset, mtcars = "wt", diamonds = "carat", grid = "x")
        }
    })
    
    yvar <- reactive({
        if (input$ggplot_scaletype == "datetime") {
            "date"
        } else {
            switch(input$dataset, mtcars = "mpg", diamonds = "price", grid = "y")
        }
    })
    
    facetvar1 <- reactive({
        if (input$ggplot_facet == "none") return(NULL)
        
        switch(input$dataset, mtcars = "cyl", diamonds = "cut", grid = "facet1")
    })
    
    facetvar2 <- reactive({
        if (input$ggplot_facet == "none") return(NULL)
        if (!(input$ggplot_facet %in% c("grid_xy", "grid_xy_free"))) return(NULL)
        
        switch(input$dataset, mtcars = "am", diamonds = "clarity", grid = "facet2")
    })
    
    
    plotInput <- function(){
        pc <- ggplot(myData(), aes_string(input$x_axis, y=input$y_axis)) +
            geom_point() +
            labs(x=input$x_label,y=input$y_label) +
            ggtitle(input$title) +
            theme_bw()
        
        p <- switch(input$ggplot_scaletype,
                    normal =
                        pc,
                    reverse =
                        pc + scale_x_reverse() + scale_y_reverse(),
                    log10 =
                        pc + scale_x_log10() + scale_y_log10(),
                    log2 =
                        pc + scale_x_continuous(trans = scales::log2_trans()) +
                        scale_y_continuous(trans = scales::log2_trans()),
                    log10_trans =
                        pc + coord_trans(x = "log10", y = "log10"),
                    log2_trans =
                        pc + coord_trans(xtrans = "log2", ytrans = "log2"),
                    coord_cartesian =
                        pc + coord_cartesian(xlim = c(2,4), ylim = c(0,50)),
                    coord_flip =
                        pc + coord_flip(),
                    coord_polar =
                        pc + coord_polar(),
                    # Discrete x, continuous y
                    x_factor =
                        pc
        )
        
        # Need to pass faceting specs as strings
        p <- switch(input$ggplot_facet,
                    none =
                        p,
                    wrap = {
                        # facet_wrap needs a formula object (bug: it can't take a string)
                        facet_formula <- as.formula(paste("~", facetvar1()))
                        p + facet_wrap(facet_formula, ncol = 2)
                    },
                    grid_x =
                        p + facet_grid(paste(". ~", facetvar1())),
                    grid_y =
                        p + facet_grid(paste(facetvar1(), "~ .")),
                    grid_xy =
                        p + facet_grid(paste(facetvar2(), "~",  facetvar1())),
                    grid_xy_free =
                        p + facet_grid(paste(facetvar2(), "~",  facetvar1()), scales = "free")
        )
        p
        
    }
    
    output$plotui <- renderUI({
        plotOutput("plot",
                   click = "plot_click",
                   dblclick = dblclickOpts(
                       id = "plot_dblclick"
                   ),
                   brush = brushOpts(
                       id = "plot_brush",
                       direction = input$brush_dir,
                       resetOnNew = input$brush_reset
                   )
        )
    })
    
    output$plot <- renderPlot({
        plotInput()
    })
    
    output$downloadPlot <- downloadHandler(
        filename = function() { paste(input$dataset, '.png', sep='') },
        content = function(file) {
            ggsave(file, plot = plotInput(), device = "png")
        }
    )
    
    output$plot_clicked_points <- DT::renderDataTable({
        data <- myData()
        
        res <- nearPoints(data, input$plot_click,
                          threshold = input$max_distance, maxpoints = input$max_points,
                          addDist = TRUE)
        
        res$dist_ <- round(res$dist_, 1)
        
        datatable(res)
    })
    
    output$plot_brushed_points <- DT::renderDataTable({
        data <- myData()
        res <- brushedPoints(data, input$plot_brush)
        
        datatable(res)
    })
})
)