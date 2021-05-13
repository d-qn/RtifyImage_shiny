library(shiny)
library(colourpicker)
library(shinythemes)
library(thematic) # theme ggplot2 to shiny theme
library(htmltools)
source("helpers.R")

thematic_on(bg = "auto")

# TO DO use tabsetpanel for extra options: https://github.com/nsgrantham/tidytuesdayrocks/blob/master/app.R
# https://nsgrantham.shinyapps.io/tidytuesdayrocks/


##### shiny ####

ui <- fluidPage(
    tags$head(
        tags$link(href = "https://fonts.googleapis.com/css?family=Roboto+Mono", rel = "stylesheet"),
        tags$style(HTML('
      * {
        font-family: Roboto Mono;
        font-size: 100%;
       }
        h1,h2,h3,h4 {
          font-family: Roboto Mono;
        }'
    ))),
    theme = shinytheme("slate"),
    
    h1("aRty face"),
    
    sidebarLayout(
        position = "right",
        sidebarPanel(
            # "Window width:",
            # textOutput("size"),
            fileInput("upload", h4("Upload an image"),
                      accept = c('image/png', 'image/jpeg', 'image/gif', 'image/jpg')),
            sliderInput("longest_dim", "Longest pixel dimension",
                               min = 40, max = 150, value = 80),
            selectInput(
            "rtype", "Choose a transformation", 
            c("point", "line", "rgb", "split bar", "b-spline", "ascii"),
            multiple = F),
            uiOutput("ui"),
            uiOutput("ui2"),
            uiOutput("uipoint"),
        width = 5),
        mainPanel(
            plotOutput("plot", height = "450px"),
        width = 7)
    )

)

server <- function(input, output, session) {
   
    img <- reactive({
        inFile <- input$upload
        if (is.null(inFile)) {
          loadImageResize("audrey.jpg", input$longest_dim)
        } else {
            loadImageResize(inFile$datapath, input$longest_dim)
        }
    })
    
    artype <- reactive(input$rtype)
    
    shape <- reactive({
      switch(input$shape,
        "point" = 16, 
        "triangle" = 17, 
        "diamond" = 18, 
        "square" = 15, 
        "smaller point" = 20,
        "$" = 36,
        stop("Invalid input shape")
      )
    })
    
    output$ui <- renderUI({
        if (is.null(img()))
            return()
        else {
            # Depending on input$artype, we'll generate a different
            # UI component and send it to the client.
            switch(artype(),
                   "point" = ,
                   "line" = ,
                   "split bar" =,
                   "b-spline" = colourInput(
                   "bgcol", "Select a background colour", value = input$bgcol %||% "#EDEDC2")
            )
        }
    })
    output$ui2 <- renderUI({
        if (is.null(img()))
            return()
        else {
            # Depending on input$artype, we'll generate a different
            # UI component and send it to the client.
            switch(artype(),
                   "point" = ,
                   "line" = ,
                   "split bar" =,
                   "b-spline" = colourInput(
                       "fgcol", "Select a foreground colour", 
                       value = input$fgcol %||% "#6B0F5C"
                   )
            )
        }
    })
    
    output$uipoint <- renderUI({
        if (is.null(img()))
            return()
        else {
            # Depending on input$artype, we'll generate a different
            # UI component and send it to the client.
            switch(artype(),
                   "point" = selectInput(
                       "shape", "Select a shape", 
                       c("point", "triangle", "diamond", "square", "smaller point", 
                         "$"),
                       multiple = F,
                       selected = input$shape %||% "point"
                   ),
                   NULL
            )
        }
    })
    
    output$plot <- renderPlot({
        if(!is.null(img())) {
            ff <- switch(artype(),
                "point" = portraitPoint,
                "line"  = portraitLine,
                "rgb"   = portraitRgb,
                "split bar" = portraitSplitbar,
                "b-spline"  = portraitBspline,
                "ascii"     = portaitAscii,
                stop("Invalid transformation input value")
            )
         
            if(is.function(ff)) {
                switch(artype(),
                       "line"  = ,
                       "split bar" = ff(img()[[1]], col_fill = ifelse(is.null(input$fgcol), "black", input$fgcol) , col_bg = input$bgcol),
                       "b-spline" = ff(img()[[1]], image_ratio = img()[[2]], col_fill = ifelse(is.null(input$fgcol), "black", input$fgcol), col_bg = input$bgcol),
                       "rgb" = ff(img()[[1]], image_ratio = img()[[2]]),
                       "ascii" = ff(img()[[1]]),
                       "point" = ff(img()[[1]], shape = shape() %||% 16, col_fill = ifelse(is.null(input$fgcol), "black", input$fgcol) , col_bg = input$bgcol),
                       stop("invalid ff")
                )
            }   
        } 
    })
}

shinyApp(ui, server)
