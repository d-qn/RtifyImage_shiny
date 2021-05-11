#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Detect window size to resize output image 
# https://github.com/daattali/shinybrowser
# https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny


library(shiny)
library(shinybrowser)
library(colourpicker)
library(shinythemes)
library(thematic)

source("helpers.R")

thematic_on(bg = "auto")

##### shiny ####

ui <- fluidPage(
    theme = shinytheme("slate"),
    titlePanel("aRtify my image"),
    shinybrowser::detect(),
    "Window width:",
    textOutput("size"),
    fileInput("upload", "Upload an image",
              accept = c('image/png', 'image/jpeg', 'image/gif', 'image/jpg')),
    selectInput(
        "rtype", "Choose a transformation", 
        c("point", "line", "rgb", "split bar", "b-spline", "ascii"),
        multiple = F
    ),
    htmlOutput("transformation"),
    plotOutput("plot", height = "450px",),
    uiOutput("ui"),
    uiOutput("uipoint")

)

server <- function(input, output, session) {
   
    output$size <- renderText({
        paste(
            shinybrowser::get_width(), "x",
            shinybrowser::get_height()
        )
    })
    
    output$transformation <- renderText(
        if(!is.null(img())) {
            paste0(
                "<b>", input$rtype, "</b> transformation of your image")
        }
    )
    
    img <- reactive({
        inFile <- input$upload
        if (is.null(inFile)) {
            return()
        } else {
            loadImageResize(
                inFile$datapath, 
                ifelse( shinybrowser::get_width() < 500, 70, 90))
        }
    })
    
    artype <- reactive(input$rtype)
    
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
                       "bgcol", "Select a background colour", value = input$bgcol %||% "#F1E34C"
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
                       c("point", "triangle"),
                       multiple = F,
                       selected = input$shape %||% "point"
                   )
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
                       "point" = ,
                       "line"  = ,
                       "split bar" = ff(img()[[1]], col_bg = input$bgcol),
                       "b-spline" = ff(img()[[1]], image_ratio = img()[[2]], col_bg = input$bgcol),
                       "rgb" = ff(img()[[1]], image_ratio = img()[[2]]),
                       "ascii" = ff(img()[[1]])
                )
            }   
        } 
    })
}

shinyApp(ui, server)
