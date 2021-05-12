library(shiny)
library(shinybrowser) # Detect window size to resize output image 
library(colourpicker)
library(shinythemes)
library(thematic) # theme ggplot2 to shiny theme
library(htmltools)
source("helpers.R")

thematic_on(bg = "auto")

# myfooter <- function() {
#     div(p(strong("Built with ♥︎ thanks to") Rstudio and Shiny."), 
#         p(strong("R Packages:"), "tidyverse, tidytext, wordcloud2, tidygraph, vizNetwork, glue."),
#         p(strong("Sources:"), a("genius.com", href = "https://genius.com/albums/The-magnetic-fields/69-love-songs"), "for lyrics,", a("wikipedia", href = "https://en.wikipedia.org/wiki/69_Love_Songs"), "for singers."),
#         style="text-align: right;")
#     
# }



##### shiny ####

ui <- fluidPage(
    theme = shinytheme("slate"),
    titlePanel("aRtify my face"),
    shinybrowser::detect(),
    # "Window width:",
    # textOutpu t("size"),
    fluidRow(
        column(6, fileInput("upload", "Upload an image",
                            accept = c('image/png', 'image/jpeg', 'image/gif', 'image/jpg'))),
        column(6,    selectInput(
            "rtype", "Choose a transformation", 
            c("point", "line", "rgb", "split bar", "b-spline", "ascii"),
            multiple = F
        ))
    ),
    htmlOutput("transformation"),
    plotOutput("plot", height = "450px"),
    uiOutput("ui"),
    uiOutput("ui2"),
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
                       "bgcol", "Select a background colour", value = input$bgcol %||% "#EDEDC2"
                   )
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
                       "fgcol", "Select a foreground colour", value = input$fgcol %||% "#EF8BA5"
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
                       "split bar" = ff(img()[[1]], col_fill = ifelse(is.null(input$fgcol), "black", input$fgcol) , col_bg = input$bgcol),
                       "b-spline" = ff(img()[[1]], image_ratio = img()[[2]], col_fill = ifelse(is.null(input$fgcol), "black", input$fgcol), col_bg = input$bgcol),
                       "rgb" = ff(img()[[1]], image_ratio = img()[[2]]),
                       "ascii" = ff(img()[[1]])
                )
            }   
        } 
    })
}

shinyApp(ui, server)
