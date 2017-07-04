library(EBImage)
library(shiny)


#purpose of this version 
#version 9
#keep and reset are working
#purpose of this version is to download image

#!!!!!!!!!!!!!!!!!!!!!!!!! ideas to save history !!!!!!!!!!!!!!!!!!!!!!!!!
#1) SAVE slider inputs


#to run shiny app
#must have the two libraries -- EBIMAGE and SHINY 
#  runApp('filename.R')

#-------- LINKS THAT I'M STILL USING TO HELP ME CREATE THE SHINY GUI ----------
#links to creating action button 
#https://shiny.rstudio.com/articles/action-buttons.html

#links to using ebimage
#https://www.r-bloggers.com/r-image-analysis-using-ebimage/

#ebimage documentation 
#https://www.bioconductor.org/packages/devel/bioc/manuals/EBImage/man/EBImage.pdf

#plotoutput options
#https://shiny.rstudio.com/reference/shiny/latest/plotOutput.html

#links to create brushing
#https://shiny.rstudio.com/articles/plot-interaction.html

#dynamic ui to fix upload button 
#https://shiny.rstudio.com/articles/dynamic-ui.html

#helps with error messages
#https://shiny.rstudio.com/articles/validation.html

#persistent data storage
#http://shiny.rstudio.com/articles/persistent-data-storage.html

#------- START OF CODE --------
#sample image we are currently using 
sample <-readImage('https://www.k9rl.com/wp-content/uploads/2016/08/Pembroke-Welsh-Corgi.jpg')

#------- UI PAGE ---------
ui <- fluidPage(

  titlePanel("Shiny Image"), 

  sidebarLayout(
    sidebarPanel(

      #TODO: needs to be fixed so that when the upload image radio button is clikced
      #      the fileinput is shown 
      radioButtons("radio", label = ("Sample or Upload Image"), 
        choices = list("Sample" = 1, "Upload Image" = 2, "Upload Link" = 3), selected = 1),
      conditionalPanel(
        condition = "input.radio == 2",
        fileInput(inputId = 'file1',
          label = 'Upload Image',
          placeholder = 'JPEG, PNG, and TIFF are supported',
          accept = c(
            "image/jpeg",
            "image/x-png",
            "image/tiff",
            ".jpg",
            ".png",
            ".tiff"))
        ),
      conditionalPanel(
        condition = "input.radio == 3",
        textInput(inputId = "url1", 
          label = 'Upload URL', 
          placeholder = 'JPEG, PNG, and TIFF are supported',
          value = '')
      ),
        #supported types are jpeg, png, and tiff
      sliderInput("bright", "Increase/Decrease Brightness:", min = -1, max = 1, value = 0, step = 0.01),
      sliderInput("contrast", "Increase/Decrease Contrast:", min = 0, max = 10, value = 1, step = 0.1), 
      sliderInput("gamma", "Increase/Decrease Gamma Correction", min = 0, max = 50, value = 1, step = 0.5),
      tags$head(tags$style(HTML('#button1{background-color:red}'))),
      tags$head(tags$style(HTML('#button2{background-color:red}'))),
      actionButton("button1", "Undo"), 
      actionButton("button2", "Redo"), 
      actionButton("button3", "Reset"), 
      textOutput("help")
    ),

    mainPanel(
      plotOutput("plot1",
       click = "plot_click",
       dblclick = "plot_dblclick",
       hover = "plot_hover",
       brush = "plot_brush"
       ),

       textOutput("txt1"),

       plotOutput("plot2"),
       #imageOutput("try"),
       actionButton("keep", label = "Keep"),
       downloadButton("download1", label = "Download Image"),
       tags$head(tags$style(HTML('#download2{background-color:red}'))),
       tags$head(tags$style(HTML('#download3{background-color:red}'))),
       downloadButton("download2", label = "Download Image Log"),
       downloadButton("download3", label = "Download Image Current Settings"),
       #TODO fix action button 
       verbatimTextOutput("info")
     )
  )
)

#------ SERVER -------
server <- function(input, output, session) {
  obj <- 1 #used as counter for the keep function
#TO DO not working right now 
#creates a plot that changes with slider inputs for 
# -- gamma, contrast, and brightness
  fileInput <- reactive({

    #if the user clicks a different radio button
    #uploads another file
    #uploads another url
    #clicks the RESET button
    #they will reset the image in plot1
    input$radio
    input$file1
    input$url1
    input$button3
    
    #user clicked sample -- default
    if(input$radio == 1)
    {
      foto0 <-readImage('https://www.k9rl.com/wp-content/uploads/2016/08/Pembroke-Welsh-Corgi.jpg')
      return(foto0)
    }

    #user uploaded their own image
    if(input$radio == 2)
    {
      inFile <- input$file1
      if(is.null(inFile))
        return(NULL)

      #validate(need(!is.null(inFile)), "Must enter valid URL of a jpeg, png, or tiff")
      oldNames = inFile$datapath
      newNames = file.path(dirname(inFile$datapath), inFile$name)
      file.rename(from = oldNames, to = newNames)
      inFile$datapath <- newNames

      foto1 <- readImage(inFile$datapath)
      return(foto1)
    }

    #user uploaded link
    if (input$radio == 3)
    {
     validate(need(input$url != "", "Must type in a valid jpeg, png, or tiff"))

      foto2 <- readImage(input$url1)
      return(foto2)
    }
  })

  imageOutput <- reactive({
    updatedImage <- fileInput()
    validate(need(!is.null(fileInput()), "Must upload a valid jpeg, png, or tiff"))




    #keep function 
    #uses a counter
    #when the user clicks keep, it updates the obk so that they are equivalent
    #makes it so that the updated image isn't from a previous radio button
    if(input$keep == obj)
    {
      isolate({
        obj <<- obj+1
        p <- input$plot_brush
        cropped <- updatedImage
        updatedImage2 <- cropped[round(p$xmin, 1):round(p$xmax, 1),round(p$ymin, 1):round(p$ymax, 1),]  
        session$resetBrush("plot_brush")
        return(updatedImage2)
      })
    }
    else return(updatedImage)
  })

  output$plot1 <- renderPlot({
    display(imageOutput() ^ input$gamma * input$contrast + input$bright, method = "raster")
  })

  #text box to see variables
  output$help <- renderText({
    paste0("number of keep: ", input$keep, " obj: ", obj, " number of reset: ", input$button3)
    })

  #text to differentiate original vs cropped version
  output$txt1 <- renderText({
      "Cropped Version (Press Keep to save and update to cropped version)"
  })

  #creates the images for the cropped version
  #uses EBImage and brushing to give dimesions to the cropped function
  imageOutput2 <- reactive({
    p <- input$plot_brush
    cropped <- imageOutput()
    updatedImage2 <- cropped[round(p$xmin, 1):round(p$xmax, 1),round(p$ymin, 1):round(p$ymax, 1),]    
    return(updatedImage2)
  })

  #creates plot
  #also fixes error messages
  output$plot2 <- renderPlot({
      validate(need(input$plot_brush != 'NULL', "Highlight a portion of the photo to see a cropped version!"))

      p <- input$plot_brush
      cropped <- imageOutput()

      validate(need(p$xmax <= dim(sample)[1], "Highlighted portion is out of bounds on the x-axis of your image 1"))
      validate(need(p$ymax <= dim(sample)[2], "Highlighted portion is out of bounds on the y-axis of your image 1"))
      validate(need(p$xmin >= 0, "Highlighted portion is out of bounds on the x-axis of your image 2"))
      validate(need(p$ymin >= 0, "Highlighted portion is out of bounds on the y-axis of your image 2"))

      display(imageOutput2() ^ input$gamma * input$contrast + input$bright, method = "raster")
  })



#updates slider with new file input from user or change in radio buttons
  observe({
    #if user clicks a new radio button, uploads new file, or url
    #the sliders will change
    #and the brush will default 
    input$file1
    input$url1
    input$radio
    
    updateSliderInput(session, "bright", value = 0)
    updateSliderInput(session, "contrast", value = 1)
    updateSliderInput(session, "gamma", value = 1)
    session$resetBrush("plot_brush")
  })

#TODO: if user inputs a photo with pre-set sliders; it needs to change to those values 
#needs to undo crop
    observe(
    {
      input$button3

      updateSliderInput(session, "bright", value = 0)
      updateSliderInput(session, "contrast", value = 1)
      updateSliderInput(session, "gamma", value = 1)
      session$resetBrush("plot_brush")
    })

#TODO: if user uploads a .png file, should return same type of file
#currently only returns jpeg because all three radio buttons dont have an image type
  output$download1 <- downloadHandler('temp.jpeg', function(file) {
    writeImage(imageOutput()^ input$gamma * input$contrast + input$bright, file)
  })

#gives information about brushing 
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }

    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  })
}

shinyApp(ui, server)
