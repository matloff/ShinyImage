library(EBImage)
library(shiny)

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
      sliderInput("gamma", "Increase/Decrease Gamma Correction", min = 0, max = 50, value = 1, step = 0.5)
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
       downloadButton("downloadCrop", label = "Download Cropped"),
       #TODO fix action button 
       verbatimTextOutput("info")
     )
  )
)

#------ SERVER -------
server <- function(input, output, session) {

#TO DO not working right now 
#creates a plot that changes with slider inputs for 
# -- gamma, contrast, and brightness

  output$plot1 <- renderPlot({
    
    if(input$radio == 1)
    {
      display(sample ^ input$gamma * input$contrast + input$bright, method = "raster")
    }

    #TODO: FIX ERROR MESSAGE FOR URL 
    if(input$radio == 3)
    {
      #validate(
       # need(input$url != 'NULL', "Must have valid URL of type JPEG, PNG, or TIFF"))

      foto <- readImage(input$url1)
      display(foto ^ input$gamma * input$contrast + input$bright, method = "raster")
    }
    else 
    {
      inFile <- input$file1
      if(is.null(inFile))
        return(NULL)

#causing issues with upload image then sample it changes image to sample 
      oldNames = inFile$datapath
      newNames = file.path(dirname(inFile$datapath), inFile$name)
      file.rename(from = oldNames, to = newNames)
      inFile$datapath <- newNames
      foto <- readImage(inFile$datapath)

      display(foto ^ input$gamma * input$contrast + input$bright, method = "raster")
    }
  })

#text output
  output$txt1 <- renderText({
      "Cropped Version (Press Keep to save and update to cropped version)"
  })

#creates a plot that changes with above
# and cropping 
#TODO
#need to fix KEEP button and update cropped version to plot 1
  output$plot2 <- renderPlot({


#currently only catches two error messages
#TODO: fix both error messages popping up
      #p <- input$plot_brush

      validate(
      need(input$plot_brush != 'NULL', "Highlight a portion of the photo to see a cropped version!"))

      #validate(
      #need(exists(input$plot_brush), "Highlighted portion is out of the bounds of your image")


      #)

     if(input$radio == 1)
        {
          p <- input$plot_brush
          cropped <- sample ^ input$gamma * input$contrast + input$bright

          validate(
            need(p$xmax <= dim(sample)[1], "Highlighted portion is out of bounds on the x-axis of your image 1")
          )

          validate(
            need(p$ymax <= dim(sample)[2], "Highlighted portion is out of bounds on the y-axis of your image 1")
          )

          validate(
            need(p$xmin >= 0, "Highlighted portion is out of bounds on the x-axis of your image 2")
          )

          validate(
            need(p$ymin >= 0, "Highlighted portion is out of bounds on the y-axis of your image 2")
          )

          display(cropped[round(p$xmin, 1):round(p$xmax, 1),round(p$ymin, 1):round(p$ymax, 1),], method = "raster")        
        }
    if(input$radio == 3)
     {
            p <- input$plot_brush

            foto <- readImage(input$url1)


          validate(
            need(p$xmax <= dim(foto)[1], "Highlighted portion is out of bounds on the x-axis of your image 1")
          )

          validate(
            need(p$ymax <= dim(foto)[2], "Highlighted portion is out of bounds on the y-axis of your image 1")
          )

          validate(
            need(p$xmin >= 0, "Highlighted portion is out of bounds on the x-axis of your image 2")
          )

          validate(
            need(p$ymin >= 0, "Highlighted portion is out of bounds on the y-axis of your image 2")
          )


          cropped <- foto ^ input$gamma * input$contrast + input$bright
          display(cropped[round(p$xmin, 1):round(p$xmax, 1),round(p$ymin, 1):round(p$ymax, 1),], method = "raster")

     }

    else 
    {
      p <- input$plot_brush

      inFile <- input$file1
      if(is.null(inFile))
        return(NULL)

      oldNames = inFile$datapath
      newNames = file.path(dirname(inFile$datapath), inFile$name)
      file.rename(from = oldNames, to = newNames)
      inFile$datapath <- newNames
      foto <- readImage(inFile$datapath)


      validate(
        need(p$xmax <= dim(foto)[1], "Highlighted portion is out of bounds on the x-axis of your image 1")
      )

      validate(
        need(p$ymax <= dim(foto)[2], "Highlighted portion is out of bounds on the y-axis of your image 1")
      )

      validate(
        need(p$xmin >= 0, "Highlighted portion is out of bounds on the x-axis of your image 2")
      )

      validate(
        need(p$ymin >= 0, "Highlighted portion is out of bounds on the y-axis of your image 2")
      )

      cropped <- foto ^ input$gamma * input$contrast + input$bright
      display(cropped[round(p$xmin, 1):round(p$xmax, 1),round(p$ymin, 1):round(p$ymax, 1),], method = "raster")
    }
  })
  
#updates slider with new file input from user or change in radio buttons
  observe(
  {
    input$file1
    input$radio
    
    updateSliderInput(session, "bright", value = 0)
    updateSliderInput(session, "contrast", value = 1)
    updateSliderInput(session, "gamma", value = 1)
    session$resetBrush("plot_brush")
  })


  #output$try <- renderImage({
   #   readImage(output$plot2)
      #https://stackoverflow.com/questions/39175099/reading-objects-from-shiny-output-object-not-allowed
      #help make it reactive 
    #})
  #output$downloadCrop <- downloadHandler(
   # tempfile = tempfile("", , ".jpeg")
   # writeImage((cropped[round(p$xmin, 1):round(p$xmax, 1),round(p$ymin, 1):round(p$ymax, 1),], method = "raster"), )

    #}
    #)
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


#to do list 
#1. figure out how to brush (viewfinder + thing; highlight an area)
#3 figure out how to upload photo
#4. fix errors

#catch error messages --> translate them
#Error: non-numeric argument to mathematical function --> cropping function is unused
#Error: subscript out of bounds: cropping outside of the photo
#Error: only 0's may be mixed with negative subscripts
#when i click upload image --> reset slider buttons

#LOG EVERYTHING __ MOST IMPORTANT PART 
#get rid of error messages
#allow user to upload link

