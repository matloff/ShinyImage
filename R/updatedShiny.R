library(shiny)
library(EBImage)
library(shinyjs)
library(dplyr)

validate((need(file.exists("imagehistory.R"), "Please input imagehistory.R into the same directory that contains updatedShiny.R")))
source('imagehistory.R')

#version 15
#purpose of this version is to integrate it with imagehistory.R

#sample, user inputs of url or image should create a new shinyimg object
#any observe event should be recorded as a shinyimg 

#TODO
#fix contrast -- currently contrast is not in sync with shiny img
#add gamma correction
#fix successive cropping -- cropping a cropped photo gives a different location 
#fix slider buttons after undo and redo -- if brightness is removed/contrast is removed -- the sliders should correlate
#fix slider buttons after image log has been uploaded
#save plot3 photo

#temporary
#currently stores responses in a folder in the current working directory
#titled tempdir

temp <- getwd()
responsesDir <- file.path(paste0(temp, "/tempdir"))

#creates an error message if user does not have tempdir directory
validate((need(file.exists("tempdir"), "Please create a directory named tempdir in your current working directory to save Image Log data")))

#fields that will be downloaded 
fieldsAll <- c("bright", "contrast", "gamma")

#helps create unique names to save the data 
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

#lists all the files that were previously saved in tempdir
#bids all the rows
#creates and saves the data
loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  data <- dplyr::bind_rows(data)
  data
}

#saves each change made to the photo 
#each change is a unique file 
saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv", 
            humanTime(),
            digest::digest(data))
  write.csv(x = data, file = file.path(responsesDir, fileName), 
        row.names = FALSE, quote = TRUE)
}

if (!require("shiny")) {
	cat("shiny is not installed. Please install it first.")
}

if (!require("EBImage")) {
	cat("EBImage is not installed. Please install it first.")
}

#needed to hide and show the keep button 
#if clicked accidentally, it causes errors
if (!require("shinyjs")) {
	cat("shinyjs is not installed. Please install it first.")
}

#used to bind all the rows
if (!require("dplyr")) {
	cat("shinyjs is not installed. Please install it first.")
}

# ______________________ start of UI _____________________
ui <- fluidPage( 
  shinyjs::useShinyjs(),

  titlePanel("Shiny Image"),

  sidebarLayout(
    sidebarPanel(
      radioButtons("radio", label = ("Sample or Upload Image"), 
        choices = list("Sample" = 1, "Upload Image" = 2, "Upload Link" = 3, "Upload Image Log" = 4), selected = 1),
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
      #********UPDATE*******
      conditionalPanel(
      	condition = "input.radio == 4", 
      	fileInput(inputId = 'file2', 
      		label = 'Upload .si file', 
      		placeholder = "Must be a ShinyImg Object", 
      		accept = c(
      			"file/si", 
      			".si"))
      ),
      sliderInput("bright", "Increase/Decrease Brightness:", min = -1, max = 1, value = 0, step = 0.01),
      sliderInput("contrast", "Increase/Decrease Contrast:", min = 0, max = 10, value = 1, step = 0.1), 
      sliderInput("gamma", "Increase/Decrease Gamma Correction", min = 0, max = 50, value = 1, step = 0.5),
      tags$head(tags$style(HTML('#button1{background-color:red}'))),
      tags$head(tags$style(HTML('#button2{background-color:red}'))),
      actionButton("button1", "Undo"), 
      actionButton("button2", "Redo"), 
      actionButton("button3", "Reset"), 
      textOutput("dimetext"), #dimensions of the picture 
      textOutput("help"),
      textOutput("idReader")
    ),
    mainPanel(
      HTML(
        paste(
          #h4('Main Image', align = "center"),
          plotOutput("plot1",
            click = "plot_click",
            dblclick = "plot_dblclick",
            hover = "plot_hover",
            brush = "plot_brush"
          ),
          h6('What the shinyimg object looks like; temporary (will become the main plot or in sync with the mainplot)'),
          plotOutput("plot3"),
          '<br/>',
          column(6, downloadButton("download1", label = "Download Image")),
          #column(6, downloadButton("download3", label = "Download Image Log")),
          #*******UPDATE******
          column(6, actionButton("download4", label = "Download Image Log")),
          '<br/>',
          tags$style(type='text/css', "#download1 { display: block; width:100%; margin-left: auto; margin-right:auto;}"),
          #tags$style(type='text/css', "#download3 { display:block; width:100%; margin-left: auto; margin-right:auto;}"),
          #*********UPDATE********
          tags$style(type='text/css', "#download4 { display:block; width:100%; margin-left: auto; margin-right:auto;}"),

          '<br/>',

          h4('Preview Crop', align = "center"),
          h6('Click and drag where you would like to crop the photo. To save the cropped version, press keep', align = "center"),
          '<br/>',
          #textOutput("txt1"),
          plotOutput("plot2"),
          tags$style(type='text/css', "#keep { display:block; width:10%; margin-left: auto; margin-right:auto;}"),
          '<br/>',
          shinyjs::hidden(
            actionButton("keep", label = "Keep")
          ),
          '<br/>',

          verbatimTextOutput("info")
        )
      )
    )
  )
)

# ______________________ start of server _____________________
server <- function(input, output, session) {
  imageFile <- reactiveValues(img_origin = NULL)
  #**********UPGRADE**********
  shinyImageFile <- reactiveValues(shiny_img_origin = NULL)

  current_image <- reactive({
    current_image <- shinyImageFile$shiny_img_origin$current_image
  })

  #currently the last valid file is displayed till either of the observeEvent conditions are met
  #e.g. user needs to click radio1, input a new file, or input a new URl
  #until they do so the last button clicked will be the original previous image
  #meaning that the brightness, contrast, gamma correction, and crop will not save

  #if they click radio button1 (sample), they will get the imageFile will be the color chart
  #TODO -- if other radio buttons are clicked; display nothing
  #currently everything is displayed
  observe({
  	if(input$radio == 1)
  	{
    	imageFile$img_origin <- readImage('http://www.pagetutor.com/common/bgcolors1536.png')
    	 #**********UPGRADE**********
    	shinyImageFile$shiny_img_origin <- shinyimg$new('http://www.pagetutor.com/common/bgcolors1536.png')
	}
  })

#********UPGRADE**********
  observeEvent(input$file2, {
  	shinyImageFile$shiny_img_origin <- shinyload(renameUpload(input$file2))
  	output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin$render())
  	#need to update sliders as well
  })

  #if they enter a new file, their file will become the new imageFile
  observeEvent(input$file1, {
    imageFile$img_origin <- readImage(renameUpload(input$file1))
    #**********UPGRADE**********
    shinyImageFile$shiny_img_origin <- shinyimg$new(renameUpload(input$file1))
  })

  #when user uploads file
  #the datapath is different from the one needed to properly recognize photo
  #so this function renames the file 
  renameUpload <- function(inFile) {
    if(is.null(inFile))
      return(NULL)

    oldNames = inFile$datapath
    newNames = file.path(dirname(inFile$datapath), inFile$name)
    file.rename(from = oldNames, to = newNames)
    inFile$datapath <- newNames

    return(inFile$datapath)
  }

  #if they enter a new url, their url will become the new new imageFile
  observeEvent(input$url1, {
    validate(need(input$url1 != "", "Must type in a valid jpeg, png, or tiff"))
    if (is.null(input$url1))
      imageFile$img_origin <- NULL
    else 
    {
    	imageFile$img_origin <- readImage(input$url1)
    	#**********UPGRADE**********
    	shinyImageFile$shiny_img_origin <- shinyimg$new(input$url1)
    }
  })

  #if user clicks button3 (reset), then we will make imageFile become the original file 
  #also resets brightness, contrast, and gamma correction
  #and resets plot_brush 
  observeEvent(input$button3, {
    if(input$radio == 1)
    {
      imageFile$img_origin <- readImage('http://www.pagetutor.com/common/bgcolors1536.png')   
      #**********UPGRADE**********
      shinyImageFile$shiny_img_origin <- shinyimg$new('http://www.pagetutor.com/common/bgcolors1536.png')   
    }
    if(input$radio == 2)
    {
      imageFile$img_origin <- readImage(renameUpload(input$file1))
      #**********UPGRADE**********
      shinyImageFile$shiny_img_origin <- shinyimg$new(renameUpload(input$file1))
    }
    if(input$radio == 3)
    {
      imageFile$img_origin <- readImage(input$url1)
      #**********UPGRADE**********
      shinyImageFile$shiny_img_origin <- shinyimg$new(input$url1)
    }

    updateSliderInput(session, "bright", value = 0)
    updateSliderInput(session, "contrast", value = 1)
    updateSliderInput(session, "gamma", value = 1)
    session$resetBrush("plot_brush")
  })

  #prompts shiny to look at recursive crop
  observe({
    recursiveCrop()
  })

  #only executes when keep is clicked 
  recursiveCrop <- eventReactive(input$keep,{
    imageFcn()
    isolate({
    	imageFile$img_origin <<- cropped(imageFile$img_origin)
    	p <- input$plot_brush #**********UPGRADE**********
    	shinyImageFile$shiny_img_origin$cropxy(p$xmin,p$xmax,p$ymin,p$ymax) #**********UPGRADE**********
    })
    session$resetBrush("plot_brush")
    #shinyjs::hide("keep")
    output$plot1 <- renderPlot(display(imageFile$img_origin ^ input$gamma * input$contrast + input$bright, method = "raster"))
    output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin$render()) #**********UPGRADE**********
  })
  
  #hides the keep button in the instance in which the user highlighted the plot
  #then clicks on the side so that the brush plot disappears
  #if user clicks keep without a valid brush, there will be errors
  #so we need to hide the button
  observeEvent(is.null(input$plot_brush), {
      shinyjs::hide("keep")
  })
  
  #returns image that is recursively updated
  imageFcn <- reactive ({
    return(imageFile$img_origin)
  })
  
  #function that crops the image based on the plot brush 
  cropped <- function(image)
  {
    p <- input$plot_brush
    return(image[p$xmin:p$xmax,p$ymin:p$ymax,])
  }

  output$plot1 <- renderPlot({
    display(imageFile$img_origin ^ input$gamma * input$contrast + input$bright, method = "raster")
  })

  #displays a cropped image of plot1's imageFile 
  output$plot2 <- renderPlot({
    p <- input$plot_brush
    validate(need(p != 'NULL', "Highlight a portion of the photo to see a cropped version!"))
    validate(need(p$xmax <= dim(imageFile$img_origin)[1], "Highlighted portion is out of bounds on the x-axis of your image 1"))
    validate(need(p$ymax <= dim(imageFile$img_origin)[2], "Highlighted portion is out of bounds on the y-axis of your image 1"))
    validate(need(p$xmin >= 0, "Highlighted portion is out of bounds on the x-axis of your image 2"))
    validate(need(p$ymin >= 0, "Highlighted portion is out of bounds on the y-axis of your image 2"))

    display(cropped(imageFile$img_origin) ^ input$gamma * input$contrast + input$bright, method = "raster")
    shinyjs::show("keep")
  })

  #**********UPGRADE**********
  output$plot3 <- renderPlot({
  	shinyImageFile$shiny_img_origin$render()
  })

  #**********UPGRADE**********
  observeEvent(input$bright, {
    shinyImageFile$shiny_img_origin$set_brightness(input$bright)
    output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin$render())
  })

  #**********UPGRADE**********
  #contrast numbers are different
  #contrast is not using the cropped image; it uses the original image 
  observeEvent(input$contrast, {
    shinyImageFile$shiny_img_origin$set_brightness(input$contrast-1)
    output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin$render())
  })

  #**********UPGRADE**********
  #undo button
  observeEvent(input$button1, {
  	shinyImageFile$shiny_img_origin$undo()

  	#slider inputs are not changing correctly becuase image history isn't affected by undo/redo
  	updateSliderInput(session, "bright", value = tail(shinyImageFile$shiny_img_origin$img_history, n = 1)[[1]]$brightness)
    updateSliderInput(session, "contrast", value = tail(shinyImageFile$shiny_img_origin$img_history, n = 1)[[1]]$contrast)
    updateSliderInput(session, "gamma", value = 1)
    output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin$render())

  })

  #**********UPGRADE**********
  #redo button 
  observeEvent(input$button2, {
  	shinyImageFile$shiny_img_origin$redo()

  	#slider inputs are not changing correctly becuase image history isn't affected by undo/redo
  	updateSliderInput(session, "bright", value = tail(shinyImageFile$shiny_img_origin$img_history, n = 1)[[1]]$brightness)
    updateSliderInput(session, "contrast", value = tail(shinyImageFile$shiny_img_origin$img_history, n = 1)[[1]]$contrast)
    updateSliderInput(session, "gamma", value = 1)
    output$plot3 <- renderPlot(shinyImageFile$shiny_img_origin$render())

  })

  #creates an ID for the image log 
  imgID <- reactive({
    if(input$radio == 1)
      id <- paste0("id1-sample")
    if(input$radio == 2)
      id <- paste0("id2-", input$file1[[1]])
    if(input$radio == 3)
      id <- paste0("id3-", input$url1)
    if(input$radio == 4)
      id <- paste("id4-prevSaved")
  return(id)
  })

  #textbox for user to see what the image ID is
  output$idReader <- renderText({
    paste0("ID: ", imgID())
  })

  #helpful text to show the user the crop function 
  #output$txt1 <- renderText({
  #    "Click and drag where you would like to crop the photo. To save the cropped version, press keep."
  #})

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

    #assuming this wouldn't be an issue for the shinyimg object
    #since the original image is the default
  })

  #creates the textbox below plot2 about the plot_brush details and etc
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

  #_________________DOWNLOAD ____________________
  #TODO: if user uploads a .png file, should return same type of file
  #currently only returns jpeg because all three radio buttons dont have an image type

  #allows user to download plot1 - imageFile
  output$download1 <- downloadHandler('temp.jpeg', function(file) {
    writeImage(imageFile$img_origin ^ input$gamma * input$contrast + input$bright, file)
  })

  #creates helpful text on sidebar about the dimensions 
  output$dimetext <- renderText({
    paste0("x axis: ", dime1(), " y axis: ", dime2())
  })

  #creates x dimensions for image log
  dime1 <- reactive({
    dim(imageFile$img_origin)[1]
  })

  #creates y dimensions for image log
  dime2 <- reactive({
    dim(imageFile$img_origin)[2]
  })  

  #DATA STORAGE SECTION
  #THE ABOVE SECTION IS THE IAMGE EDITOR
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(ID = imgID(), data, dimenX = dime1(), dimenY = dime2())
    data <- t(data)

    data #saves the data 
  })

  #this observes any changes to the image and saves 
  observe({
    saveData(formData())
  })

  #download button for image log
  #creates a file for the user to see the image log 
  output$download3 <- downloadHandler(
    filename = 'ImageLog.csv',
    content = function(file) {
      write.csv(loadData(), file, row.names = FALSE)
    }
  )

#*******UPDATE*********
  observeEvent(input$download4, {
  	shinyImageFile$shiny_img_origin$save()
  	#creates a pop up window 
  	showModal(modalDialog(
  		title = "Important message", 
  		"Check your current directory for workplace.si for Image History!"))
  })

}

shinyApp(ui, server)


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
#http://deanattali.com/2015/06/14/mimicking-google-form-shiny/#save

#download button -- input
#https://stackoverflow.com/questions/43663352/r-count-shiny-download-button-clicks

#themes
#https://shiny.rstudio.com/gallery/shiny-theme-selector.html

#recursion
#https://stackoverflow.com/questions/37128528/self-referencing-reactive-variables-in-shiny-r
#https://groups.google.com/forum/#!topic/shiny-discuss/8ouy0eS15Jc
#https://stackoverflow.com/questions/29716868/r-shiny-how-to-get-an-reactive-data-frame-updated-each-time-pressing-an-actionb
