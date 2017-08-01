
# ShinyImage

Imaging package, with an emphasis on *journaling*, i.e.  recording
history of changes.  Undo/redo operations, ability to display multiple
versions (currently under construction), etc.  The history is
persistent, i.e. across sessions.  Can be run from the R command line,
or from a Shiny-based GUI.

## Installation

You will need the following packages for the command-line interface to the
package:

<UL>

<li> 
<b>ffwtools</b>:  Install from CRAN, except for Linux; for the latter,
  sse 
<a href="#Linux">these special instructions.</a> 
</li> </p> 

<li>
<b>EBImage</b>:  Run these commands from within R:
</p>

```R
source("http://bioconductor.org/biocLite.R", verbose = FALSE) #Install package
biocLite("EBImage", suppressUpdates=TRUE, suppressAutoUpdate=FALSE, ask = FALSE)
```
</li> </p> 

<li>
<b>R6</b>: Install from CRAN. 

```R
# and if you would like to use the GUI, run the command
# below as well
install.packages(c('shiny','shinydashboard'))
```
</li> </p> 

</UL>

Having done this, you can install ShinyImage.  For instance, download
the **.zip** package available [here](//github.com/matloff/ShinyImag) and
unpack it, creating a directory/folder **ShinyImage-master**.  Then from a
terminal window, run 

```
R CMD build ShinyImage-master
R CMD INSTALL -l z ShinyImage_0.1.0.tar.gz
```

with __z__ being the location you wish to install ShinyImg to
(changing the version number as necessary).

## Example Usage

Here we will perform several actions, both to illustrate some ShinyImage
operations and also to show the journaling.  All operations will use the
R command line; examples of the GUI are given later in this document.

```R
# load image, whether local file or from the Web
> tiger <- 
   shinyimg$new("https://upload.wikimedia.org/wikipedia/commons/1/1c/Tigerwater_edit2.jpg")

# 'tiger' is an object of class 'shinyimg', which in turn is a subclass
# of 'R6'

# crop the image
> tiger$crop()
[1] "Select the two opposite corners of a rectangle on the plot."
# add contrast
> tiger$add_contrast()
# remove contrast
> tiger$remove_contrast()
# add contrast again
> tiger$add_brightness()
# remove contrast, again
> tiger$remove_brightness()

# we have had five actions, and can undo the last 4 of them, say,
# by calling undo four times
> tiger$undo()
> tiger$undo()
> tiger$undo()
> tiger$undo()
# we are now back at the cropped image
# we can also redo the adding of contrast
> tiger$redo()
# and finally, display the image
> tiger$render()

# we can also save the image to edit later on
> tiger$save("tiger-water.si")
# and later we can come back after a cold boot to do:
> tiger <- shinyload("tiger-water.si")
# if you want to revert to a previous saved state, you can also do:
> tiger$load("tiger-water.si")
# this will load the image back to the state it was in when you saved the image.
> tiger$undo()  # not too late to undo changes made before the save!
```

## GUI Installation and Usage

### Installation

Download from CRAN:

```R
install.packages(c('shiny','shinydashboard'))
```

### Usage

```R
# A gui can also be spawned to edit images using the sample provided or a user can upload an image, link, or an image log of a .si object created through shinyimg
# A user can edit brightness, contrast, and gamma correction. The user can also rotate, blur, and crop an image. These changes can be made to the image using the sliders. In order to crop a photo, the user has to highlight a box over the original plot. A preview of the cropped image with pop up below the original image. To keep the cropped image, click the keep button which will pop up below the preview image.  
# While editing an image, a user can undo, redo, or reset the image. These actions are executed through buttons at the bottom of the sidebar. 
# After editing an image, a user can download the image and the image log. These actions are below the main plot. 
# The user can also view the image log to see which actions were recorded. 

# The user's working directory must be set to ShinyImage/R
> shiny::runApp('app19.R')

## Documentation 

```R
> ?shinyimg  # the various operations, e.g. crop(), are described here

```

<h3>
<a name="Linux">Installing ffwtools on Linux </a> 
</h3>

<UL>

<li> Download http://www.fftw.org/fftw-3.3.6-pl1.tar.gz
</li> </p> 

<li> Unpack, say to x/fftw-3.3.6-pl1 and from that directory run
</p>

<pre>
./configure --prefix=y --enable-shared=yes 
</pre>

<p>
where <b>y</b> is your desired installation directory for <b>ffwtools</b>,
say <b>/usr/local</b>.
</li> </p>

<li> Run the usual <b>make; make install</b> sequence.
</li> </p>

<li> Set environment variables (no spaces around the = sign!):
</p>

<pre>
export C_INCLUDE_PATH=x/fftw-3.3.6-pl1/api 
export LD_RUN_PATH=y/lib 
export LIBRARY_PATH=y/lib 
</pre>
</li> </p>

<li> You may need to install <b>libtiff-dev</b>, say by 
</p>

<pre>
sudo apt-get install libtiff-deve
</pre>
</li> </p> 

<li> Then run the R steps as above.
</li> </p>

</UL>
