# ShinyImage

Imaging package, with an emphasis on *journaling*, i.e.  recording
history of changes.  Undo/redo operations, ability to display multiple
versions (currently under construction), etc.  The history is
persistent, i.e. across sessions.  Can be run from the R command line,
or from a Shiny-based GUI.

## Installation

The R library __ffwtools__ is needed, so install it from CRAN.  This should be
straightforward on Macs or on Windows machines; for Linux see 
<a href="#Linux">these special instructions.</a> 

Now install **EBImage**.  Run these commands from within R:

<pre>
source("http://bioconductor.org/biocLite.R", verbose = FALSE) #Install package
biocLite("EBImage", suppressUpdates=TRUE, suppressAutoUpdate=FALSE, ask = FALSE)
</pre>

You will also need to install **R6** from CRAN. 
Run this command from within R:

<pre>
install.packages('R6')
# and if you would like to use the GUI, run the command
# below as well
install.packages(c('shiny','shinydashboard'))
</pre>

Having done this, you can install ShinyImage.  For instance, download
the **.zip** package from __https://github.com/matloff/ShinyImag__ and
unpack it, creating a directory/folder **ShinyImage-master**.  Then from a
terminal window, run 

<pre>
R CMD build ShinyImage-master
R CMD INSTALL -l z ShinyImage_0.1.0.tar.gz
</pre>
with __z__ being the location you wish to install ShinyImg to
(changing the version number as necessary).

## Example Usage

Here we will perform several actions, both to illustrate some ShinyImage
operations and also to show the journaling. 

<pre>
# load image, whether local file or from the Web
> tiger = shinyimg$new("https://upload.wikimedia.org/wikipedia/commons/1/1c/Tigerwater_edit2.jpg")

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
# We can also save the image to edit later on:
> tiger$save("tiger-water.si")
# And later we can come back after a cold boot to do:
> tiger = shinyload("tiger-water.si")
# If you want to revert to a previous saved state, you can also do:
> tiger$load("tiger-water.si")
# This will load the image back to the state it was in when you saved the image. 
>
>
# A gui can also be spawned to edit images, either with a raw image file or an existing ShinyImage.
> editor_instance = shinygui$new()
# The following is using an instance of ShinyImage
> editor_instance$load(tiger)
# And this is using a raw image:
> editor_instance$load("https://upload.wikimedia.org/wikipedia/commons/1/1c/Tigerwater_edit2.jpg")
# The plus side of the former is that if the editor crashes, the image changes are all still saved in the "tiger" variable!
</pre>

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
where __y__ is your desired installation directory for __ffwtools__.  
</li> </p>

<li> Run the usual make; make install sequence.
</li> </p>

<li> Set environment variables:
</p>

<pre>
export C_INCLUDE_PATH = x/fftw-3.3.6-pl1/api 
export LD_RUN_PATH = ylib 
export LIBRARY_PATH = y/lib 
</pre>
</li> </p>

<li> Then run the R steps as above.
</li> </p>

</UL>
