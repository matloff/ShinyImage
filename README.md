# ShinyImage

Imaging package, with an emphasis on recording history of changes.

## Installation

The R library ffwtools is needed, so install it from CRAN.  This should be
straightforward on Macs or on Windows machine; for Linux see 
<a href="#Linux">these special instructions.</a> 

Now install EBImage.  Run these commands from within R:

<pre>
source("http://bioconductor.org/biocLite.R", verbose = FALSE) #Install package
biocLite("EBImage", suppressUpdates=TRUE, suppressAutoUpdate=FALSE, ask = FALSE)
</pre>

You will also need to install R6. 

Run this command from within R:

<pre>
install.packages("R6")
# And if you would like to use the GUI, run the commands
# below as well
install.packages("shiny")
install.packages("shinydashboard")
</pre>

Having done this, you can install ShinyImage.  For instance, download
the .zip package from https://github.com/matloff/ShinyImage and unpack it,
creating a directory/folder ShinyImage-master.  Then type 

<pre>
R CMD build ShinyImage-master
R CMD INSTALL -l z ShinyImage_0.1.0.tar.gz
</pre>
with z being the location you wish to install ShinyImg to
(changing the version number as necessary).

## Example Usage

Here we will perform several actions, both to illustrate some ShinyImage operations and also to show the journaling, i.e. version save/restore.

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
where y is your desired installation directory for ffwtools.  
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
