# ShinyImg
Imaging package, with an emphasis on recording history of changes.

## Prerequisites
There are two prerequisite steps. The first is platform-dependent. The second step is the same for all platforms.
### 1a. Linux Specific
```ffwtools``` must be installed fpr this package to work. 
It can be found at this link:
https://cran.r-project.org/web/packages/fftwtools/index.html

Then follow the instructions in step 2.

### 1b. Windows Specific
No additional steps are needed besides step 2.

### 1c. Mac Specific
To do.

### 2. Installing EBImage
The following package must be installed in order to use EBImage, an image library.
In order to do so, run the commands:
```R
source("http://bioconductor.org/biocLite.R", verbose = FALSE) #Install package
biocLite("EBImage", suppressUpdates=TRUE, suppressAutoUpdate=FALSE, ask = FALSE)
```
Having done this, you can install ShinyImg.


## Installation Errors
If you receive the error:
"fftwtools.c:28:18: fatalerror: fftw3.h: No such file or directory", download and install
from this link: http://micro.stanford.edu/wiki/Install_FFTW3
 
## Example Usage

```R
> # Online images can be used
> tiger = shinyimg("https://upload.wikimedia.org/wikipedia/commons/1/1c/Tigerwater_edit2.jpg")
> # Local is fine also
> tiger_local = shinyimg("~/Tigerwater_edit2.jpg")
>
> # Crops the image. Will allow you to specify two points on the canvas that make up opposite ends of
> # the rectangular crop.
> tiger$crop()
[1] "Select the two opposite corners of a rectangle on the plot."
> # Adds contrast
> tiger$add_contrast()
> # Removes contrast
> tiger$remove_contrast()
> # Adds contrast
> tiger$add_brightness()
> # Removes contrast
> tiger$remove_brightness()
>
> # We have had four actions in contrast and brightness, and to undo them we call undo four times
> tiger$undo()
> tiger$undo()
> tiger$undo()
> tiger$undo()
> # We are now back at the cropped image.
> # We can also redo the adding of contrast
> tiger$redo()
> # And finally, display the image
> tiger$render()
```
