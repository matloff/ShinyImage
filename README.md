# ShinyImg
Imaging package, with an emphasis on recording history of changes.

## Prerequisites

### Linux Specific
```ffwtools``` must be installed for this package to work. 
It can be found at this link:
https://cran.r-project.org/web/packages/fftwtools/index.html

Then follow the instructions below,

### Windows Specific
No additional steps are needed besides the steps below.

### Both Platforms
The following package must be installed in order to use EBImage, an image library.
In order to do so, run the commands:
```R
source("http://bioconductor.org/biocLite.R", verbose = FALSE) #Install package
biocLite("EBImage", suppressUpdates=TRUE, suppressAutoUpdate=FALSE, ask = FALSE)
```
Having done this, you can install ShinyImg.


## Example Usage

```R
> # Online images can be used
> tiger = img("https://upload.wikimedia.org/wikipedia/commons/1/1c/Tigerwater_edit2.jpg")
> # Local is fine also
> tiger_local = img("~/Tigerwater_edit2.jpg")
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
