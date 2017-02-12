# ShinyImg
Imaging package, with an emphasis on recording history of changes.

## Installation Errors
If you receive the error:
"fftwtools.c:28:18: fatalerror: fftw3.h: No such file or directory", download and install
from this link: http://micro.stanford.edu/wiki/Install_FFTW3
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
