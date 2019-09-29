#lang scribble/manual
@require[@for-label[image-colors
                    2htdp/image
                    racket/base]]

@title{image-colors}
@author{thoughtstem}

@(require "../main.rkt" 2htdp/image animal-assets)

@defmodule[image-colors]

This is for doing basic color shifting transformations on @racket[image?] values.  It does this by converting them into bitmaps and operating on the pixels.  What is returned is a bitmap.  So keep that in mind if you are intending to do operations like @racket[scale] downstream of, say, a @racket[change-hue].  If you scale up after the hue shift, you'll see a blury image.  If you scale up before, the color shift will have to operate on more pixels.   

@defproc[(change-hue [amount from-0-to-360?] [img image?]) image?]{
  Shifts the image's hue by the given @racket[amount].

  @codeblock{
    #lang racket
    (require image-colors)

    (change-hue 50 (circle 40 'solid 'red))
  }

  Gives us:

  @(change-hue 50 (circle 40 'solid 'red))

  Or:
  
  @codeblock{
    #lang racket
    (require image-colors)
    (require animal-assets)

    (change-hue 50 chicken-sheet)
  }

  Gives us:

  @(change-hue 50 chicken-sheet)

  Whereas the original was:
  
  @chicken-sheet
}

@defproc[(tint-image [color color?] [img image?]) image?]{
  Overlays the given color onto non-transparent pixels of @racket[img].

  @codeblock{
    #lang racket
    (require image-colors)
    (require animal-assets)

    (change-hue 50 chicken-sheet)
  }

  Gives us:

  @(tint-image "red" chicken-sheet)

  Whereas the original was:
  
  @chicken-sheet
}

TODO: Test and document the other functions in @racket[main.rkt]
