
<!-- README.md is generated from README.Rmd. Please edit that file -->

# webmorph

<!-- badges: start -->

<!-- badges: end -->

The goal of webmorph is to integrate with
[webmorph.org](https://webmorph.org).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("facelab/webmorph")
```

## Example

Load in all the tems from a directory. The code below loads images built
into {webmorph} from the CC-BY licensed [Young Adult Composite
Faces](https://doi.org/10.6084/m9.figshare.4055130.v1).

``` r
library(webmorph)
## basic example code

path <- system.file("extdata/composite", package = "webmorph")

tems <- read_tem(path)
```

You can plot an image with the `plot()` function.

``` r
plot(tems$f_multi, image = TRUE)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="50%" />

``` r
plot(tems,
     color = "blue",
     image = TRUE,
     nrow = 2)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

You can also visualise just the templates. If you omit the image and
don’t manually set a width and height, the x- and y-axis limit will be
set automatically.

``` r
plot(tems, 
    color = "#FF0000",
    pt.size = 2,
    pt.shape = 1,
    bg.fill = "black",
    image = FALSE,
    nrow = 2)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

You can average and visualise templates, as well.

``` r
system.file("extdata/london", package = "webmorph") %>%
  read_tem(images = FALSE) %>%
  average() %>%
  plot(pt.colour = "orchid", bg.fill = "grey30",
       width =1350, height = 1350)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="50%" />
