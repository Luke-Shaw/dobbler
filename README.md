Pesonal project to create bespoke version of card game [dobble](https://www.asmodee.co.uk/featured-product/dobble/). 

*Please note* this is not in any way affilitated with the makers of dobble, it is my own personal code.


# Installation

``` r
if (!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("luke-shaw/dobbler")
```

# Usage

The main function is:
``` r
dobbler::make_deck()
```
which runs an example if no parameters are given. The parameters are:

* `path` location of images to be used in the deck.
* `save_loc` location for output images.
* `resize` TRUE/FALSE - whether to resize the images so that on a card different images are larger/smaller.
* `max_dim` the largest dimension in pixels of an image. The background image is 1000
* `pics_per_card` how many images to be on each card. If you have `n` images per card, you can have at most `n(n-1) + 1` cards.

# Issues 

The 2 most common issues are:
1. The images don't fill the card. A fix to this is to run the code for `make_deck` line-by-line, specifying `i` then running the code in the for loop to re-print the specific card with the issue
2. The images are too large to fit on the card. A fix to this is to make `max_dim` smaller, or to re-run with a different seed in the `make_card_random` function.

Happy coding :) 
