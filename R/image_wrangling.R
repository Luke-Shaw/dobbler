################################################################################
#Functions that manipulate or deal with the image side of things
################################################################################

make_base <- function(n=1000){
  ##makes the base image as a list containing:
  ##img      the base image (circle)
  ##mat      the base matrix of TRUE/FALSE for if valid to add a pixel there
  ##num_imgs the number of images added
  ##warnings if there are any warnings from compiling the image

  base <- magick::image_read("./figures/paint_circle.png")
  bckgrnd <- base %>%
#    image_transparent("white") %>%
#    image_background("blue") %>%
    magick::image_scale(as.character(n))
  mat <- outer(1:n,1:n,
               FUN=function(x,y) (x-n/2)^2 + (y-n/2)^2 < (19*n/40)^2)

  base <- list(img = bckgrnd,
               mat = mat,
               num_imgs = 0,
               warnings=NULL,
               sub_imgs_info = data.frame(loc_x=numeric(),
                                          loc_y=numeric(),
                                          width=numeric(),
                                          height=numeric()))
  return(base)
}

add_image <- function(main_pic, img, loc){
  #adds the img to main_pic$img with top-left coords loc, and updates main_pic.
  #main_pic is a list containing the image and the matrix of valid pixels.

  height = magick::image_info(img)$height
  width = magick::image_info(img)$width

  valid = check_valid_loc(main_pic$mat, loc, height, width)
  if(valid==FALSE){warning("will overwrite some of the image here")}

  #add the new image and make matrix FALSE in those places
  main_pic$img = main_pic$img %>%
    magick::image_composite(img,
                    offset=paste0("+",loc[1],"+",loc[2]))
  #this could be written more intuitively, but tracking with a small example
  #shows it works. Remember that loc is (x,y) top left coord of rectangle
  main_pic$mat[seq(loc[2],loc[2] + height-1),
               seq(loc[1],loc[1] + width-1)] = FALSE

  #add some parameters for info
  main_pic$num_imgs = main_pic$num_imgs + 1
  main_pic$sub_imgs_info[main_pic$num_imgs,] = c(loc[1],loc[2],width,height)

  return(main_pic)
}

rotate_randomly = function(img, fix_size=TRUE, flip_flop = TRUE){
  #randomly choose how much to rotate by and whether to flip/flop
  #Sometimes magick::image_rotate changes the size of the image, which is not
  #what we want - the fix is to fix the size. This can result in actual image
  #shrinking
  if(fix_size){
    width = magick::image_info(img)$width
  }

  deg = round(runif(1,0,360))
  if(flip_flop){
    #even when the user has chosen to flip or flop the image, we don't want to do it
    #always as then ever image on every card would be flipped & flopped - which is not
    #the randomness we want.
    flip = runif(1)>0.5
    flop = runif(1)>0.5
  } else {
    flip = 0
    flop = 0
  }
  #apply the manipulations
  img = magick::image_rotate(img, deg)
  if(flip){img = magick::image_flip(img)}
  if(flop){img = magick::image_flop(img)}
  #resize back to fixed if needed
  if(fix_size){img = magick::image_resize(img,as.character(width))}
  return(img)
}

rotate_quarter_turns = function(img){
  #randomly chooses how much to rotate by and whether to flip/flop,
  #within boundary of nearest 90degrees. This is because magick::imag_rotate
  #resizes the image, otherwise

  deg = sample(c(0,90,180,270),1)
  flip = runif(1)>0.5
  flop = runif(1)>0.5
  #apply the manipulations
  img = magick::image_rotate(img, deg)
  return(img)
}

resize_big_med_small = function(imgs, num_bms = NA){
  #resizes the given images into big / medium / small in amounts given by num_bms.
  #if num_bms is NA goes for 1 big 3 medium result small. Ratio between three is
  #1.5 : 1 : 0.75

  #warning("issue with image quality being lowered in this function")

  if(is.na(num_bms)){
    #define a default behaviour
    num_bms = c(1,3,length(imgs)-4)
    #if there are less than 4 images just leave same size
    if(length(imgs)<4){num_bms=c(0,length(imgs),0)}
  } else {
    #error checking that valid big / medium / small entered
    stopifnot(length(num_bms)==3,sum(num_bms)==length(imgs))
  }

  #take the size of the first image as our medium
  med = max(magick::image_info(imgs[[1]])$width,magick::image_info(imgs[[1]])$height)

  sizes = c(rep(med*1.5,num_bms[1]),
            rep(med*1,num_bms[2]),
            rep(med*0.75,num_bms[3]))

  imgs = mapply(function(x,y) magick::image_resize(x,as.character(y)),
                imgs, sizes)

  return(imgs)
}



