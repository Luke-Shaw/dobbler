################################################################################
#Storing the functions that make the card
################################################################################

make_card_tile = function(imgs, pieces = 1, num_random=0,
                          rotate=FALSE, resize=FALSE,...){
  #makes card with tile appraoch, also can take
  #the first num_random as a random selection for location, to mix it up

  if(rotate){imgs = lapply(imgs, rotate_quarter_turns)}
  if(resize){imgs = resize_big_med_small(imgs,...)}
  if(num_random==0){
    main_pic = make_base()
  } else {
    #note rotate=FALSE as we've already performed any rotations
    main_pic = make_card_random(imgs[1:num_random], rotate=FALSE)
  }

  for (img in imgs[ (num_random+1) : length(imgs) ]){
    loc = find_valid_loc_tile(mat = main_pic$mat,
                              height = magick::image_info(img)$height,
                              width = magick::image_info(img)$width,
                              pieces = pieces)
    if(loc[1]==FALSE){
      warning("tile approach has failed - maybe increase pieces variable?")
      return(main_pic)
    } else {
      main_pic = add_image(main_pic, img, loc)
      print(paste0("added image ",main_pic$num_imgs))}
  }
  return(main_pic)
}

make_card_random = function(imgs, rotate = TRUE, num_attempts = 1000){
  #makes a card made of all the images in the list of imgs by trying random coordinates
  #and seeing if there is the space to put the image. If num_attempts fails in a row
  #quits.
  set.seed(1)
  main_pic = make_base()

  for (img in imgs){
    if(rotate){
      img = rotate_quarter_turns(img)
    }
    a = FALSE
    b = 0
    n = img
    #find a valid location
    while (a == FALSE){
      loc = round(c(magick::image_info(img)$width, magick::image_info(img)$height) * runif(2)) #random location guess
      b = b + 1
      a = check_valid_loc(main_pic$mat,loc,
                          magick::image_info(img)$height,
                          magick::image_info(img)$width)
      if(b==num_attempts){
        warning("can't find a location through random: giving up")
        a = "issue"
      }
    }
    if(a!="issue"){
      main_pic = add_image(main_pic, img, loc)}
  }

  return(main_pic)
}

#commented out until needed - other methods are so quick currently not an issue
# make_card_template = function(imgs){
#   #makes a card made of all the images in the list of imgs by using a
#   #predefined template
#   #rescales images in the process
#
#   main_pic = make_base()
#
#   if(length(imgs)>10){
#     warning("will only use the first 10 images")
#     main_pic$warnings = "over 10 images supplied - will only do first 10"
#   }
#
#   n = magick::image_info(main_pic$img)$height
#   template = data.frame(
#     loc_x = n/10,
#     loc_y = n/10,
#     rotate = 0,
#     size = n/20
#   )
#
#
#   return(main_pic)
# }
