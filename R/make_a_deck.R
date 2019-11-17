################################################################################
#Storing the functions that make the whole card deck
################################################################################

make_deck = function(path = './figures/example',
                     save_loc = './figures/outputs',
                     resize = TRUE,
                     max_dim = 200,
                     pics_per_card = 4){

  imgs = dobbler::images_read_from_folder(path)

  #resize (keeping aspect ratio) so that both length & width less than max_dim
  imgs = magick::image_resize(imgs,geometry =
                                paste0(as.character(max_dim),'x',as.character(max_dim)))
  imgs = as.list(imgs)

  combs = dobbler::get_image_combinations(pics_per_card)
  #remove any options where the card number is more than we have in imgs
  if(dim(combs)[1] < length(imgs)){
    warning(paste('There are more images in the folder than combinations - only first',
                  dim(combs)[1], 'images read will be used'))
    combs = combs[apply(combs,1,function(x) max(x) <= length(imgs)),1:pics_per_card]}
  #annoying edge case where default behaviour is to change 1d matrix into vector
  if(length(combs)==pics_per_card){ combs = matrix(combs,nrow=1)}


  incompletes = c()
  for (i in 1:dim(combs)[1]){
    temp_imgs = imgs[combs[i,]]
    #if resizing is going to happen, we randomize so it's not the same
    #image being made big (happens with first due to how combs is made)
    if(resize){temp_imgs = temp_imgs[sample(1:length(temp_imgs))]}
    card = make_card_tile(imgs = temp_imgs,
                          pieces=128,
                          resize = resize,
                          rotate=TRUE)
    magick::image_write(card$img, paste0(save_loc, '/image',i,'.png'))
    if(card$num_imgs < pics_per_card){
      incompletes = append(incompletes, i)
    }
    print(paste0('image written: ', save_loc, '/image',i,'.png'))
  }
  if(length(incompletes)){
    print(paste0(length(incompletes), " cards without full number of images: ",
                 paste(as.character(incompletes),collapse=" ")))
  } else {
    print(paste0('No cards without ',pics_per_card,' images on :)'))
  }
}



get_image_combinations = function(pics_per_card){
  #Gets the matrix for unique cards, using the way that Clare Sudbury creates
  #here https://medium.com/a-woman-in-technology/dobble-vision-7cd896a26671
  #NOTE: I couldn't find a pretty way of making it a formula so this is a
  #      brute force approach. Read the article for the intuition (it is
  #      a harder problem than it first seems!)

  if(pics_per_card<3){
    stop('pics_per_card must be at least 3 (1 and 2 are trivial but break the code)')
  }

  ppc = pics_per_card
  nc = ppc*(ppc-1) + 1 #num_cards or number of cards

  combs = matrix(0,nc, ncol=ppc)
  combs[1,] = 1:ppc #first card / first row is the intuitive one!
  combs[,1] = c(1, rep(1:ppc, each=ppc-1))

  #fill the rest of the cards that have "1" in. The second line is so matches link
  combs[2:ppc, 2:ppc] = (ppc+1): nc
  combs[2:ppc, 2:ppc] = t(combs[2:ppc, 2:ppc])

  #fill the rest of the cards that have "2" in, as is just transposed
  combs[(ppc+1):(2*ppc-1),2:ppc] = t(combs[2:ppc, 2:ppc])

  #now fill columnwise using all that have "2" in it as baseline
  r = (ppc+1):nc #rows
  for (col in 2:ppc){
    combs[r,col] = shift_right_append(v = combs[col,-1],
                                      p = col-2,
                                      times = ppc-1)
  }
  return(combs)
}

shift_right_append = function(v,p,times){
  #returns vector length length(v)*times with v as first length(v) elements
  #then iteratively applies shift_right to v
  temp = v
  for (i in 2:times){
    temp = shift_right(temp,p)
    v = c(v, temp)
  }
  return(v)
}

shift_right = function(v,p){
  #shifts a vector v by p positions, backfilling as it goes

  #checking values fine. WHY ON EARTH DOES R DO is.integer(1) = FALSE?
  stopifnot(p==floor(p), is.vector(v))

  p = p %% length(v) #shifting by length(v) is the same as doing nothing

  if(p!=0){
    temp = v
    #split into 2 steps: shifting everything to right first, then backfill using temp
    v[(p+1) : length(v)] = v[1:(length(v) -p)]
    v[1 : p] = temp[(length(v) - p + 1) : length(v)]
  }
  return(v)
}

images_read_from_folder = function(path){
  #reads the folder and returns the images within, giving a warning if there are
  #multiple file extensions and an error if image types invalid. Searches all
  #subfolders, too

  files = list.files(path, recursive=TRUE)
  #fails if the directory doesn't exist

  #the file extensions (everything after the dot)
  ext = sub("^[^.]*", "", files)
  ext = base::tolower(ext) #capitalisation doesn't matter in file extensions
  uniq_ext = unique(ext)
  if(length(uniq_ext)!=1){
    warning(paste('different file types in the folder', path))
  }

  #test for valid file types
  stopifnot(uniq_ext %in% c('.png', '.jpg','.svg'))

  #get each file path and read the image
  paths = paste(path, files, sep='/')
  imgs = magick::image_read(paths)

  return(imgs)
}
