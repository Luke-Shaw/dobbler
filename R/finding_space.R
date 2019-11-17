################################################################################
#Functions linked to finding a place in the pixel matrix where the image would
#fit
################################################################################

check_valid_loc <- function(mat,loc, height, width){
  #is it valid to add an image of dim width*height to
  #position loc (top left coord)

  #return FALSE if outside matrix dims
  if( (loc[2]+height-1)>dim(mat)[1] | (loc[1]+width-1)>dim(mat)[2]){
    return(FALSE)}

  #return FALSE if height / width invalid
  if(height == 0 | width == 0){
    return(FALSE)
  }

  #the as.matrix wrapper is because selecting one element auto-defaults to int, which
  #causes NULL when calling dim()
  sub = as.matrix(mat[seq(loc[2],loc[2] + height-1),
                      seq(loc[1],loc[1] + width-1)])
  if(dim(sub)[1] * dim(sub)[2] == sum(sub)){
    #ie all elements are TRUE
    return(TRUE)
  } else {
    return(FALSE)}
}

create_tiled_matrix = function(mat,x,y){
  #Given a matrix and a sub-rectangle size x*y, return the matrix of proportion of
  #space in each tile of size x*y, starting with top left

  y0 = dim(mat)[1]
  x0 = dim(mat)[2]
  stopifnot(y0/y== floor(y0/y), x0/x == floor(x0/x))

  #Create the empty matrix
  to_fill = matrix(0, nrow=y0/y, ncol=x0/x)
  #Each element points to an x*y rectangle in mat. Let's fill it!
  for (i in 1:dim(to_fill)[1]){
    for (j in 1:dim(to_fill)[2]){
      loc = c(x,y)*c(j-1,i-1)

      sub = mat[seq(loc[2],loc[2] + y-1),
                seq(loc[1],loc[1] + x-1)]
      to_fill[i,j] = sum(sub)/length(sub)
    }
  }
  return(to_fill)
}

find_factors <- function(x) {
  #finds all the positive factors of a number
  x <- as.integer(x)
  div <- seq_len(abs(x))
  factors <- div[x %% div == 0L]
  return(factors)
}

find_valid_loc_tile = function(mat, height, width,
                               pieces=1){
  #Searches for any unfilled tiles of size height*width in mat.
  #The variable "pieces" makes subtiling possible, and then combining to see if any
  #have the full gap. Default is the tile the size of the image

  #checking pieces is a power of 2 - based off defn of halving/quatering/... the tile
  stopifnot(log(pieces,2)%%1 == 0)

  div = log(pieces,2) + 1

  #divide through to effectively make a smaller image (that we'll subtile)
  y = height / div
  x = width / div
  y0 = dim(mat)[1]
  x0 = dim(mat)[2]

  #first deal with situation that shape doesn't tile nicely. The solution is to
  #increase offending dimension to the next integer that does
  if(y0/y != floor(y0/y)){
    y0_factors = find_factors(y0)
    y = min(y0_factors[y < y0_factors])
  }
  if(x0/x != floor(x0/x)){
    x0_factors = find_factors(x0)
    x = min(x0_factors[x < x0_factors])
  }

  #make the matrix with proportion of TRUEs in each tile
  tiled_matrix_small = create_tiled_matrix(mat,x,y)

  #now we search through the matrix for any clusters of ones
  tiled_matrix_small = tiled_matrix_small == 1
  #actually we don't need two matrices here as wouldn't get overwritten, but
  #worth it for understanding
  tiled_matrix = tiled_matrix_small
  for (i in 1:dim(tiled_matrix)[1]){
    for(j in 1:dim(tiled_matrix)[2]){
      tiled_matrix[i,j] = check_valid_loc(tiled_matrix_small,loc=c(j,i),
                                          height = div, width = div)
    }
  }
  #check for any non-taken patches
  gaps = which(tiled_matrix == 1, arr.ind = TRUE)
  if(length(gaps)!=0){
    #default is to take a random element
    r = round(runif(1,min=1,max=dim(gaps)[1]))
    choice = unname(gaps[r,]) - 1 #-1 as top left is (0,0) but (1,1) in matrix form
    loc = c(0,0)
    loc[1] = choice[2] * x
    loc[2] = choice[1] * y
  } else {
    loc = FALSE
  }
  return(loc)
}

plot_matrix = function(m){
  #plots the available space in the matrix as white and the unavailable as
  #black
  par(pty='s')
  image(t(m[nrow(m):1,] ), axes=FALSE, zlim=c(-4,4), col=c("black","white"))
}
