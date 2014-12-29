#source("http://bioconductor.org/biocLite.R")
#biocLite("rhdf5")

library("EBImage", lib.loc="~/R/x86_64-unknown-linux-gnu-library/3.1")
library("rhdf5", lib.loc="~/R/x86_64-unknown-linux-gnu-library/3.1")
img <- readImage("~/M1/MoPe/Projet/highlight.jpg")

#toolbox
###################################################
### utilitaires (fournis en TD) pour les images ###
###################################################

largeur <- function (I) dim(I)[1]
hauteur <- function (I) dim(I)[2]

#########################################################################
### Image memory allocation with dimension <Height X Width X nbComponent>
#########################################################################
createImage <- function ( Height, Width , nbComp = 3 ) { # default nbComlp = 3
  # constructs an ARRAY not a MATRIX
  array( numeric( Width*Height*nbComp ), c( Width, Height, nbComp ) )
}

####################################
### Read images from file
### Put magnitude in between [0,255]
####################################
lireImage <- function() {
  readImage( file.choose() ) * 255
}

###################################
### Display images of any magnitude
###################################
afficherImage <- function( I, title="Image" ) {
  display( normalization( I ) , title )
}

####################################
### normalization ; default in [0,1]
####################################
normalization <-function( I , newMin = 0 , newMax = 1 ) {
  if( length( dim( I ) ) == 3 )
    for( i in 1:dim(I)[3] ) {
      minI <- min( I[,,i] )
      maxI <- max( I[,,i] )
      ratio <- (newMax - newMin) / (maxI - minI)
      I[,,i] <- (I[,,i] - minI) * ratio + newMin
    }
  else {
    minI <- min( I )
    maxI <- max( I )
    ratio <- (newMax - newMin) / (maxI - minI)
    I <- (I - minI) * ratio + newMin
  }
  return( I )
}


### basic function

convertChannel <- function ( I , P ) { ## TD init R
}

### signe of numeric n (-1=neg ; +1=pos; 0 eitherwise)
sgn <- function(n) {
  if (n == 0) return (0)
  return(abs(n)/n)
}

### list toolsbox
cons <- function (v , l) { return (c(list(v),l)) }

car <- function (l) { return (l[[1]]) }

cdr <- function (l) {
  if (length(l)<2)
    return (list())
  return (l[2:length(l)])
}

estVide <- function (l) { length(l) == 0 }

### matrix toolbox
ones <- function (m, n) { matrixInit (1, m, n) }

zeros <- function (m=0, n=0) { matrixInit (0, m, n) }

matrixInit <- function (v, m, n) { matrix (rep (v, n*m), m, n) }

# Partie 1
Yuv <- function(img) {
  Y <- img[,,1] * 0.299 + img[,,2] * 0.587 + img[,,3] * 0.114
  u <- img[,,1] * -0.147 + img[,,2] * -0.289 + img[,,3] * 0.436
  v <- img[,,1] * 0.615 + img[,,2] * -0.515 + img[,,3] * -0.1
  mat <- array(0,c(dim(img)[1],dim(img)[2],3))
  mat[,,1] <- Y
  mat[,,2] <- u
  mat[,,3] <- v
  return(mat)
}

# Partie uv

padding <- function(img, val){
  if(dim(img)[1]%%val)
    padr <- dim(img)[1] + val-dim(img)[1]%%val
  else
    padr <- dim(img)[1]
  if(dim(img)[2]%%val)
    padb <- dim(img)[2] + val-dim(img)[2]%%val
  else
    padb <- dim(img)[2]
  img_finale <- matrix(0,padr,padb)
  img_finale[1:dim(img)[1],1:dim(img)[2]] <- img[]
  return(img_finale)
}

average <- function(mat) {
  pad <- padding(mat, 2)
  iterateur <- blocking(mat,2)
  res <- matrix(0,dim(pad)[1],dim(pad)[2])
  av <- (pad[iterateur]+pad[iterateur+1]+pad[iterateur+dim(pad)[1]]+pad[iterateur+dim(pad)[1]+1])/4
  res[iterateur] <- av
  res[iterateur+1] <- av
  res[iterateur+dim(pad)[1]] <- av
  res[iterateur+dim(pad)[1]+1] <- av
  return(res)
}

# Partie Y

blocking <- function(img, size) {
pad <- padding(img, size)
 iterateur <- matrix((size*c(1:(dim(pad)[1]*dim(pad)[2]/size))-(size-1)),dim(pad)[1],dim(pad)[2])[1:(dim(pad)[1]/size),1:(dim(pad)[2]/size)]
return(iterateur)
}

#DC  /OK
blocking <- function(img, size) {
  pad <- padding(img, size)
  return(matrix(pad,size,ncol(pad)*nrow(pad)))
  #return(matrix(pad,(size*size),(dim(pad)[1]*dim(pad)[2])/(size*size)))
}


#dc OK
# blocking2<- function(mat,size)
# {
#   pad <- padding(mat, size)
#   resultat<-matrix(,0,0)
#   for (i in seq(1,nrow(pad),size))
#        for(j in seq(1,ncol(pad),size))
#           resultat<-c(resultat,pad[i:(i+(size-1)),j:(j+(size-1))])
#   return(matrix(resultat,size*size,(ncol(pad)*nrow(pad)/(size*size))))
# }

blocking2<- function(mat,size)
{
  pad <- padding(mat, size)
  resultat<-matrix(,0,0)
  zigzag<-zigzag(size)
  for (i in seq(1,nrow(pad),size))
    for(j in seq(1,ncol(pad),size))
      resultat<-c(resultat,pad[i:(i+(size-1)),j:(j+(size-1))][zigzag])
  return(matrix(resultat,size*size,(ncol(pad)*nrow(pad)/(size*size))))
}

convertDCT <- function(img){
  image <- blocking2(img,8)
  image <- mvdct(image,2,FALSE)
  return(image)
}

#dc
View(blocking2(mat,8)[1,])
#ac
View(blocking2(mat,8)[2:nrow(blocking2(mat,8)),])

#DPCMTest<-function(mat)
#{
#dpcm<-(matrix(c(mat[1],(mat[2:(nrow(mat)*ncol(mat))]-mat[1:(nrow(mat)*ncol(mat)-1)])),nrow(mat),ncol(mat)))  
#return(dpcm)
#}
DPCM<-function(mat)
{
#   dpcm<-c(mat[1],(mat[2:(nrow(mat)*ncol(mat))]-mat[1:(nrow(mat)*ncol(mat)-1)]))  
  dpcm<-c(mat[1],(mat[2:length(mat)]-mat[1:(length(mat)-1)]))
  return(dpcm)
}

zigzag<-function(size)
{ num<-1
  resultat<-array(0,size*size)
  for(i in 1:size) 
    {
         for(j in 1:abs(size-abs(size-i))) 
           {
           if(i%%2 == 0)
            {
              resultat[num]<-(j-1)*size+(abs(size-abs(size-i+j-1)))
#               print(j)
#               resultat[num,2]<-(abs(8-abs(8-i+j-1)))
#               print(abs(8-abs(8-i+j-1)))
            }
           else
            {
              resultat[num]<-((abs(size-abs(size-i+j-1)))-1)*size+j
#               print(j)
#               resultat[num,1]<-(abs(8-abs(8-i+j-1)))
#               print(abs(8-abs(8-i+j-1)))
            }
            num<-num+1
         }
    }
  for(i in 1:(size-1)) 
    {
        for(j in abs(size-abs(size-i):1)) 
            {
              if(i%%2==1)
              {
                resultat[num]<-((abs(15-abs(size-i+j-1)))-1)*size+j+1
#                 print(abs(15-abs(8-i+j-1)))
#                 resultat[num,2]<-(j+1)
#                 print(j+1)
              }
              else
              {
                resultat[num]<-j*size+(abs((size*2-1)-abs(size-i+j-1)))
#                 print(abs(15-abs(8-i+j-1)))
#                 resultat[num,1]<-(j+1)
#                 print(j+1)
              }
              num<-num+1
            }
    
    }

  return(resultat)
}


