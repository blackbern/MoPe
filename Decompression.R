#decompression
# source("http://bioconductor.org/biocLite.R")
# biocLite("rhdf5")

# library("EBImage", lib.loc="~/R/x86_64-unknown-linux-gnu-library/3.1")
# library("rhdf5", lib.loc="~/R/x86_64-unknown-linux-gnu-library/3.1")
# img <- readImage("~/M1/MoPe/Projet/highlight.jpg")

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
RGB <- function(img) {
  R <- img[,,1] * 1 + img[,,2] * 0 + img[,,3] * 1.14
  G <- img[,,1] * 1 + img[,,2] * -0.395 + img[,,3] * -0.58
  B <- img[,,1] * 1 + img[,,2] * 2.032 + img[,,3] * 0
  mat <- array(0,c(dim(img)[1],dim(img)[2],3))
  mat[,,1] <- R
  mat[,,2] <- G
  mat[,,3] <- B
  return(round(mat))
}

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

##########
# Sur echantillonnage

Surechantillonnage <- function(mat,h,w) {
#   #pad <- padding(mat, 2)
#   pad <- padding(mat, 8)
#   #   iterateur <- blocking(mat,2)
#   iterateur <- 2*c(1:(nrow(pad)*ncol(pad)/2))-1
#   res <- matrix(0,dim(pad)[1],dim(pad)[2])
#   av <- (pad[iterateur]+pad[iterateur+1])/2
#   res[iterateur] <- av
#   res[iterateur+1] <- av
#   #   res[iterateur+dim(pad)[1]] <- av
#   #   res[iterateur+dim(pad)[1]+1] <- av
#   return(res)
  
  res<-matrix(0,dim(mat)[1]*2,dim(mat)[2])
  pad<-padding(res,2)
  iterateur <- 2*c(1:(nrow(pad)*ncol(pad)/2))-1
  res[iterateur]<-mat
  res[iterateur+1]<-mat
  return(res[1:h,1:w])
}

# Partie Y

blocking <- function(img, size) {
  pad <- padding(img, size)
  iterateur <- matrix((size*c(1:(dim(pad)[1]*dim(pad)[2]/size))-(size-1)),dim(pad)[1],dim(pad)[2])[1:(dim(pad)[1]/size),1:(dim(pad)[2]/size)]
  return(iterateur)
}

#DC  /OK
# blocking <- function(img, size) {
#   pad <- padding(img, size)
#   return(matrix(pad,size,ncol(pad)*nrow(pad)))
#   #return(matrix(pad,(size*size),(dim(pad)[1]*dim(pad)[2])/(size*size)))
# }



blockingInv<- function(mat,size,h,w)
{
  res<-padding(matrix(0,h,w),size)
  n<-1
  zigzag<-zigzag(size)
  for (i in seq(1,nrow(res),size))
    for(j in seq(1,ncol(res),size))
    {
      res[i:(i+(size-1)),j:(j+(size-1))][zigzag]<-mat[,n]
      n<-n+1
    }
  return(res[1:h,1:w])
}

convertDCTInv <- function(img){
  # image <- blocking2(img,8)
  image <- mvdct(img,2,TRUE)
  return(round(image))
}



DPCMInv<-function(mat)
{
  for(i in 2:length(mat))
    mat[i]<-mat[i]+mat[i-1]
  return(mat)
}


#################
# Zizg zag Inverse


zigzagInv<-function(size)
 { #num<-1
#   resultat<-array(0,size*size)
#   for(i in size:1) 
#   {
#     for(j in abs(size-abs(size-i)):1) 
#     {
#       if(i%%2 == 0)
#       {
#         resultat[num]<-(j-1)*size+(abs(size-abs(size-i+j-1)))
#          #              print(j-1)*size+(abs(size-abs(size-i+j-1)))
#         #              resultat[num,2]<-(abs(8-abs(8-i+j-1)))
#         #               print(abs(8-abs(8-i+j-1)))
#       }
#       else
#       {
#         resultat[num]<-((abs(size-abs(size-i+j-1)))-1)*size+j
#          #              print(((abs(size-abs(size-i+j-1)))-1)*size+j)
#         #               resultat[num,1]<-(abs(8-abs(8-i+j-1)))
#         #               print(abs(8-abs(8-i+j-1)))
#       }
#       num<-num+1
#     }
#   }
#   for(i in (size-1):1) 
#   {
#     for(j in 1:abs(size-abs(size-i))) 
#     {
#       if(i%%2==1)
#       {
#         resultat[num]<-((abs(15-abs(size-i+j-1)))-1)*size+j+1
#          #               print(abs(15-abs(8-i+j-1)))
#         #                 resultat[num,2]<-(j+1)
#         #                 print(j+1)
#       }
#       else
#       {
#         resultat[num]<-j*size+(abs((size*2-1)-abs(size-i+j-1)))
#         #                 print(abs(15-abs(8-i+j-1)))
#         #                 resultat[num,1]<-(j+1)
#         #                 print(j+1)
#       }
#       num<-num+1
#     }
#     
#   }
  return(c(1,2,6,7,15,16,28,29,3,5,8,14,17,27,30,43,4,9,13,18,26,31,42,44,10,12,19,25,32,41,45,54,11,20,24,33,40,46,53,55,21,23,34,39,47,52,56,61,22,35,38,48,51,57,60,62,36,37,49,50,58,59,63,64))
}

quantificationInv <- function(mat,h,w)
{
  Q<-array(600,dim(mat))
  Q[1:7,]<-1
  
  return(mat*Q)
}

#test
test<-function(mat)
{
  nb<-1
  l1<-seq(1,nrow(mat),8)
  l2<-seq(1,ncol(mat),8)
  res<-matrix(0,2,(length(l1)*length(l2)))
for (i in l1)
{
  for(j in l2)
  {
    res[1,nb]<-i
    res[2,nb]<-j
    nb<-nb+1
  }
}
return (res)
}

decodage<-function ()
{
  param<-array(,6)
  # lecture des parametres
  param[1]<-h5read(  "JPEGfile.h5" , "parameters/H" )
  param[2]<-h5read( "JPEGfile.h5" , "parameters/W" )
  param[3]<-h5read(  "JPEGfile.h5" , "parameters/bh" )
  param[4]<-h5read(  "JPEGfile.h5" , "parameters/bw" )
  param[5]<-h5read(  "JPEGfile.h5" , "parameters/rdown" )
  param[6]<-h5read(  "JPEGfile.h5" , "parameters/cdown" )
  # lecture des composantes Y, Cb et Cr
  dimYblocs<-h5read( "JPEGfile.h5" , "Y_dct_q/dim" )
  Yblocs<-h5read( "JPEGfile.h5" , "Y_dct_q/blocks" )
  subU<-h5read( "JPEGfile.h5" , "subCb_q/image" )
  subV<-h5read( "JPEGfile.h5" , "subCr_q/image" ) 
  l<-list(param,Yblocs,subU,subV)
  return(posttraitement(l))
}
posttraitement<-function(liste)
{
  #   yuv<-Yuv(img)
  #   final<-array(0,c(dim(padding(img[,,1],8)),dim(img)[3]))
  #   final[,,2]<-average(yuv[,,2])
  #   final[,,3]<-average(yuv[,,3])  
  #   final[,,1]<-convertDCT(yuv[,,1])
  #   blocs<-blocking2(final[,,1],8)
  #   blocs[1,]<-DPCM(blocs[1,])
  #   blocs<-quantification(blocs)
  #   final[,,1]<-blocs
  #   l<-list(c(hauteur(img),largeur(img),8,8,2,2),final[,,1],final[,,2],final[,,3])
  #   return(l)
  Y<-quantificationInv(liste[[2]])
  Y[1,]<-DPCMInv(Y[1,])
  Y<-blockingInv(Y,liste[[1]][3],liste[[1]][1],liste[[1]][2])
  Y<-convertDCTInv(Y)
  u<-Surechantillonnage(liste[[3]],liste[[1]][1],liste[[1]][2])
  v<-Surechantillonnage(liste[[4]],liste[[1]][1],liste[[1]][2])
  img<-array(c(Y,u,v),c(liste[[1]][1],liste[[1]][2],3))
  img<-RGB(img)/255
  
  return(Image(img,,Color))
}
