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
  #pad <- padding(mat, 2)
  pad <- padding(mat, 8)
#   iterateur <- blocking(mat,2)
  iterateur <- 2*c(1:(nrow(pad)*ncol(pad)/2))-1
  res <- matrix(0,dim(pad)[1],dim(pad)[2])
  av <- (pad[iterateur]+pad[iterateur+1])/2
  res[iterateur] <- av
  res[iterateur+1] <- av
#   res[iterateur+dim(pad)[1]] <- av
#   res[iterateur+dim(pad)[1]+1] <- av
  return(res)
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
 # image <- blocking2(img,8)
  image <- mvdct(padding(img,8),2,FALSE)
  return(round(image))
}



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

quantification <- function(mat)
{
   Q<-array(10,dim(mat))
  
  return(trunc(mat/Q))
}

######
prétraitement<-function(img)
{
yuv<-trunc(Yuv(img))
final<-array(0,c(dim(padding(img[,,1],8)),dim(img)[3]))
final[,,2]<-average(yuv[,,2])
final[,,3]<-average(yuv[,,3])
final[,,1]<-convertDCT(yuv[,,1])
blocs<-blocking2(final[,,1],8)
blocs[1,]<-DPCM(blocs[1,])
blocs<-quantification(blocs)
final[,,1]<-blocs
#l<-list(c(hauteur(img),largeur(img),8,8,2,2),final[,,1],final[,,2],final[,,3])
return(final)
}
################################


codage <- function() {
  # supprimer le fichier JPEGfile.h5 s'il existe déjà
 # if (system(command = "ls | grep JPEGfile.h5") == 0) system(command = "rm JPEGfile.h5")
  
  # lecture de l'image à coder
  RGB <- readImage( file.choose()) * 255 ## conversion oblige
  
  L <- prétraitement( RGB ) # TODO
  # L est une liste contenant :
  #   les paramètres <H(hauteur img) , W(largeur img) , bh(hauteur bloc) , bw(largeur bloc) , rdown(sous-échantillonage ligne) , cdown(sous-échantillonage colonne)>,
  #   la matrice des blocs de la composante Y
  #   les matrices des composantes Cr et Cb sous-échantillonnées
  
  # création du fichier et de ces dossiers
  h5createFile( "JPEGfile.h5" )
  h5createGroup( "JPEGfile.h5" , "parameters" )
  h5createGroup( "JPEGfile.h5" , "Y_dct_q" )
  h5createGroup( "JPEGfile.h5" , "subCb_q" )
  h5createGroup( "JPEGfile.h5" , "subCr_q" )
  
#   param <- car( L )
#   Yblocs <- car( cdr( L ))
#   subCb <- car( cdr( cdr( L )))
#   subCr <- car( cdr( cdr( cdr( L ))))
#   # écriture des paramètres
#   h5write( param[1], "JPEGfile.h5" , "parameters/H" )
#   h5write( param[2], "JPEGfile.h5" , "parameters/W" )
#   h5write( param[3], "JPEGfile.h5" , "parameters/bh" )
#   h5write( param[4], "JPEGfile.h5" , "parameters/bw" )
#   h5write( param[5], "JPEGfile.h5" , "parameters/rdown" )
#   h5write( param[6], "JPEGfile.h5" , "parameters/cdown" )
#   # écriture des composantes Y, Cb et Cr
#   h5write( dim( Yblocs ) , "JPEGfile.h5" , "Y_dct_q/dim" )
#   h5write( Yblocs , "JPEGfile.h5" , "Y_dct_q/blocks" )
#   h5write( subCb , "JPEGfile.h5" , "subCb_q/image" )
#   h5write( subCr , "JPEGfile.h5" , "subCr_q/image" )
  # écriture des paramètres
  h5write( hauteur(RGB), "JPEGfile.h5" , "parameters/H" )
  h5write( largeur(RGB), "JPEGfile.h5" , "parameters/W" )
  h5write( 8, "JPEGfile.h5" , "parameters/bh" )
  h5write( 8, "JPEGfile.h5" , "parameters/bw" )
  h5write( 2, "JPEGfile.h5" , "parameters/rdown" )
  h5write( 2, "JPEGfile.h5" , "parameters/cdown" )
  # écriture des composantes Y, Cb et Cr
  h5write( dim( finale[,,1] ) , "JPEGfile.h5" , "Y_dct_q/dim" )
  h5write( finale[,,1] , "JPEGfile.h5" , "Y_dct_q/blocks" )
  h5write( finale[,,2] , "JPEGfile.h5" , "subCb_q/image" )
  h5write( finale[,,3] , "JPEGfile.h5" , "subCr_q/image" )  
  #     h5ls( "JPEGfile.h5" ) car il faut penser
  #   à libérer dès que possible de la mémoire
  rm( param , Yblocs , subCb , subCr )
}

decodage<-function ()
{
  param<-array(,6)
  # lecture des paramètres
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
  l<-list(param,Yblocs,subU,SubV)
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
  final[,,2]<-average(yuv[,,2])
  final[,,3]<-average(yuv[,,3]) 
}

