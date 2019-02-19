library(cloudml)
library(RoogleVision)
library(jsonlite)
#source("http://bioconductor.org/biocLite.R")
#biocLite("EBImage")
library(EBImage)
library(tidyverse)


#creds = fromJSON('credentials.json')

#options("googleAuthR.client_id" = "775032820080-i3t741tt7kbv6hogelet4v2chpfdpnni.apps.googleusercontent.com")
#options("googleAuthR.client_secret" = "9bt7ZfZr1LbLMUYsxlrLe6wZ")

#options("googleAuthR.client_id" = creds$installed$client_id)
#options("googleAuthR.client_secret" = creds$installed$client_secret)
#options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloud-platform"))
googleAuthR::gar_auth()

fileListPath = "Selfie-dataset/images/"
fileListFP = dir("Selfie-dataset/images", full.names = TRUE)
fileList = dir("Selfie-dataset/images", full.names = FALSE)

## This is a function to outline the face
outlineFace = function(imageFile){
  selfie = readImage(imageFile)
  test = getGoogleVisionResponse(imageFile, feature = 'FACE_DETECTION')
  head(test)
  xs1 = test$fdBoundingPoly$vertices[[1]][1][[1]]
  ys1 = test$fdBoundingPoly$vertices[[1]][2][[1]]
  xs2 = test$landmarks[[1]][[2]][[1]]
  ys2 = test$landmarks[[1]][[2]][[2]]
  
  plot(selfie)
  polygon(x=xs1,y=ys1,border='red',lwd=4)
  points(x=xs2,y=ys2,lwd=2, col='lightblue')
  
}


extractedFaceDir = "/home/bcaffo/sandboxes/aiScratch/Selfie-dataset-faces-extracted/"

## Loop over all of the files and perform facial recognition
out = lapply(fileListFP, function(imageFile){
  print(imageFile)
  selfie = readImage(imageFile)
  faceRecog = getGoogleVisionResponse(imageFile, feature = 'FACE_DETECTION')
  list(
    name = imageFile,
    faceRecog = faceRecog
    )
}
)
## benchmark the data
save(out, file = "out.Rdata")

## draws a plot for a specific file
outlineFace = function(i){
  imageFile = fileListFP[i]
  selfie = readImage(imageFile)
  faceRecog = out[[i]]$faceRecog
  head(faceRecog)
  xs1 = faceRecog$fdBoundingPoly$vertices[[1]][1][[1]]
  ys1 = faceRecog$fdBoundingPoly$vertices[[1]][2][[1]]
  xs2 = faceRecog$landmarks[[1]][[2]][[1]]
  ys2 = faceRecog$landmarks[[1]][[2]][[2]]
  
  plot(selfie)
  polygon(x=xs1,y=ys1,border='red',lwd=4)
  points(x=xs2,y=ys2,lwd=2, col='lightblue')
  
}


nameList = sapply(out, 
  function(x){
    unlist(strsplit(x$name, "/"))[3]
  } 
)

## If there's more than one face detected, return NA
detectionConfidence = sapply(out, function(x){
  rval = x$faceRecog$detectionConfidence
  nfaces = length(rval)
  if (is.null(rval)) rval = NA
  if (nfaces > 1) rval = NA
  rval
})


nfaces = sapply(out, function(x){
  length(x$faceRecog$detectionConfidence)
})


detectionConfidence = faceRecog$detectionConfidence
underExposedLikelihood = faceRecog$underExposedLikelihood
headwearLikelihood = faceRecog$headwearLikelihood

temp = order(detectionConfidence)






