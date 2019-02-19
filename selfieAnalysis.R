library(keras)
library(reticulate)
library(jpeg)
library(tidyverse)

cenv = "r-tensorflow"
envs = conda_list()$name
# make sure in the right env
source_condaenv = function(condaenv) {
  use_condaenv(condaenv)
  sourcer_cmd = paste0("source activate ", 
                       condaenv)
  system(sourcer_cmd)
}
source_condaenv(cenv)
# Sys.setenv(
#   "CONDA_PKGS_DIRS" = path.expand("~/.conda/pkgs"))

#download image data
#system("wget http://crcv.ucf.edu/data/Selfie/Selfie-dataset.tar.gz")
#system("tar -zxf Selfie-dataset.tar.gz")
<<<<<<< HEAD

homedir = "~/sandboxes/aiScratch/"
#homedir = paste(getwd(), "/", sep = "")
#homedir = "/home/bcaffo/sandboxes/aiScratch/"
#homedir = "/users/bcaffo/"


#imageDir = "/home/bcaffo/sandboxes/appianSleep/Selfie-dataset/images/"

imageDir = paste(homedir, "/Selfie-dataset/images/", sep ="")
covDatLoc = paste(homedir, "/Selfie-dataset/selfie_dataset.txt", sep = "")

trainDir = paste(homedir, "Selfie-dataset/trainDir", sep = "")
testDir = paste(homedir, "Selfie-dataset/testDir", sep = "")

ovalTrainDir = paste(homedir, "Selfie-dataset/trainDir/ovalTrainDir", sep = "")
roundTrainDir = paste(homedir, "Selfie-dataset/trainDir/roundTrainDir", sep = "")
heartTrainDir = paste(homedir, "Selfie-dataset/trainDir/heartTrainDir", sep = "")

ovalTestDir = paste(homedir, "Selfie-dataset/testDir/ovalTestDir", sep = "")
roundTestDir = paste(homedir, "Selfie-dataset/testDir/roundTestDir", sep = "")
heartTestDir = paste(homedir, "Selfie-dataset/testDir/heartTestDir", sep = "")

## Don't need to rerun these if you already ran them
dir.create(trainDir)
dir.create(testDir)
dir.create(ovalTrainDir)
dir.create(roundTrainDir)
dir.create(heartTrainDir)
dir.create(ovalTestDir)
dir.create(roundTestDir)
dir.create(heartTestDir)

#Age: baby, child, teenager, youth, middle age, senior. 
#Race: white, black, asian. 
#Face shape: oval, round, heart. 
#Facial gestures: smiling, frowning, mouth open, tongue out, duck face. 
#Hair color: black, blond, brown, red. 
#Hair shape: curly, straight, braid. 
#Accessories: glasses, sunglasses, lipstick, hat, earphone. 
#Misc.: showing cellphone, using mirror, having braces, partial face. 
#Lighting condition: harsh, dim. 

covDat = read.table(covDatLoc, stringsAsFactors = FALSE)
temp = "partial_faces is_female baby child teenager youth middle_age senior white black asian oval_face round_face heart_face smiling mouth_open frowning wearing_glasses wearing_sunglasses wearing_lipstick tongue_out duck_face black_hair blond_hair brown_hair red_hair curly_hair straight_hair braid_hair showing_cellphone using_earphone using_mirror braces wearing_hat harsh_lighting dim_lighting"
names(covDat) = c("id", "huh", str_split(temp, " ")[[1]])
rm(temp)

#merge in file locations
files = dir(imageDir)
filesFP = dir(imageDir, full.names = TRUE)
id = str_replace(files, ".jpg", "")
covDat = inner_join(data.frame(id = id, files = files, filesFP = filesFP, stringsAsFactors = FALSE), 
                    covDat, by = "id")

## Just the data where someone has oval, round or heart shaped face
## create a random variable and sort by it to make grabbing training
## and test sets easy
subDat = covDat %>% select(id, files, filesFP, oval_face, round_face, heart_face) %>%
  filter( (oval_face ==  1 & round_face == -1 & heart_face == -1) |
          (oval_face == -1 & round_face ==  1 & heart_face == -1) |
          (oval_face == -1 & round_face == -1 & heart_face ==  1)) %>%
  mutate(odr = runif(n())) %>% arrange(odr)
#table(covDat$oval_face, covDat$round_face, covDat$heart_face)

N = nrow(subDat)

trainSize = 1000; testSize = 500
#file.copy((subDat %>%  filter(oval_face == 1) %>% top_n(trainSize))$filesFP, ovalTrainDir )
#file.copy((subDat %>% filter(round_face == 1) %>% top_n(trainSize))$filesFP, roundTrainDir)
#file.copy((subDat %>% filter(heart_face == 1) %>% top_n(trainSize))$filesFP, heartTrainDir)

#file.copy((subDat %>%  filter(oval_face == 1) %>% slice((trainSize + 1) : (trainSize + testSize)))$filesFP, ovalTestDir )
#file.copy((subDat %>% filter(round_face == 1) %>% slice((trainSize + 1) : (trainSize + testSize)))$filesFP, roundTestDir)
#file.copy((subDat %>% filter(heart_face == 1) %>% slice((trainSize + 1) : (trainSize + testSize)))$filesFP, heartTestDir)



<<<<<<< HEAD
## all of the images have the same size
testImage = readJPEG(filesFP[16], native = TRUE)
## all of the iamges have the same size
#testImage = image_load(filesFP[16]) %>% image_to_array()
#summary(testImage)
imageDim = dim(testImage)


model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                input_shape = imageDim) %>%
  layer_max_pooling_2d(pool_size = c(2, 2))%>% 
  layer_flatten() %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
              loss = "binary_crossentropy",
              optimizer = optimizer_rmsprop(lr = 1e-4),
              metrics = c("acc")
          )
summary(model)


model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(lr = 1e-4),
  metrics = c("acc")
)



model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                input_shape = c(imageDim[1],imageDim[2], 3)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")


## building up the keras model
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                input_shape = c(imageDim[1],imageDim[2], 3)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")
=======
>>>>>>> 10a28ea4468dd602297a124498b7f91003c849b9

train_datagen = image_data_generator(rescale = 1/255)
validation_datagen = image_data_generator(rescale = 1/255)

train_generator = flow_images_from_directory(
  trainDir,
  train_datagen,
  target_size = imageDim[1 : 2],
  batch_size = 20,
  class_mode = "binary"
)

validation_generator = flow_images_from_directory(
  testDir,
  validation_datagen,
  target_size = imageDim[1 : 2],
  batch_size = 20,
  class_mode = "binary"
)


batch <- generator_next(train_generator)
str(batch)

epochs = 5

history <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = 100,
  epochs = epochs,
  validation_data = validation_generator,
  validation_steps = 50
)



## Some plotting commands
plot_jpeg = function(jpg, add=FALSE){
  res = dim(jpg)[2:1] # get the resolution, [x, y]
  if (!add){ # initialize an empty plot area if add==FALSE
    par(mar = rep(0, 4), oma = rep(0, 4))
    plot(1, 1, xlim=c(1,res[1]),
         ylim=c(1,res[2]), 
         asp=1, type='n',
         xaxs='i', yaxs='i', xaxt='n', yaxt='n',
         xlab='', ylab='',
         bty='n'
         )
  }
  rasterImage(jpg,1,1,res[1],res[2])
}
test = readJPEG(filesFP[16], native = TRUE)
plot_jpeg(test)



