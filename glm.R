library(tidyverse)    
library(keras)

dat = mtcars %>% mutate(highMPG = (mpg> 20) * 1)
y = matrix(dat$highMPG, ncol = 1)
x = as.matrix(dat %>% select(disp, wt))

model = keras_model_sequential() 
model %>% 
  layer_dense(units = 1, 
              input_shape = ncol(x),   
              activation = "sigmoid") %>% 
  compile(
    optimizer = 'rmsprop',
    loss = 'binary_crossentropy'
  ) %>%   
fit(x, y, epochs = 40)

p = model %>% predict_proba(as.matrix(x))
table(p, y)

p2 = predict(glm(I(as.vector(y)) ~ I(as.matrix(x)), family = binomial))
```
