rm(list = ls())

##### iHeatmap #####

# (cf., Vlad-Debusschere, Guyader, & Guérin-Dugué, 2015)

# contrast thresold(e) = A * (a / (a + e))

# A: constant related to the visualization distance (0 ≤ A ≤ 1)
# α: parameter controlling the decrease in contrast set to 2.3°
# e: eccentricity in degrees of visual angle at which the image is visualized


# Calculer la taille de l'image en degré angulaire (α), longueur et largeur
# Créer un vecteur (e1) de même longueur que l'image (1 cellule = 1 pixel) allant de -α/2 à α/2
# Créer un vecteur (e2) de même largeur que l'image (1 cellule = 1 pixel) allant de -α/2 à α/2
# Utiliser les fonctions rep.row pour créer deux matrices (E1 & E2) de même taille que l'image
# Appliquer la fonction 'SV' pour chaque 'gaze'
# puis…
# 1) Créer une heatmap superposée à l'image ?
# 2) Modifier les valeurs des pixels de l'image ?


# Distance from the screen: 50 cm
# LCD 23", 1920 x 1080, 16:9


# v.0.1 (Stable)
iHeatmap1 = function(path) {
  
  if(missing(path)){
    stop("Your forgot to indicate where your image was.")
  }
  
  Distance <- function() {
    if (!exists('distance', envir = globalenv())) {
      distance <- readline(prompt = "Please inform the distance (cm) separating the participants from the screen: distance <- ")
      if(!grepl("^[0-9]+$", distance))
        return(Distance())
      assign('distance', value = as.integer(distance), envir = globalenv())
      }
  }
  Distance()
  
  Diagonal <- function() {
    if (!exists('diag_inch', envir = globalenv())) {
      diag_inch <- readline(prompt = "Please inform your computer's diagonal viewable size (inch): diag_inch <- ")
      if(!grepl("^[0-9]+$", diag_inch))
        return(Diagonal())
      assign('diag_inch', value = as.integer(diag_inch), envir = globalenv())
      }
  }
  Diagonal()
  
  Res.Width <- function() {  
    if (!exists('res_width', envir = globalenv())) {
      res_width <- readline(prompt = "Please inform your computer's width resolution (pixels): res_width <- ")
      if(!grepl("^[0-9]+$", res_width))
        return(Res.Width())
      assign('res_width', value = as.integer(res_width), envir = globalenv())
      }
  }
  Res.Width()
  
  Res.Height <- function() {
    if (!exists('res_height', envir = globalenv())) {
      res_height <- readline(prompt = "Please inform your computer's height resolution (pixels): res_height <- ")
      if(!grepl("^[0-9]+$", res_height))
        return(Res.Height())
      assign('res_height', value = as.integer(res_height), envir = globalenv())
      }
  }
  Res.Height()
  
  rep.row <- function(x, n){
    matrix(rep(x, each = n), nrow = n)
  }
  rep.col <- function(x, n){
    matrix(rep(x, each = n), ncol = n, byrow = TRUE)
  }
  
  
  diag_size <- diag_inch * 2.54 # en cm
  width_size <- diag_size / ((sqrt(res_width^2 + res_height^2)) / res_width) # Hypothénuse cm / (Hypothénuse pixels / Width pixels)
  height_size <- diag_size / ((sqrt(res_width^2 + res_height^2)) / res_height) # Hypothénuse cm / (Hypothénuse pixels / Height pixels)
  require(aspace)
  alphaW <- 2 * atan_d(width_size / (2 * distance)) # Width in angular degrees
  alphaH <- 2 * atan_d(height_size / (2 * distance)) # Height in angular degrees
  
  require(png)
  png = readPNG(path, native = T) # read the file
  res = dim(png)[2:1]
  
  A <- 1
  a <- 2.3
  e1 <- seq(-alphaW/2, alphaW/2, len = res[1])
  e2 <- seq(-alphaH/2, alphaH/2, len = res[2])
  E1 <- rep.row(e1, res[2])
  E2 <- rep.col(e2, res[1])
  SV <- A * sqrt((a / (a + ((E1^2) + (E2^2)))))
  
  require(plotly)
  plot_ly(z = SV) %>% add_surface()
}
iHeatmap1("~/Desktop/DATA/stimuli.png")


# v.0.2 (Stable)
iHeatmap2 = function(x, y, stim) {

  # Prerequisites #
  if(missing(x)){
    stop("You need to indicate the 'x' coordinates of gazes.")
  }
  
  if(missing(y)){
    stop("You need to indicate the 'y' coordinates of gazes.")
  }
  
  if(missing(stim)){
    stop("The 'stim' argument is missing. You need to indicate in which column the stimuli are. P.S.: The image files need to be in your working directory.")
  }

  Distance <- function() {
    if (!exists('distance', envir = globalenv())) {
      distance <- readline(prompt = "Please inform the distance (cm) separating the participants from the screen: distance <- ")
      if(!grepl("^[0-9]+$", distance))
        return(Distance())
      assign('distance', value = as.integer(distance), envir = globalenv())
    }
  }
  Distance()
  
  Diagonal <- function() {
    if (!exists('diag_inch', envir = globalenv())) {
      diag_inch <- readline(prompt = "Please inform your computer's diagonal viewable size (inch): diag_inch <- ")
      if(!grepl("^[0-9]+$", diag_inch))
        return(Diagonal())
      assign('diag_inch', value = as.integer(diag_inch), envir = globalenv())
    }
  }
  Diagonal()
  
  Res.Width <- function() {  
    if (!exists('res_width', envir = globalenv())) {
      res_width <- readline(prompt = "Please inform your computer's width resolution (pixels): res_width <- ")
      if(!grepl("^[0-9]+$", res_width))
        return(Res.Width())
      assign('res_width', value = as.integer(res_width), envir = globalenv())
    }
  }
  Res.Width()
  
  Res.Height <- function() {
    if (!exists('res_height', envir = globalenv())) {
      res_height <- readline(prompt = "Please inform your computer's height resolution (pixels): res_height <- ")
      if(!grepl("^[0-9]+$", res_height))
        return(Res.Height())
      assign('res_height', value = as.integer(res_height), envir = globalenv())
    }
  }
  Res.Height()
  
  
  # Functions #
  rep.row <- function(x, n){
    matrix(rep(x, each = n), nrow = n)
  }

  rep.col <- function(x, n){
    matrix(rep(x, each = n), ncol = n, byrow = TRUE)
  }

  require(aspace) # atand_d()

    
  # Screen/Viewer Characteristics #
  diag_size <- diag_inch * 2.54 # diagonal viewable size (cm)
  width_size <- diag_size / ((sqrt(res_width^2 + res_height^2)) / res_width) # hypotenuse (cm) / [hypotenuse (pixels) / width (pixels)]
  height_size <- diag_size / ((sqrt(res_width^2 + res_height^2)) / res_height) # hypotenuse (cm) / [hypotenuse (pixels) / height (pixels)]
  pix_size <- width_size / res_width # pixel size (cm)
  alpha_width <- 2 * atan_d(width_size / (2 * distance)) # width (arcdegrees)
  alpha_height <- 2 * atan_d(height_size / (2 * distance)) # height (arcdegrees)
  alpha_pix <- 2 * atan_d(pix_size / (2 * distance)) # width (arcdegrees)
  

  # Stimuli Characteristics #
  path = file.path(getwd(), as.character(stim))
  if(!exists('path')) {
    stop("Unable to find your images files. You should either move the files into the folder defined as your working directory, reset your working directory using 'setwd()', or check if the files' names into your variable are correct and writtent with the file extension.")
  }
  require(png)
  image = readPNG(path) # reads the file
  
  res = dim(image)[2:1]
  
  
  # SV Processing #
  A <- 1
  a <- 2.3
  e1 <- rep.row(seq(0, alpha_width, len = res[1]), res[2])
  e2 <- rep.col(seq(0, alpha_height, len = res[2]), res[1])
  SV <- A * sqrt((a / (a + (((e1 - (x * alpha_pix))^2) + ((e2 - (y * alpha_pix))^2)))))
  
  
  # 3D Plot #
  require(plotly)
  plot_ly(z = SV) %>% add_surface()
}
iHeatmap2(x = Data[5,]$MediaX, y = Data[5,]$MediaY, stim = Data[5,]$Stimulus)


# v.0.3 (Unstable)
iHeatmap3 = function(x, y, stim, data) {
  
  # Prerequisites #
  if(missing(x)){
    stop("You need to indicate the 'x' coordinates of gazes.")
  }
  
  if(missing(y)){
    stop("You need to indicate the 'y' coordinates of gazes.")
  }
  
  if(missing(stim)){
    stop("The 'stim' argument is missing. You need to indicate in which column the stimuli are. P.S.: The image files need to be in your working directory.")
  }
  
  Distance <- function() {
    if (!exists('distance', envir = globalenv())) {
      distance <- readline(prompt = "Please inform the distance (cm) separating the participants from the screen: distance <- ")
      if(!grepl("^[0-9]+$", distance))
        return(Distance())
      assign('distance', value = as.integer(distance), envir = globalenv())
    }
  }
  Distance()
  
  Diagonal <- function() {
    if (!exists('diag_inch', envir = globalenv())) {
      diag_inch <- readline(prompt = "Please inform your computer's diagonal viewable size (inch): diag_inch <- ")
      if(!grepl("^[0-9]+$", diag_inch))
        return(Diagonal())
      assign('diag_inch', value = as.integer(diag_inch), envir = globalenv())
    }
  }
  Diagonal()
  
  Res.Width <- function() {  
    if (!exists('res_width', envir = globalenv())) {
      res_width <- readline(prompt = "Please inform your computer's width resolution (pixels): res_width <- ")
      if(!grepl("^[0-9]+$", res_width))
        return(Res.Width())
      assign('res_width', value = as.integer(res_width), envir = globalenv())
    }
  }
  Res.Width()
  
  Res.Height <- function() {
    if (!exists('res_height', envir = globalenv())) {
      res_height <- readline(prompt = "Please inform your computer's height resolution (pixels): res_height <- ")
      if(!grepl("^[0-9]+$", res_height))
        return(Res.Height())
      assign('res_height', value = as.integer(res_height), envir = globalenv())
    }
  }
  Res.Height()
  
  
  # Functions #
  rep.row <- function(x, n){
    matrix(rep(x, each = n), nrow = n)
  }
  
  rep.col <- function(x, n){
    matrix(rep(x, each = n), ncol = n, byrow = TRUE)
  }
  
  require(aspace) # atand_d()
  
  
  # Screen/Viewer Characteristics #
  diag_size <- diag_inch * 2.54 # diagonal viewable size (cm)
  width_size <- diag_size / ((sqrt(res_width^2 + res_height^2)) / res_width) # hypotenuse (cm) / [hypotenuse (pixels) / width (pixels)]
  height_size <- diag_size / ((sqrt(res_width^2 + res_height^2)) / res_height) # hypotenuse (cm) / [hypotenuse (pixels) / height (pixels)]
  pix_size <- width_size / res_width # pixel size (cm)
  alpha_width <- 2 * atan_d(width_size / (2 * distance)) # width (arcdegrees)
  alpha_height <- 2 * atan_d(height_size / (2 * distance)) # height (arcdegrees)
  alpha_pix <- 2 * atan_d(pix_size / (2 * distance)) # width (arcdegrees)
  
  
  # Stimuli Characteristics #
  path = file.path(getwd(), as.character(stim))
  if(!exists('path')) {
    stop("Unable to find your images files. You should either move the files into the folder defined as your working directory, reset your working directory using 'setwd()', or check if the files' names into your variable are correct and writtent with the file extension.")
  }
  require(png)
  image = readPNG(path) # reads the file
  # if(get('error message') then('explain why'))
  
  res = dim(image)[2:1]
  
  
  # SV Processing #
  A <- 1
  a <- 2.3
  e1 <- rep.row(seq(0, alpha_width, len = res[1]), res[2])
  e2 <- rep.col(seq(0, alpha_height, len = res[2]), res[1])
  for(n in data[1:n, ]) {
    sv <- A * sqrt((a / (a + (((e1 - (x * alpha_pix))^2) + ((e2 - (y * alpha_pix))^2)))))
    SV <- sv
  }
  
  # 3D Plot #
  require(plotly)
  plot_ly(z = SV) %>% add_surface()
}
iHeatmap3(x = Data[1:2,]$MediaX, y = Data[1:2,]$MediaY, stim = Data[1:2,]$Stimulus)




choose.dir <- function() {
  system("osascript -e 'tell app \"RStudio\" to POSIX path of (choose folder with prompt \"Choose Folder:\")' > /tmp/R_folder",
         intern = FALSE, ignore.stderr = TRUE)
  p <- system("cat /tmp/R_folder && rm -f /tmp/R_folder", intern = TRUE)
  return(ifelse(length(p), p, NA))
}
setwd(choose.dir())


### À FAIRE ###

# – Pouvoir utiliser les coordonnées des X et Y en pixels dans le DF avec les degrés
# – for('n' rows), repeat 'x' * expr
# – Les 0 de X et Y doivent être dans le même coin
# – Faire que si la résolution de l'écran n'est pas sa résolution native il n'y ai pas de problème

#### References ####

# Ho-Phuoc, T. (2010). Développement et mise en œuvre de modèles d'attention visuelle. Thesis.
# Séré, B., Marendaz, C., & Hérault, J. (2000). Nonhomogenous resolution of images of natural scenes. Perception, 29, 1403-1412.
# Vlad-Debusschere, R., Guyader, N., & Guérin-Dugué, A. (2015). A bio-inspired model of central and peripheral vision for scene categorization.
