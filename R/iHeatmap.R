#' iHeatmap
#'
#' @param x [TODO]
#' @param y [TODO]
#' @param stim [TODO]
#' @param data [TODO]
#'
#' @import aspace
#'         plotly
#'         png
#'   
#' @return [TODO]
#' @export
#'
#' @examples [TODO]

iHeatmap = function(x, y, stim, data) {
  
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