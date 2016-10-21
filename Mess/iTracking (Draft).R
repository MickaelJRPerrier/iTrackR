rm(list = ls())

# Fonctions
rep.row <- function(x, n){
  matrix(rep(x, each = n), nrow = n)
}
rep.col <- function(x, n){
  matrix(rep(x, each = n), ncol = n, byrow = TRUE)
}

##### iHeatmap #####

## SV Processing ##

# (cf., Vlad-Debusschere, R., Guyader, N., & Guérin-Dugué, A. (2015).
#       A bio-inspired model of central and peripheral vision for scene categorization)

# Fonction 2D #

# SV Processing
# contrast thresold(e) = A * (a / (a + e))
# A: constant related to the visualization distance (0 ≤ A ≤ 1)
# α: parameter controlling the decrease in contrast set to 2.3°
# e: eccentricity in degrees of visual angle at which the image is visualized

A <- 1
a <- 2.3
e <- seq(-90, 90, len = 180)
sv <- A * sqrt((a/(a + e^2))) # sqrt(e^2) in order to avoid a 'black hole'
plot(e, sv,
     type = "l",
     xlim = c(-90, 90),
     ylim = c(0, 1))


# Visual Acuity
# A: represents the level of visual acuity in foveal vision
# e: retinal eccentricity in degrees of visual angle
# b: parameter which has a value of 1=1:6

A <- 1
b <- 1/1.6
Ae <- A/(1 + b * sqrt((e)^2))
plot(e, Ae,
     type = "l",
     xlim = c(-90, 90),
     ylim = c(0, 1))


# Fonction 3D #
e2 <- seq(-90, 90, len = 180)
E1 <- rep.row(e, 180)
E2 <- rep.col(e2, 180)
SV <- A * sqrt((a / (a + (E1^2 + E2^2))))
persp(e, e2, SV,
      col = "lightgoldenrod",
      border = NA,
      theta = 30,
      phi = 15,
      ticktype = "detailed",
      ltheta = -120,
      shade = 0.25,
      xlim = c(-90, 90),
      ylim = c(-90, 90),
      zlim = c(0, 1))

# Fonction Gaussienne (Exemple) #
gauss <- exp(-((E1^2)/(2*sd(E1)) + (E2^2)/(2*sd(E2))))
persp(e, e2, gauss,
      col = "lightgoldenrod",
      border = NA,
      theta = 30,
      phi = 15,
      ticktype = "detailed",
      ltheta = -120,
      shade = 0.25,
      xlim = c(-90, 90),
      ylim = c(-90, 90),
      zlim = c(0, 1))
