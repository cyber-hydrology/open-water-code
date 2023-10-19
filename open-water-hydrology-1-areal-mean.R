# install.packages("deldir")
library(deldir)

set.seed(2)
x <- runif(50)
y <- runif(50)
z <- rnorm(50)

tesselation <- deldir(x, y)
tiles <- tile.list(tesselation)

plot(tiles, pch = 19,
     border = "white",fillcol = hcl.colors(50, "Sunset"))


max(tesselation$summary$del.area)
which.max(tesselation$summary$del.area)

# Data
set.seed(1532)
x <- runif(5)
y <- runif(5)
z <- rnorm(5)

# Calculate Voronoi Tesselation and tiles
tesselation <- deldir(x, y)
tiles <- tile.list(tesselation)

plot(tiles, pch = 19,
     border = "white",fillcol = hcl.colors(5, "Sunset"))
z

tesselation$summary$del.area
tesselation$summary$del.area%*%z
z
tesselation$summary$del.area
mean(z)
tesselation$summary$del.area%*%z/sum(tesselation$summary$del.area)
