\name{contour3d}
\alias{contour3d}
\title{Draw an Isosurface, a Three Dimension Contour Plot}
\description{
  Uses rgl to render an isosurface computed by the marching cubes algorithm.
}
\usage{
   contour3d(f, level, x, y, z, mask = NULL, color = "white", alpha = 1, 
             add = FALSE, draw = TRUE, engine = "rgl", ...)
}
\arguments{
  \item{f}{a function of 3 arguments or a three dimensional array.}
  \item{level}{The level or levels at which to construct contour surfaces.}
  \item{x,y,z}{locations of grid planes at which values in \code{f} are
    measured or \code{f} is to be evaluated. Can be omitted if \code{f}
    is an array.}
  \item{mask}{a function of 3 arguments returning a logical array, a
    three dimensional logical array, or \code{NULL}.  If not
    \code{NULL}, only cels for which \code{mask} is true at all eight
    vertices are used in forming the contour.  Can also be a lsit of
    functions the same length as \code{level}.}
  \item{color}{color to use for the contour surface. Recycled to the
    length of \code{'levels'}. Can also be a function, of list of
    functions, of three arguments.  These are called for each level with
    three arguments, the coordiantes of the midpoints of the triangles
    making up the surface.  They should return a vector of colors to use
    for the triangles.}
  \item{alpha}{alpha channel level, a number between 0 and 1. Recycled to the
    length of \code{'levels'}.}
  \item{add}{logical; if \code{TRUE}, add to current \code{rgl} graph.}
  \item{draw}{logical; if \code{TRUE}, draw the results; otherwise,
    return contour triangles.}
  \item{engine}{character; currently "rgl" or "none"; for "none" the
    computed triangles are returned.}
  \item{...}{material and texture properties. See \code{rgl.material}
    for details.}
}
\details{
  Uses the marching-cubes algorithm, with adjustments for dealing with
  face and internal ambiguities, to draw the isosurface.
  See references for the details.
}
\references{
 Chernyaev E. (1995)  
 Marching Cubes 33: Construction of Topologically Correct Isosurfaces  
 \emph{Technical Report CN/95-17, CERN}

 Daniel Adler, Oleg Nenadic and Walter Zucchini (2003)
 RGL: A R-library for 3D visualization with OpenGL

 Lorensen W. and Cline H. (1987)  
 Marching Cubes: A High Resolution 3D Surface Reconstruction Algorithm  
 \emph{Computer Graphics} \bold{vol. 21, no. 4}, 163-169
 
 Nielson G. and Hamann B. (1992)
 The Asymptotic Decider: Resolving the Ambiguity in Marching Cubes  
 \emph{Proc. IEEE Visualization}  \bold{92}, 83-91
} 
\seealso{
  \code{\link[rgl]{rgl.triangles}}, \code{\link[rgl]{rgl.material}},
  \code{\link[rgl]{rgl.surface}}.
}
\examples{
  #Example 1: Draw a ball
  f <- function(x, y, z)x^2+y^2+z^2
  x <- seq(-2,2,len=20)
  contour3d(f,4,x,x,x)

  # ball with one corner removed.
  contour3d(f,4,x,x,x, mask = function(x,y,z) x > 0 | y > 0 | z > 0)

  # ball with computed colors
  w <- function(x,y,z) {
      v <- sin(x) + cos(2 * y) * sin(5 * z)
      r <- range(v)
      n <- 100
      i <- pmax(pmin(ceiling(n * (v - r[1]) / (r[2] - r[1])), n), 1)
      terrain.colors(n)[i]
  }
  contour3d(f,4,x,x,x, color = w)

  #Example 2: Nested contours of mixture of three tri-variate normal densities
  nmix3 <- function(x, y, z, m, s) {
      0.4 * dnorm(x, m, s) * dnorm(y, m, s) * dnorm(z, m, s) +
      0.3 * dnorm(x, -m, s) * dnorm(y, -m, s) * dnorm(z, -m, s) +
      0.3 * dnorm(x, m, s) * dnorm(y, -1.5 * m, s) * dnorm(z, m, s)
  }
  f <- function(x,y,z) nmix3(x,y,z,.5,.5)
  g <- function(n = 40, k = 5, alo = 0.1, ahi = 0.5, cmap = heat.colors) {
      th <- seq(0.05, 0.2, len = k)
      col <- rev(cmap(length(th)))
      al <- seq(alo, ahi, len = length(th))
      x <- seq(-2, 2, len=n)
      contour3d(f,th,x,x,x,color=col,alpha=al)
      rgl.bg(col="white")
  }
  g(40,5)

\dontrun{
  #Example 3: Nested contours for FMRI data.
  library(AnalyzeFMRI)
  a<-f.read.analyze.volume(system.file("example.img", package="AnalyzeFMRI"))
  contour3d(aperm(a[,,,1],c(1,3,2)),1:64,1.5*(1:21),1:64,lev=3000,alpha=0.5)
  contour3d(aperm(a[,,,1],c(1,3,2)),1:64,1.5*(1:21),1:64,lev=8000,
            col="red",add=TRUE, alpha=0.5)
  contour3d(aperm(a[,,,1],c(1,3,2)),1:64,1.5*(1:21),1:64,lev=10000,
            col="green",add=TRUE)

  # alternative masking out a corner
  m <- array(TRUE, dim(a)[-4])
  m[1:30,1:30,1:10] <- FALSE
  contour3d(a[,,,1],1:64,1:64,1.5*(1:21),lev=3000, mask = m)
  contour3d(a[,,,1],1:64,1:64,1.5*(1:21),lev=8000, col="red",
            add=TRUE, mask = m)
  contour3d(a[,,,1],1:64,1:64,1.5*(1:21),lev=10000,col="green",
            add=TRUE, mask = m)
  }
}
\keyword{hplot}



