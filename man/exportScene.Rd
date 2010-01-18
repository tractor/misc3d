\name{exportScene}
\alias{exportScene}
\title{Writing Out Triangular Mesh Scenes}
\description{
  Writing out scenes consisting of one or more surfaces described by triangular
  mesh data structures to textual files.
}
\usage{
exportScene(scene, filename, format=c("OFF", "IDTF", "ASY"))
}
\arguments{
  \item{scene}{a triangle mesh object of class \code{Triangles3D} or a
    list of such objects representing the scene to be exported.}
  \item{filename}{the name of the exported textual file.}
  \item{format}{the format of the exported textual file.
    It must be one of "OFF", "IDTF", or "ASY" and can be abbreviated.
    The default is "OFF".}
}
\value{
  Textual files representing triangular mesh scenes.
}
\details{
  \code{exportScene} renders a scene consisting of one or more triangle
  mesh objects using standard or grid graphics.  Object-specific
  rendering features such as smoothing and material are controlled by
  setting in the objects.  Arguments to \code{exportScene} control global
  factors such as viewer and light position.

  \code{exportScene.rgl} renders the scene in an rgl window.

  If \code{add=TRUE} in standard or grid graphics then coordinates are
  not further scaled after the transformations implied by \code{R.mat},
  and \code{distance} are applied.  For the grid engine exporting occurs
  in the current viewport.
}
\note{
  The "rgl" engine now uses the standard rgl coordinates instead of
  negating \code{y} and swapping \code{y} and \code{z}. If you need to
  reproduce the previous behavior you can use
  \code{options(old.misc3d.orientation=TRUE)}.

  Transparency only works properly in the "rgl" engine.  For standard or
  grid graphics on devices that support transparency using alpha levels
  less than 1 does work but the triangle borders show as a less
  transparent mesh.
}
\seealso{\code{\link[rgl]{rgl.material}}}
\examples{
  vtri <- local({
      z <- 2 * volcano
      x <- 10 * (1:nrow(z))
      y <- 10 * (1:ncol(z))
      surfaceTriangles(x, y, z, color="green3")
  })
  exportScene(vtri, scale = FALSE)
  exportScene(vtri, screen=list(x=40, y=-40, z=-135), scale = FALSE)
  exportScene(vtri, screen=list(x=40, y=-40, z=-135), scale = FALSE,
            perspective = TRUE)
  exportScene(vtri, screen=list(x=40, y=-40, z=-135), scale = FALSE,
            perspective = TRUE, depth = 0.4)
}
\keyword{hplot}



