makeTriangles <- function(v1, v2, v3, 
                          color = "red", color2 = NA, 
                          fill = TRUE, material = "default", alpha = 1,
                          col.mesh = if (fill) NA else "black",
	                  phong = 0) {
    if (missing(v2) || missing(v3)) {
        if (missing(v2) && missing(v3))
  	    v <- unzipTriangleMatrix(v1)
        else if (missing(v3))
            v <- ve2t(list(vb = v1, ib = v2))
        else stop("unknown form of triangle specification")
	v1 <- v$v1
	v2 <- v$v2
	v3 <- v$v3
    }
    structure(list(v1 = v1, v2 = v2, v3 = v3,
                   color = color, color2 = color2, fill = fill,
                   material = material, col.mesh = col.mesh, alpha = alpha,
                   phong = phong),
              class = "Triangles3D")
}

is.Triangles3D <- function(x) identical(class(x), "Triangles3D")

ve2t <- function(ve) {
    list (v1 = t(ve$vb[,ve$ib[1,]]),
          v2 = t(ve$vb[,ve$ib[2,]]),
          v3 = t(ve$vb[,ve$ib[3,]]))
}

unzipTriangleMatrix <- function(tris) {
    if (ncol(tris) != 3)
        stop("triangle matrix must have three columns.")
    if (nrow(tris) %% 3 != 0)
        stop("number of rows in triangle matrix must be divisible by 3")
    n <- nrow(tris) / 3
    list(v1 = tris[3 * (1 : n) - 2,],
         v2 = tris[3 * (1 : n) - 1,],
         v3 = tris[3 * (1 : n),])
}

zipTriangles <- function(tris) {
    n <- nrow(tris$v1)
    if (nrow(tris$v2) != n || nrow(tris$v3) != n)
        stop("vertex arrays must have the same number of rows")
    v <- matrix(0, nrow = 3 * n, ncol = 3)
    v[3 * (1 : n) - 2,] <- tris$v1
    v[3 * (1 : n) - 1,] <- tris$v2
    v[3 * (1 : n),] <- tris$v3
    v
}
