library(misc3d)

local({
    haveRGL <- suppressWarnings(require(rgl,quietly=TRUE))

    makeTriangles <- misc3d:::makeTriangles
    drawScene.rgl <- misc3d:::drawScene.rgl
    translateTriangles <- misc3d:::translateTriangles
    scaleTriangles <- misc3d:::scaleTriangles
    updateTriangles <- misc3d:::updateTriangles
    drawScene <- misc3d:::drawScene
    perspLighting <- misc3d:::perspLighting
    surfaceTriangles <- misc3d:::surfaceTriangles
    
    ## Example 1: Bivariate quadratic
    zz<-surfaceTriangles(seq(-1,1,len=30), seq(-1,1,len=30),
                         function(x, y) (x^2 + y^2), color2 = "green")
    drawScene(zz)
    drawScene(updateTriangles(zz, material = "metal"),
              screen=list(z=45, y=110),light=c(.3,.3,1))

    ## Example 2: Bivariate normal density
    zz<-surfaceTriangles(seq(-2,2,len=30), seq(-2,2,len=30),
                         function(x, y) 2 * exp(-0.5 * (x^2 + y^2)))
    drawScene(zz)
    drawScene(zz, light=c(.5,.5,1))

    drawScene(zz, lighting=perspLighting, light=c(.5,.5,1)) 
    drawScene(updateTriangles(zz, material = "dull"), light=c(.5,.5,1))
    drawScene(updateTriangles(zz, material = "shiny"), light=c(.5,.5,1))
    drawScene(updateTriangles(zz, material = "metal"), light=c(.5,.5,1))

    ## Example 3: Volcano
    z <- 2 * volcano
    x <- 10 * (1:nrow(z))
    y <- 10 * (1:ncol(z))
    vtri <- surfaceTriangles(x, y, z, color="green3")
    vtriDull <- updateTriangles(vtri,material="dull")
    vtriMetal <- updateTriangles(vtri,material="metal")
    vtriShiny <- updateTriangles(vtri,material="shiny")
    
    drawScene(vtri, screen=list(x=40, y=-40, z=-135), scale = FALSE)
    drawScene(vtriShiny, screen=list(x=40, y=-40, z=-135), scale = FALSE)
    drawScene(vtri,lighting=perspLighting,
              screen=list(x=40, y=-40, z=-135), scale = FALSE)

    drawScene(vtri, light=c(1, 1.5, 0),screen=list(x=40, y=-40, z=-135),
              scale=FALSE)
    drawScene(vtri,lighting=perspLighting, light=c(1, 1.5, 0),
              screen=list(x=40, y=-40, z=-135), scale = FALSE)

    drawScene(vtriDull, light=c(1, 1.5, 0),
              screen=list(x=40, y=-40, z=-135), scale = FALSE)
    drawScene(vtriMetal, light=c(1, 1.5, 0),
              screen=list(x=40, y=-40, z=-135), scale = FALSE)
    drawScene(vtriShiny, light=c(1, 1.5, 0),
              screen=list(x=40, y=-40, z=-135), scale = FALSE)

    drawScene(vtriDull, light=c(1, 1.5, 0),
              screen=list(x=40, y=-40, z=-135), scale = FALSE, engine = "grid")
    drawScene(vtriMetal, light=c(1, 1.5, 0),
              screen=list(x=40, y=-40, z=-135), scale = FALSE, engine = "grid")
    drawScene(vtriShiny, light=c(1, 1.5, 0),
              screen=list(x=40, y=-40, z=-135), scale = FALSE, engine = "grid")

    drawScene(list(vtri,
                   translateTriangles(vtriMetal, y = 650),
                   translateTriangles(vtriDull, x=900),
                   translateTriangles(vtriShiny, x=900,y = 650)),
              light = c(1, 1.5, 0), screen = list(x=40, y=-40, z=-135),
              scale = FALSE)

    n <- 50
    tx <- matrix(seq(-pi, pi, len = 2 * n), 2 * n, n)
    ty <- matrix(seq(-pi, pi, len = n)/2, 2 * n, n, byrow = T)
    xx <- cos(tx) * cos(ty)
    yy <- sin(tx) * cos(ty)
    zz <- sin(ty)
    zzz <- zz

    zzz[, 1:12 * 4] <- NA

    ## wireframe(zzz ~ xx * yy, shade = TRUE, light.source = c(3, 3, 3))
    ##**** redo this with parametric3d
    vv <- local({
        n1 <- nrow(xx)
        n2 <- ncol(xx)
        tx <- surfaceTriangles(1:n1,1:n2,xx)
        ty <- surfaceTriangles(1:n1,1:n2,yy)
        tz <- surfaceTriangles(1:n1,1:n2,zz)
        makeTriangles(v1 = cbind(tx$v1[,3],ty$v1[,3],tz$v1[,3]),
                      v2 = cbind(tx$v2[,3],ty$v2[,3],tz$v2[,3]),
                      v3 = cbind(tx$v3[,3],ty$v3[,3],tz$v3[,3]))
    })

    w <- local({
        v <- (vv$v1 + vv$v2 + vv$v3)/3
        sin(3*v[,1])+cos(5*v[,2])*sin(7*v[,3])
    })

    drawScene(updateTriangles(vv,color=terrain.colors(length(w))[rank(w)]) )
    drawScene(updateTriangles(vv,color=rainbow(length(w))[rank(w)]))
    drawScene(updateTriangles(vv,color=rainbow(length(w))[rank(w)],
                              col.mesh="black"))

    if (suppressWarnings(require(maps,quietly=TRUE))) {
        m <- map(plot = F)
        drawScene(updateTriangles(vv,color="lightblue"))
        i <- which(m$x > 0)
        m$x[i] <- NA
        m$y[i] <- NA
        m$x <- m$x * pi / 180
        m$y <- m$y * pi / 180
        lines(sin(m$x+pi/2)*cos(m$y), sin(m$y))
    }

    vv <- local({
        n1 <- nrow(xx)
        n2 <- ncol(xx)
        tx <- surfaceTriangles(1:n1,1:n2,xx)
        ty <- surfaceTriangles(1:n1,1:n2,yy)
        tz <- surfaceTriangles(1:n1,1:n2,zzz)
        makeTriangles(v1 = cbind(tx$v1[,3],ty$v1[,3],tz$v1[,3]),
                      v2 = cbind(tx$v2[,3],ty$v2[,3],tz$v2[,3]),
                      v3 = cbind(tx$v3[,3],ty$v3[,3],tz$v3[,3]))
    })

    drawScene(updateTriangles(vv,color=terrain.colors(length(w))[rank(w)]) )
    drawScene(updateTriangles(vv,color=rainbow(length(w))[rank(w)]))
    drawScene(updateTriangles(vv,color=rainbow(length(w))[rank(w)],
                              col.mesh="black"))

    drawScene(updateTriangles(vtri, smooth = 1),
              screen = list(x = 40,  y= -40, z = -135), scale = FALSE)
    drawScene(updateTriangles(vtri, smooth = 2),
              screen = list(x = 40, y = -40, z = -135), scale = FALSE)

    drawScene(updateTriangles(vtri, smooth = 2,
                              color = function(x,y,z) {
                                  cols <- terrain.colors(diff(range(z)))
                                  cols[z - min(z) + 1]}),
              screen = list(x = 40, y = -40, z = -135), scale = FALSE,
              persp = TRUE, depth = 0.6)

    ##**** allow mask in parametric3d instead?
    omitTriangles <- function(triangles, omit) {
        if (is.function(omit)) {
            v <- (triangles$v1 + triangles$v2 + triangles$v3) / 3
            omit <- omit(v[,1], v[,2], v[,3])
        }
        n.tri <- nrow(triangles$v1)
        triangles$v1 <- triangles$v1[! omit,]
        triangles$v2 <- triangles$v2[! omit,]
        triangles$v3 <- triangles$v3[! omit,]
        for (i in seq(along = triangles))
            if (length(triangles[[i]]) == n.tri)
                triangles[[i]] <- triangles[[i]][! omit]
        triangles
    }

    svtri <- omitTriangles(vtri,function(x,y,z)
                           floor(0.1 * x) %% 4 != 0 & floor(0.1 * x) %% 4 != 1)
    drawScene(updateTriangles(svtri, smooth=2,
                              color = function(x,y,z) {
                                  cols <- terrain.colors(diff(range(z)))
                                  cols[z - min(z) + 1]}),
              screen=list(x=40, y=-40, z=-135), scale = FALSE)
})
