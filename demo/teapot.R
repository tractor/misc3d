library(misc3d)

local({
    data(teapot)

    ttri <- misc3d:::makeTriangles(teapot$vertices, teapot$edges,
                                   color = "red", color2 = "green")

    misc3d:::drawScene.rgl(ttri)

    misc3d:::drawScene.rgl(list(ttri,misc3d:::translateTriangles(ttri,z=4)))

    misc3d:::drawScene.rgl(list(misc3d:::updateTriangles(ttri,color=heat.colors(ncol(teapot$edges))),
              misc3d:::translateTriangles(ttri,z=4)))

    misc3d:::drawScene.rgl(list(misc3d:::updateTriangles(ttri,alpha = 0.5,color="blue"),
              misc3d:::scaleTriangles(ttri, 0.6)))
})
