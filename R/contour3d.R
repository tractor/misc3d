#       8-------7         
#      /|      /|         
#     5-+-----6 |         
#     | |     | |
#     | |     | |
#     | 4-----+-3
#     |/      |/
#     1------ 2
#"contour3d" is a program using marching cubes algorithm to draw an isosurface.   
#Ideas and algorithms can be found in references as follows:
#[1] W. Lorensen and H. Cline,  "Marching Cubes: A High Resolution 3D Surface Reconstruction Algorithm",  
#Computer Graphics, vol. 21, no. 4, pp. 163-169, 1987. 
#[2] G. Nielson and B. Hamann,  "The Asymptotic Decider: Resolving the Ambiguity in Marching Cubes",  
#Proc. IEEE Visualization  92, pp. 83-91, 1992. 
#[3] E. Chernyaev,  "Marching Cubes 33: Construction of Topologically Correct Isosurfaces",  
#Technical Report CN/95-17, CERN, 1995. 
#  
#"Density" can be gotten from either 3d array volume data or function. 

#"contour3d" has 8 parameters
#f: the function used to get the "density" or an 3d array.
#threshold: the density value according to which to draw the isosurface
#xmin, ymin, zmin: the mimum value of the three dimensional region in which to draw the isosurface.
#	For array data, these are 1 by default.
#xmax, ymax, zmax: the maxium value of the three dimensional region in which to draw the isosurface. 
#	For array data, these are the dimension of each axis.
#xpoints, ypoints, zpoints: the number of cut points on each axis to form cubes  


#The package consists of two parts mainly.
#The first part is pre-computation of some tables
#The second part try to figure out the vertices of triangles within each cube
#################################################################################
#The PDL of the program is as follows:

#
#"PreProcessing" pre-compute a bunch of tables containing information of cube and triangles.
#      >BasicRotation is the table of basic rotations. The table has 24 rows and 8 columns. 
#       Each row corresponding to one basic rotaion. 8 columns are 8 vertices corresponding to each rotation. 
#       There are 24 rotations because:
#       1. Each cube has 6 faces, each face can be treated as the bottom face.
#       2. For each bottom face, there are 4 vertices and each can be treated as the left-front point of the rectangle.  


#      The program to get CaseRotation is at:~\draw3dgraphNew/Rotation/getTiangles-rotation-1.txt
#      >CaseRotationFlip is a table consists of the corresponding basic case, basic rotation and whether is a flip of basic rotation  
#       for each of 256 cases.
#       There are 256 rows corresponding to 256 cases. There are 3 columns. Column 1 is the corresponding basic case;
#       2 is the basic rotation, 3 is whether there is a flip.

#      >EdgePoints is the table telling the two endpoints of each edge.
#       There are 13 rows each for one edge and edge 13 is useful for some subcases when an additional point in the middle 
#       of the cube is needed(see reference [3]). There are 3 columns. Column 1 is the edge number, column 2 and 3 are endpoints of the edge 

#      >BasicEdges is a list which tells the edges where the vertices of triangles are.
#       For example, BasicEdges[[1]] is c(1, 4, 9) because the vertices of triangle for basic case 1 is on edge 1, 4 and 9.(see reference [1] for basic edges for each case.)


#      >EdgeSequence1 and EdgeSequence2 are lists telling in what sequence to connect vertices of triangles. For example,
#       EdgeSequence1[[2]] is c(1,2,4,2,3,4), which means the points on the first edge, the second and the forth of 
#       basic case 2 are vertices of the first triangle; the points on the second, the third and the forth edge are vertices
#	of the second triangles. 
#	Since BasicEdge[[2]] is c(2,4,9,10),  vertices of the first triangle are on edge 2,4 and 10,
#       and vertices of the second triangle are on edge 4,9 and 10.
#	The only difference between the two lists is the orientation of triangles. The orientation of the first list is clockwise 
#	if the larger side of triangles is put on top. Flip-flop changes the orientation. 

#	>GetEdges is a function used to get the list "Edges". Edges tells , for each case, on what edges those vertices of triangles are.
#		>>Tell the edges where vertices of triangles are, base on the BasicEdge and rotation relation.
#		>>Tell the sequence of edges based on the EdgeSequence1 or 2, depended on flip-flop relation.

#	>BasicFace tells which faces need to be checked for further judgment of subcases of some basic cases. 
#	 See [2] and [3] for detail.

#	>FacePoints is the table telling the points of each face and the production of the 4 positions which characterize faces. 


#       The following should be changed.
#	>GetFaces is a function used to get the list "Faces". Faces tells, for each case, what faces need to be checked for further judgment of
#	 subcases.(see [3] for detail) 
#		>>Get the faces for each case based on the BasicFace and rotation relation
#		>>Get the structure of the face.
#                       >>>In order to deal with flip and flop,
#                                       >>>> explode the case to make a index 
#                                       >>>> when the sum of index > 4, flip the index.
#                                            Note when index = 4, flip-flop is useless for basic case 10
#			>>>For face ambiguity,
#                              there are four cases before flip flop,
#                              0----1                  1----0                
#                              |    |  connection      |    |  connection 
#                              |    |  AC-BD > 0       |    |  AC-BD < 0
#                              1----0                  0----1
#                          (1) Test two positive   (2)  Test two positive
#
#                              1----0                  0----1                
#                              |    |   connection     |    |   connection
#                              |    |   AC-BD > 0      |    |   AC-BD < 0
#                              0----1                  1----0   
#                          (3) Test two negative    (4)  Test two negative

#                          (3) is the flip of (1)   (4) is the flip of (2)
#
#                          after flip-flop, there are two cases.
#				>>>>if the point on the left-lower is above the threshold, face=face  
#                               >>>>else face = -face
#			>>>For internal ambiguity
#				>>>>for case 4 ,6, 10 and 12, test whether the positive nodes are connected. 
#				>>>>for case 7, test whether the negative nodes are connected.
 


#contour3d
#	>Make a judgment whether the first parameter is a function or an array
#		>>if it is a function, interface = 1
#		>>else if it is an array, interface = 2
#		>>else error

#	>Use GetPos1scale to get the coordinates of position 1 of each cube and increment on each axis.
#	>Decide whether the matrix is too large to manipulate. The limit is now set as 10Mb. The global
#        variable "memorylimit" is used as the limit. Split the work into several batches, if needed.
#       >Get the vertices of triangles for rendering.
#		>>GetInfo  makes the matrix called "information", which consists of 5 columns and #-of-cubes*8 rows.
#                 The first 3 columns tell the coordinate(x,y,z) of each vertex of the cube, 
#                 the 4th gives the density minus the threshold, which actually makes the threshold eaqual to 0. This is convenient
#                 for further judgment of subcases(see [3]).   
#                 The 5th column is 1 or 0, based on 4th column>=0 or not. 
#                 Every 8 rows represent a cube.
#      			>>>stretch the coordinate matrix of position 1 of each cube out to every position, based on the coordinate relation.
#		        	>>>>Get the corresponding coordinate(x,y,z) for 8 vertices with respect to position 1 for each cube
#                               >>>>Get the real coordinates of 8 vertices for each cube, based on the increment of each axis.		  			    
#      			>>>Compute the 4th and 5th column
#	        
#
#	        >>GetPoints gets the coordinates of vertices of triangles for a single cube. The input is a 8row*5col matrix containing
#                 the information of the cube, the output is the coordinates of vertices of triangles.
#       		>>>Based on the case of the cube to get the edges where the vertices of triangles locate.
#               		>>>>if the case is among (1,2,5,8,9,11,13,14), check Edges table directly.
#            			>>>>else use GetFaceEdges to get edges for other cases which consists of subcases. 
#                                   The input of GetFaceEdges is a vector indicating the relevant faces need to be checked.
#                                   The output is edges of a subcase.
#                       		>>>>>Use "GetFaceIndex" to get the index vector,which indicates the status of relevant faces of the case
#                                            The input is the vector containing face numbers of a case.
#                                            The output is the index vector
#                    	        		>>>>>>For each component of index vector, get the index 1 or -1 
#                                                     no matter the face is negative or positive.
#                       				>>>>>>>if face!= 7, use GetIndexFaceNo7 to get the index
#                                                              The algorithm is in [3].
#                                               	>>>>>>>else face = 7, get the index from GetIndexFace7. The input of it is the 4th column of 
#                                                              8row*5col matrix. The algorithm is also in [3]. 
#							>>>>>>>if face < 0, flip the index
#                               	   
#                               	>>>>>According to the index to get edges of subcases of each case.
#               	>>>Use CalPoint" to get the coordinate of vertex. The input is two endpoints of the edge.
#                          Use bilinear interpolation to get the output, the coordinate of one vertex of the triangle.
#               		>>>>if vertex of triangle is on the edge of cube, use bilinear interpolation.
#               		>>>>else take the middle point of the cube.
#    		>>use rgl to draw the graph
       	   	    





# In order to use grid.polygon() to render 3 dimensional contour plot,
# First: concatenate all triangles together.
# Second: render those triangles by grid.polygon().
#1/23/06
# the rendering part of contour3d is deleted
# the "add" judgement is deleted
# the output is the matrix consists of triangles.
contour3d <- function (f, level, x = 1:dim(f)[1], y = 1:dim(f)[2], z = 1:dim(f)[3], add, color, alpha, engine,...)
 
{
 
    PreProcessing <- local({
        implode <- function(x) sum(2^(0:7) * x) + 1
        explode <- function(x) floor(((x - 1)%%2^(1:8))/2^(0:7))
        flip <- function(x) implode(ifelse(explode(x) == 1, 0, 
            1))
        BasicRotation <- matrix(data = c(1, 2, 3, 4, 5, 6, 7, 
            8, 5, 6, 2, 1, 8, 7, 3, 4, 8, 7, 6, 5, 4, 3, 2, 1, 
            4, 3, 7, 8, 1, 2, 6, 5, 2, 6, 7, 3, 1, 5, 8, 4, 6, 
            5, 8, 7, 2, 1, 4, 3, 5, 1, 4, 8, 6, 2, 3, 7, 4, 1, 
            2, 3, 8, 5, 6, 7, 3, 4, 1, 2, 7, 8, 5, 6, 2, 3, 4, 
            1, 6, 7, 8, 5, 6, 7, 3, 2, 5, 8, 4, 1, 7, 8, 4, 3, 
            6, 5, 1, 2, 8, 5, 1, 4, 7, 6, 2, 3, 7, 3, 2, 6, 8, 
            4, 1, 5, 4, 8, 5, 1, 3, 7, 6, 2, 3, 2, 6, 7, 4, 1, 
            5, 8, 2, 1, 5, 6, 3, 4, 8, 7, 1, 4, 8, 5, 2, 3, 7, 
            6, 1, 5, 6, 2, 4, 8, 7, 3, 5, 8, 7, 6, 1, 4, 3, 2, 
            8, 4, 3, 7, 5, 1, 2, 6, 3, 7, 8, 4, 2, 6, 5, 1, 7, 
            6, 5, 8, 3, 2, 1, 4, 6, 2, 1, 5, 7, 3, 4, 8), ncol = 8, 
            byrow = TRUE)
        CaseRotation <- matrix(data = c(1, 24, 2, 19, 2, 17, 
            3, 17, 2, 24, 4, 24, 3, 24, 6, 10, 2, 15, 3, 19, 
            4, 17, 6, 9, 3, 9, 6, 8, 6, 1, 9, 23, 2, 20, 3, 18, 
            4, 7, 6, 16, 5, 24, 7, 5, 7, 24, 12, 9, 4, 20, 6, 
            22, 8, 24, 10, 24, 7, 9, 15, 24, 13, 20, 6, 20, 2, 
            21, 4, 6, 3, 16, 6, 4, 4, 16, 8, 23, 6, 14, 10, 23, 
            5, 21, 7, 10, 7, 16, 15, 9, 7, 2, 13, 8, 12, 23, 
            6, 6, 3, 6, 6, 17, 6, 18, 9, 18, 7, 4, 13, 17, 15, 
            18, 6, 13, 7, 6, 12, 16, 13, 18, 6, 2, 11, 24, 7, 
            3, 7, 12, 3, 12, 2, 23, 5, 23, 4, 23, 7, 1, 3, 14, 
            7, 14, 6, 21, 15, 23, 4, 15, 7, 19, 8, 19, 13, 23, 
            6, 11, 12, 17, 10, 19, 6, 23, 4, 12, 7, 18, 8, 22, 
            13, 16, 7, 13, 11, 23, 13, 21, 7, 15, 8, 21, 13, 
            22, 14, 24, 8, 15, 13, 11, 7, 7, 8, 12, 4, 22, 3, 
            23, 7, 23, 6, 24, 12, 18, 6, 7, 13, 19, 9, 24, 6, 
            19, 7, 21, 11, 18, 13, 24, 7, 20, 15, 16, 7, 22, 
            6, 15, 3, 22, 6, 3, 15, 17, 10, 22, 6, 12, 12, 24, 
            7, 11, 6, 5, 3, 15, 13, 10, 7, 8, 8, 20, 4, 9, 7, 
            17, 5, 22, 4, 18, 2, 22, 2, 22, 4, 18, 5, 22, 7, 
            17, 4, 9, 8, 20, 7, 8, 13, 10, 3, 15, 6, 5, 7, 11, 
            12, 24, 6, 12, 10, 22, 15, 17, 6, 3, 3, 22, 6, 15, 
            7, 22, 15, 16, 7, 20, 13, 24, 11, 18, 7, 21, 6, 19, 
            9, 24, 13, 19, 6, 7, 12, 18, 6, 24, 7, 23, 3, 23, 
            4, 22, 8, 12, 7, 7, 13, 11, 8, 15, 14, 24, 13, 22, 
            8, 21, 7, 15, 13, 21, 11, 23, 7, 13, 13, 16, 8, 22, 
            7, 18, 4, 12, 6, 23, 10, 19, 12, 17, 6, 11, 13, 23, 
            8, 19, 7, 19, 4, 15, 15, 23, 6, 21, 7, 14, 3, 14, 
            7, 1, 4, 23, 5, 23, 2, 23, 3, 12, 7, 12, 7, 3, 11, 
            24, 6, 2, 13, 18, 12, 16, 7, 6, 6, 13, 15, 18, 13, 
            17, 7, 4, 9, 18, 6, 18, 6, 17, 3, 6, 6, 6, 12, 23, 
            13, 8, 7, 2, 15, 9, 7, 16, 7, 10, 5, 21, 10, 23, 
            6, 14, 8, 23, 4, 16, 6, 4, 3, 16, 4, 6, 2, 21, 6, 
            20, 13, 20, 15, 24, 7, 9, 10, 24, 8, 24, 6, 22, 4, 
            20, 12, 9, 7, 24, 7, 5, 5, 24, 6, 16, 4, 7, 3, 18, 
            2, 20, 9, 23, 6, 1, 6, 8, 3, 9, 6, 9, 4, 17, 3, 19, 
            2, 15, 6, 10, 3, 24, 4, 24, 2, 24, 3, 17, 2, 17, 
            2, 19, 1, 24), ncol = 2, byrow = TRUE)
        CaseRotationFlip <- cbind(CaseRotation, c(1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, -1, 1, 1, 1, 1, 
            1, 1, 1, 1, 1, 1, 1, -1, 1, -1, -1, -1, 1, 1, 1, 
            1, 1, 1, 1, -1, 1, 1, 1, 1, 1, 1, -1, -1, 1, 1, 1, 
            1, 1, 1, 1, -1, 1, 1, 1, -1, 1, -1, -1, -1, 1, 1, 
            1, 1, 1, 1, 1, -1, 1, 1, 1, -1, 1, -1, -1, -1, 1, 
            1, 1, 1, 1, 1, 1, -1, 1, 1, -1, -1, 1, -1, -1, -1, 
            1, 1, 1, 1, 1, -1, 1, -1, 1, 1, 1, -1, -1, -1, -1, 
            -1, 1, 1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, 
            -1, -1, -1, -1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
            -1, 1, 1, -1, -1, 1, 1, 1, 1, 1, -1, -1, -1, 1, -1, 
            1, -1, -1, -1, -1, -1, 1, 1, 1, -1, 1, 1, -1, -1, 
            1, -1, -1, -1, -1, -1, -1, -1, 1, 1, 1, -1, 1, -1, 
            -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, 1, 1, 1, -1, 
            1, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, 1, 
            1, -1, -1, -1, -1, -1, -1, 1, -1, -1, -1, -1, -1, 
            -1, -1, 1, 1, 1, -1, 1, -1, -1, -1, -1, -1, -1, -1, 
            -1, -1, -1, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1, 
            -1, -1, -1, -1, -1, -1, -1))
        EdgePoints <- matrix(data = c(1, 1, 2, 2, 2, 3, 3, 3, 
            4, 4, 4, 1, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, 5, 9, 
            1, 5, 10, 2, 6, 11, 3, 7, 12, 4, 8, 13, 9, 9), ncol = 3, 
            byrow = TRUE)
        BasicEdges <- list(c(1, 4, 9), c(2, 4, 9, 10), c(1, 4, 
            5, 6, 9, 10), c(1, 4, 6, 7, 9, 11), c(1, 4, 10, 11, 
            12), c(2, 4, 6, 7, 9, 10, 11), c(1, 2, 5, 6, 7, 8, 
            9, 10, 11, 13), c(9, 10, 11, 12), c(1, 2, 7, 8, 9, 
            11), c(1, 2, 3, 4, 5, 6, 7, 8, 13), c(1, 2, 6, 7, 
            9, 12), c(1, 4, 5, 8, 9, 10, 11, 12, 13), c(1, 2, 
            3, 4, 5, 6, 7, 8, 9, 10, 11, 12,13), c(1, 4, 7, 8, 10, 
            11))

        EdgeSequence1 <- list(c(1, 2, 3), c(1, 2, 4, 2, 3, 4), list(c(1, 2, 5, 3, 4, 6), c(1, 2, 6, 2, 4, 6, 2, 
                3, 4, 2, 5, 3)), list(c(1, 2, 5, 3, 4, 6), c(1,3, 5, 3, 4, 5, 2, 5, 4, 1, 2, 6, 1, 6, 3, 2, 
                4, 6)), c(1, 3, 2, 2, 3, 5, 3, 4, 5), list(c(1,2, 6, 2, 5, 6, 3, 4, 7), c(1, 2, 7, 2, 4, 7, 
                2, 5, 4, 3, 4, 5, 3, 5, 6, 1, 6, 3, 1, 3, 7, 7, 3, 4), c(1, 2, 7, 2, 4, 7, 2, 5, 4, 3, 4, 
                5, 3, 5, 6)), list(c(1, 8, 2, 3, 7, 6, 4, 5, 9), c(1, 8, 2, 3, 9, 4, 3, 7, 9, 5, 9, 7, 5, 
                7, 6), c(3, 7, 6, 1, 8, 4, 1, 4, 5, 1, 9, 2, 1, 5, 9), c(4, 5, 9, 1, 7, 6, 1, 6, 2, 2, 6, 
                3, 2, 3, 8), c(1, 10, 2, 2, 10, 9, 5, 9, 10, 5, 10, 6, 6, 10, 7, 3, 7, 10, 3, 10, 4, 4, 10, 
                8, 1, 8, 10), c(1, 10, 2, 1, 10, 7, 6, 10, 7,5, 10, 6, 5, 9, 10, 4, 9, 10, 3, 10, 4, 3, 8, 
                10, 2, 10, 8), c(5, 9, 10, 2, 10, 9, 1, 10, 2, 1, 7, 10, 6, 10, 7, 3, 10, 6, 3, 8, 10, 4, 10, 
                8, 4, 5, 10), c(1, 7, 6, 1, 6, 9, 1, 9, 2, 5, 9, 6, 3, 8, 4), c(1, 7, 8, 3, 8, 7, 3, 7, 6, 
                3, 6, 5, 3, 5, 4, 4, 5, 9, 4, 9, 8, 2, 8, 9, 1, 8, 2)), c(1, 2, 3, 1, 3, 4), c(1, 2, 6, 1, 
                3, 5, 1, 6, 3, 3, 4, 5), list(c(1, 4, 5, 2, 6, 3, 3, 6, 7, 4, 8, 5), c(1, 4, 3, 1, 3, 2, 3, 
                4, 8, 3, 8, 7, 5, 8, 6, 6, 8, 7, 1, 2, 6, 1, 6, 5), c(1, 2, 9, 1, 9, 5, 5, 9, 8, 4, 8, 9, 
                3, 4, 9, 3, 9, 7, 6, 7, 9, 2, 6, 9), c(5, 9, 6, 1, 9, 5, 1, 4, 9, 4, 8, 9, 7, 9, 8, 3, 9, 
                7, 2, 9, 3, 2, 6, 9), c(1, 2, 5, 2, 6, 5, 3, 4, 8, 3, 8, 7)), c(1, 2, 3, 1, 3, 6, 1, 6, 5, 
                3, 4, 6), list(c(1, 6, 2, 2, 6, 8, 3, 5, 4, 6, 7, 8), c(1, 5, 3, 1, 3, 6, 3, 6, 7, 3, 7, 4, 
                2, 4, 8, 2, 5, 4, 1, 5, 2, 4, 7, 8), c(1, 9, 2, 2, 9, 5, 3, 5, 9, 4, 9, 8, 3, 9, 4, 7, 8, 
                9, 6, 7, 9, 1, 9, 6), c(4, 9, 5, 1, 5, 9, 1, 9, 2, 2, 9, 8, 7, 8, 9, 6, 9, 7, 3, 6, 9, 3, 
                9, 4), c(1, 5, 2, 3, 8, 4, 3, 6, 8, 6, 7, 8)),
                list(
                #13.1
		c(7,8,12,2,3,11,1,4,9,5,6,10),
		#13.2
		c(2,  3, 11,  7,  8, 12,  9,  5,  4,  5,  4,  6,  4,  6,  1,  6,  1, 10),
		c(1,  4,  9,  7,  8, 12, 10,  2,  5,  2,  5,  3,  5,  3,  6,  3,  6, 11),
		c(5,  6, 10,  1,  4,  9, 11,  7,  2,  7,  2,  8,  2,  8,  3,  8,  3, 12),
		c(5,  6, 10,  2,  3, 11, 12,  4,  7,  4,  7,  1,  7,  1,  8,  1,  8,  9),
		c(5,  6, 10,  7,  8, 12,  2, 11,  1, 11,  1,  9, 11,  9,  3,  9,  3,  4),
		c(2,  3, 11,  4,  1,  9,  5, 10,  8, 10,  8, 12, 10, 12,  6, 12,  6,  7),
		#13.3
		c(7, 8, 12, 13, 3, 11, 13, 11, 6, 13,  6, 5, 13, 5,  9, 13,  9,  4, 13,  4,  1, 13, 1,10,13,10, 2,13,2,3),
		c(2, 3, 11, 13, 6, 10, 13, 10, 1, 13,  1, 4, 13, 4, 12, 13, 12,  7, 13,  7,  8, 13, 8, 9,13,9, 5, 13,5,6),
		c(7, 8, 12, 13, 6,  5, 13,  5, 9, 13,  9, 4, 13, 4,  3, 13,  3, 11, 13, 11,  2, 13, 2, 1,13,1,10,13,10,6),
		c(2, 3, 11, 13,  4,  1, 13,  1, 10, 13, 10, 6, 13, 6, 7,13,  7, 12, 13, 12,  8, 13, 8, 5,13,5, 9,13, 9,4),
		c(1, 4, 9,  13,  8, 12, 13, 12,  3, 13,  3, 2, 13, 2, 10, 13, 10, 5, 13,  5, 6, 13, 6,11,13,11,7,13, 7,8),
		c(7, 8, 12, 13,  5,  6, 13,  6, 11, 13, 11, 3, 13, 3,  4, 13,  4, 9, 13,  9, 1, 13, 1, 2,13,2,10,13,10,5),
		c(1, 4,  9, 13,  3,  2, 13,  2, 10, 13, 10, 5, 13, 5,  8, 13,  8, 12,13, 12, 7, 13, 7, 6,13,6,11,13,11,3),
	c(5, 6, 10, 13,  1,  9, 13,  9,  8, 13,  8, 7, 13, 7, 11, 13, 11,  2,13,  2,  3, 13, 3, 12, 13, 12, 4,13,  4, 1),
	c(5, 6, 10, 13,  8,  7, 13,  7, 11, 13, 11, 2, 13, 2,  1, 13,  1,  9,13,  9,  4, 13, 4,  3, 13, 3, 12,13, 12, 8),
	c(1, 4,  9, 13,  2,  3, 13,  3, 12, 13, 12, 8, 13, 8,  5, 13,  5, 10, 13, 10, 6, 13, 6,  7, 13, 7, 11,13, 11, 2),
	c(5, 6, 10, 13,  7,  8, 13,  8,  9, 13,  9, 1, 13, 1,  2, 13,  2, 11, 13, 11, 3, 13, 3,  4, 13, 4, 12,13, 12, 7),
	c(2, 3, 11, 13,  1,  4, 13,  4, 12, 13, 12, 7, 13, 7,  6, 13,  6, 10, 13, 10, 5, 13, 5,  8, 13, 8, 9, 13,  9, 1),
	#13.4
	c(13, 3, 11, 13, 11, 6, 13, 6, 7, 13, 7, 12, 13, 12, 8, 13, 8, 5, 13, 5, 9, 13, 9, 4, 
  	13, 4, 1,  13, 1, 10, 13,10, 2, 13, 2,  3),
	c(13, 4, 12, 13, 12, 7, 13, 7, 8, 13, 8,  9, 13,  9, 5, 13, 5, 6, 13, 6, 10, 13, 10, 1, 13,
  	1,  2, 13,  2, 11, 13, 11,  3, 13,  3,  4),
	c(13, 2, 10, 13, 10, 5, 13, 5, 6, 13, 6, 11, 13, 11, 7, 13, 7, 8, 13, 8, 12, 13,12, 3, 13, 
  	3,  4, 13,  4,  9, 13,  9,  1, 13,  1,  2),
	c(13, 1,  9, 13,  9, 8, 13, 8, 5, 13, 5, 10, 13, 10, 6, 13, 6, 7, 13, 7, 11, 13,11, 2, 13,
  	2,  3, 13,  3, 12, 13, 12,  4, 13,  4,  1),
	#13.5.1
	c(7,  8, 12,  2,  1, 10,  3,  4, 11,  4, 11,  6,  4,  6,  9,  6,  9,  5),
	c(3,  2, 11,  8,  5,  9,  4,  1, 12,  1, 12,  7,  1,  7, 10,  7, 10,  6),
	c(1,  4,  9,  6,  7, 11,  2,  3, 10,  3, 10,  5,  3,  5, 12,  5, 12,  8),
	c(5,  6, 10,  4,  3, 12,  1,  2,  9,  2,  9,  8,  2,  8, 11,  8, 11,  7),
	#13.5.2
	c(1, 2, 10, 8, 5, 9,  8, 9,  4, 8, 4,12, 4,12, 3,12, 3,11,12,11, 7,11, 7, 6, 7, 6, 8, 6, 8, 5),
	c(8, 5,  9, 3, 4, 12, 3, 12, 7, 3, 7,11, 7,11, 6,11, 6,10,11,10, 2,10, 2, 1, 2, 1, 3, 1, 3, 4),
	c(6, 7, 11, 1, 2, 10, 1, 10, 5, 1, 5, 9, 5, 9, 8, 9, 8,12, 9,12, 4,12, 4, 3, 4, 3, 1, 3, 1, 2),
	c(3, 4, 12, 6, 7, 11, 6, 11, 2, 6, 2, 10,2, 10, 1,10,1, 9, 10,9, 5, 5, 9, 8, 5, 8, 6, 8, 6,  7)),
        c(1, 4, 2, 1, 6, 4, 1, 5, 6, 3, 6, 4))
	
	switch23 <- function(x){
		num <- length(x) / 3
		temp <- x[c(0: (num-1))*3 + 2]
		x[c(0: (num-1))*3 + 2] <- x[c(0: (num-1))*3+ 3]
      		x[c(0: (num-1))*3 + 3] <- temp
		x
	}

	SwitchSeq <- function(ed){
		for (i in 1:length(ed)){
      			ver <- ed[[i]]
			if (is.list(ver)){
				for(j in 1:length(ver)){
                  			ed[[i]][[j]] <- switch23(ver[[j]])
				}
      			}
      			else
			ed[[i]] <- switch23(ver)
		}
		ed
              }
	
	EdgeSequence2 <- SwitchSeq(EdgeSequence1)
       
      
       
        GetEdges <- local({
            Edges <- apply(CaseRotationFlip[-c(1, 256), ], 1, 
                function(x) {
                  case <- x[1]
                  rotation <- x[2]
                  map <- rep(0, 8)
                  for (i in 1:8) {
                    temp <- as.integer(BasicRotation[rotation, 
                      ][i])
                    map[temp] <- i
                  }
                  sapply(BasicEdges[[case - 1]], function(x) {
                    if (x != 13) {
                      EndP1 <- EdgePoints[x, 2]
                      EndP2 <- EdgePoints[x, 3]
                      newEdge <- EdgePoints[(EdgePoints[, 2] == 
                        map[EndP1] & EdgePoints[, 3] == map[EndP2]) | 
                        (EdgePoints[, 3] == map[EndP1] & EdgePoints[, 
                          2] == map[EndP2]), ][1]
                    }
                    else newEdge <- 13
                    newEdge
                  })
                })
            Case <- cbind(seq(1:256), CaseRotationFlip[, c(1, 
                3)])
            Edges <- apply(Case[-c(1, 256), ], 1, function(x) {
                case <- x[2] - 1
                EdgeNum <- x[1] - 1
                if (x[3] == 1) 
                  sapply(EdgeSequence1[[case]], function(x) Edges[[EdgeNum]][x])
                else sapply(EdgeSequence2[[case]], function(x) Edges[[EdgeNum]][x])
            })
            Edges
        })
        BasicFace <- list(c(0), c(0), c(1), c(7), c(0), c(2, 
            7), c(1, 2, 6, 7), c(0), c(0), c(5, 6, 7), c(0), 
            c(1, 4, 7), c(1,2,3,4,5,6,7), c(0))
        FacePoints <- matrix(data = c(seq(1, 6), 1, 2, 4, 1, 
            1, 5, 6, 7, 7, 8, 3, 7, 2, 3, 3, 4, 2, 6, 5, 6, 8, 
            5, 4, 8), ncol = 5)
        FacePoints <- cbind(FacePoints, apply(FacePoints[, 2:5], 
            1, prod))
        GetFaces <- local({
            Faces <- apply(CaseRotationFlip[-c(1, 256), ], 1, 
                function(x) {
                  case <- x[1]
                  rotation <- x[2]
                  map <- rep(0, 8)
                  for (i in 1:8) {
                    temp <- as.integer(BasicRotation[rotation, 
                      ][i])
                    map[temp] <- i
                  }
                  sapply(BasicFace[[case - 1]], function(x) {
                    EndP <- rep(0, 4)
                    if (x == 0) 
                      newFace <- 0
                    else if (x == 7) 
                      newFace <- 7
                    else {
                      for (i in 1:4) {
                        point <- FacePoints[x, i + 1]
                        EndP[i] <- map[point]
                      }
                      newFace <- FacePoints[FacePoints[, 6] == 
                        prod(EndP[1:4]), ][1]
                    }
                    newFace
                  })
                })
            for (i in 1:254) {
                for (j in 1:length(Faces[[i]])) {
                  x <- Faces[[i]][j]
                  if (x != 0) {
                    index <- explode(i + 1)
                    if (sum(index) > 4) 
                      index <- ifelse(index == 0, 1, 0)
                    if (x != 7 && index[FacePoints[x, 2]] == 
                      0) 
                      Faces[[i]][j] <- -Faces[[i]][j]
                    if (x == 7) {
                      tcase <- CaseRotationFlip[i + 1, 1] - 1
                      if ((tcase == 4 || tcase == 6 || tcase == 10 || tcase == 
                        12) && !(index[1] + index[7] == 2) && 
                        !(index[3] + index[5] == 2)) 
                        Faces[[i]][j] <- -Faces[[i]][j]
                      if (tcase == 7 && !(index[1] + index[7] == 
                        0) && !(index[3] + index[5] == 0)) 
                        Faces[[i]][j] <- -Faces[[i]][j]
                    }
                  }
                }
            }
            Faces
        })
        list(Edges = GetEdges, Faces = GetFaces, EdgePoints = EdgePoints, 
            FacePoints = FacePoints, CRF = CaseRotationFlip)
    })

 GetTriangles <- function (f, level, x = 1:dim(f)[1], y = 1:dim(f)[2], z = 1:dim(f)[3]){
    implode.vec <- function(x1, x2, x3, x4, x5, x6, x7, x8) x1 + 
        2 * x2 + 2^2 * x3 + 2^3 * x4 + 2^4 * x5 + 2^5 * x6 + 
        2^6 * x7 + 2^7 * x8 + 1
    fgrid <- function(fun, x, y, z) {
        g <- expand.grid(x = x, y = y, z = z)
        array(fun(g$x, g$y, g$z), c(length(x), length(y), length(z)))
    }
    levCells <- function(v, level) {
        nx <- dim(v)[1]
        ny <- dim(v)[2]
        nz <- dim(v)[3]
        val <- vector("list", nz - 1)
        type <- vector("list", nz - 1)
        i <- 1:(nx - 1)
        j <- 1:(ny - 1)
        v1 <- v[, , 1, drop = TRUE]
        vv1 <- ifelse(v1 > level, 1, 0)
        ttt1 <- vv1[i, j] + 2 * vv1[i + 1, j] + 4 * vv1[i + 1, 
            j + 1] + 8 * vv1[i, j + 1]
        for (k in 1:(nz - 1)) {
            v2 <- v[, , k + 1, drop = TRUE]
            vv2 <- ifelse(v2 > level, 1, 0)
            ttt2 <- vv2[i, j] + 2 * vv2[i + 1, j] + 4 * vv2[i + 
                1, j + 1] + 8 * vv2[i, j + 1]
            ttt <- ttt1 + 16 * ttt2
            iii <- ttt > 0 & ttt < 255
            val[[k]] <- which(iii) + (nx - 1) * (ny - 1) * (k - 
                1)
            type[[k]] <- as.integer(ttt[iii])
            v1 <- v2
            vv1 <- vv2
            ttt1 <- ttt2
        }
        v <- unlist(val)
        i <- as.integer((v - 1)%%(nx - 1) + 1)
        j <- as.integer(((v - 1)%/%(nx - 1))%%(ny - 1) + 1)
        k <- as.integer((v - 1)%/%((nx - 1) * (ny - 1)) + 1)
        t <- unlist(type)
        NAS <- which(is.na(t))
        if (length(NAS) > 0) 
            t <- t[-NAS]
        list(i = i, j = j, k = k, t = t)
    }
    GetInfo <- function(cube.1) {
        index <- matrix(c(0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 
            0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1), nrow = 8)
        if (interface == 1) 
            ax.inc <- c(x[2] - x[1], y[2] - y[1], z[2] - z[1])
        else ax.inc <- c(1, 1, 1)
        ver.inc <- t(apply(index, 1, function(x) x * ax.inc))
        cube.co <- kronecker(rep(1, nrow(cube.1)), ver.inc) + 
            kronecker(cube.1, rep(1, 8))
        if (interface == 1) 
            value <- f(cube.co[, 1], cube.co[, 2], cube.co[, 
                3]) - level
        else value <- apply(cube.co, 1, function(x) vol[x[1], 
            x[2], x[3]]) - level
        if (interface == 1) 
            information <- cbind(cube.co, value)
        else information <- cbind(x[cube.co[, 1]], y[cube.co[, 
            2]], z[cube.co[, 3]], value)
        information
    }
    GetPoints <- function(edge, p1) {
        x1 <- EP[edge, 2]
        x2 <- EP[edge, 3]
        c((1 - floor(x1/9)) * information[p1 + x1 - 1, 1] + floor(x1/9) * 
            information[p1, 1], (1 - floor(x1/9)) * information[p1 + 
            x2 - 1, 1] + floor(x1/9) * information[p1 + 1, 1], 
            (1 - floor(x1/9)) * information[p1 + x1 - 1, 2] + 
                floor(x1/9) * information[p1 + 1, 2], (1 - floor(x1/9)) * 
                information[p1 + x2 - 1, 2] + floor(x1/9) * information[p1 + 
                2, 2], (1 - floor(x1/9)) * information[p1 + x1 - 
                1, 3] + floor(x1/9) * information[p1 + 1, 3], 
            (1 - floor(x1/9)) * information[p1 + x2 - 1, 3] + 
                floor(x1/9) * information[p1 + 5, 3], (1 - floor(x1/9)) * 
                information[p1 + x1 - 1, 4] + floor(x1/9) * (0 * 
                information[p1 + 1, 3] + 1), (1 - floor(x1/9)) * 
                information[p1 + x2 - 1, 4] + floor(x1/9) * (0 * 
                information[p1 + 1, 3] - 1))
    }
    CalPoint <- function(x1, x2, y1, y2, z1, z2, v1, v2) {
        s <- v1/(v1 - v2)
        x <- x1 + s * (x2 - x1)
        y <- y1 + s * (y2 - y1)
        z <- z1 + s * (z2 - z1)
        c(x, y, z)
    }
    FaceNo7 <- function(faces, p1) {
        index <- ifelse(faces > 0, 1, -1)
        faces <- abs(faces)
        e1 <- FP[faces, 2]
        e2 <- FP[faces, 3]
        e3 <- FP[faces, 4]
        e4 <- FP[faces, 5]
        A <- information[p1 + e1 - 1, 4]
        B <- information[p1 + e2 - 1, 4]
        C <- information[p1 + e3 - 1, 4]
        D <- information[p1 + e4 - 1, 4]
        index <- index * ifelse(A * B - C * D > 0, 1, -1)
        index <- ifelse(index == 1, 1, 0)
        index
    }
    Face7 <- function(faces, p1) {
        index <- ifelse(faces > 0, 1, -1)
        A0 <- information[p1, 4]
        B0 <- information[p1 + 3, 4]
        C0 <- information[p1 + 2, 4]
        D0 <- information[p1 + 1, 4]
        A1 <- information[p1 + 4, 4]
        B1 <- information[p1 + 7, 4]
        C1 <- information[p1 + 6, 4]
        D1 <- information[p1 + 5, 4]
        a <- (A1 - A0) * (C1 - C0) - (B1 - B0) * (D1 - D0)
        b <- C0 * (A1 - A0) + A0 * (C1 - C0) - D0 * (B1 - B0) - 
            B0 * (D1 - D0)
        c <- A0 * C0 - B0 * D0
        tmax <- -b/(2 * a)
        maximum <- a * tmax^2 + b * tmax + c
        maximum <- ifelse(maximum == "NaN", -1, maximum)
        cond1 <- ifelse(a < 0, 1, 0)
        cond2 <- ifelse(tmax > 0, 1, 0)
        cond3 <- ifelse(tmax < 1, 1, 0)
        cond4 <- ifelse(maximum > 0, 1, 0)
        totalcond <- cond1 * cond2 * cond3 * cond4
        index <- index * ifelse(totalcond == 1, 1, -1)
        index <- ifelse(index == 1, 1, 0)
    }
    PreRender <- function(edges, p1, type) {
     if(type==1){
        if (typeof(edges) == "list") {
            count <- sapply(edges, function(x) length(x))
            edges <- cbind(unlist(edges), rep(p1, count))
        }
        else {
            count <- nrow(edges)
            edges <- cbind(as.vector(t(edges)), rep(p1, each = count))
        }
     }

     else{
	if (is.vector(edges)) 
            edges <- matrix(edges, ncol = length(edges))
        p1 <- edges[, 1]
        count <- ncol(edges) - 1
        edges <- cbind(as.vector(t(edges[, -1])), rep(p1, each = count))
     }

 
        information <- GetPoints(edges[, 1], edges[, 2])
        information <- matrix(information, ncol = 8)
        information <- CalPoint(information[, 1], information[, 
            2], information[, 3], information[, 4], information[, 
            5], information[, 6], information[, 7], information[, 
            8])
        information <- matrix(information, ncol = 3)
        information
   }
    

    Faces <- PreProcessing$Faces
    Edges <- PreProcessing$Edges
    EP <- PreProcessing$EdgePoints
    FP <- PreProcessing$FacePoints
    CR <- PreProcessing$CRF
    
    if (interface == 1) {
        NAS <- unique(c(which(is.na(x)), which(is.na(y)), which(is.na(z))))
        if (length(NAS) > 0) {
            x <- x[-NAS]
            y <- y[-NAS]
            z <- z[-NAS]
        }
        vol <- fgrid(f, x, y, z)
    }
    else vol <- f

    if (interface == 2) {
        nx <- dim(vol)[1]
        ny <- dim(vol)[2]
        nz <- dim(vol)[3]
                
        if (length(x) != nx || length(y) != ny || length(z) != 
            nz) 
            stop("dimensions of f do not match x, y, or z")
    }
    

    GetBasic <- function(R){
     if (interface == 1) 
            cube.1 <- cbind(x[v$i[R]], y[v$j[R]], z[v$k[R]])
        else cube.1 <- cbind(v$i[R], v$j[R], v$k[R])
        information <- GetInfo(cube.1)
        information <- rbind(information, rep(0, 4))
        p1 <- (1:length(R) - 1) * 8 + 1
        cases <- v$t[R]
        list(information=information, p1 = p1, cases=cases)
   }
    
    v <- levCells(vol, level)
    tcase <- CR[v$t + 1, 1] - 1
    R <- which(tcase %in% c(1, 2, 5, 8, 9, 11,14))
    if (length(R) > 0) {
        Basics <- GetBasic(R)
        information <- Basics$information
        p1 <- Basics$p1
        cases <- Basics$cases
        edges <- Edges[cases]
        triangles <- PreRender(edges, p1,type=1)
         
    }
    #special
    #name : name of the case
    #nface: how many cases need to be checked
    #sev: whether face 7 need to be checked
    #nedge: total number of edges in the lookuptable
    #ind: the index needed to check the lookuptable
    #position: the corresponding positions in the lookuptable.
    special <- list(name =c(3, 4, 6, 7,  10,12,13),
                    nface=c(1, 1, 2, 4,  3, 3, 7),
                    sev = c(0, 1, 1, 1,  1, 1, 1),
                    nedge=c(18,24,48,177,96,96,816),
                    ind=list(c(0,1), c(0,1), c(0,2,1,3),c(0,8,4,12,2,10,1,9,6,14,5,13,3,11,15,7),
                             c(0,4,1,5,2,6,3,7),c(0,4,2,6,1,5,3,7),
                             c(0,1,2,4,8,16,32,3,9,17,33,6,18,34,12,20,36,24,40,35,25,22,44,19,41,38,28,83,105,102,92)),
                    position=list(list(c(1:6),c(7:18)),
                                  list(c(1:6),c(7:24)),
                                  list(c(1:9),c(10:33),c(34:48),c(34:48)),
                                  list(c(1:9),c(1:9),c(10:24),c(10:24),c(25:39),c(25:39),c(40:54), c(40:54),
                                       c(55:81), c(55:81),c(82:108),c(82:108),c(109:135),c(109:135),c(136:150),c(151:177)),
                                  list(c(1:12),c(13:36),c(37:60),c(37:60), c(61:84), c(61:84),c(85:96),c(85:96)),
                                  list(c(1:12), c(13:36), c(37:60), c(37:60),c(61:84), c(61:84),c(85:96),c(85:96)),
                                  list(
                                       c(1:12),
                                       c(13:30),c(31:48),c(49:66),c(67:84),c(85:102),c(103:120),
                                       c(121:150),c(151:180),c(181:210),c(211:240),c(241:270),c(271:300),c(301:330),
                                       c(331:360),c(361:390),c(391:420),c(421:450),c(451:480),
                                       c(481:516),c(517:552),c(553:588),c(589:624),
                                       c(625:642),c(643:660),c(661:678),c(679:696),
                                       c(697:726),c(727:756),c(757:786),c(787:816))
                                 )
                    )


    for (i in 1:length(special$name)){
      R <- which(tcase == special$name[i])
      if (length(R) > 0) {
       
        Basics <- GetBasic(R)
        information <- Basics$information
        p1 <- Basics$p1
        cases <- Basics$cases

        nface <- special$nface[i]
        nedge <- special$nedge[i]
        faces <- matrix(unlist(Faces[cases]), ncol = nface, byrow = TRUE)

        if (i==1)
          index <- FaceNo7(faces[, 1], p1)
        else if (i==2)
          index <- Face7(faces[, 1], p1)
        else{ 
          index <-  Face7(faces[, nface], p1)*2^(nface-1)
          for(j in 1:(nface-1)){
            temp <-  FaceNo7(faces[, j], p1)
            index <- index + temp * 2^(j-1) 
          }
        }
        edges <- matrix(unlist(Edges[cases]), ncol = nedge, byrow = TRUE)
        edges <- cbind(edges, p1, index)
        ind <- special$ind[[i]]
        position <- special$position[[i]]
        
        for (j in 1:length(ind)){
          ed <- edges[which(index == ind[j]), c(nedge+1, position[[j]])]
          if (length(ed) > 0) 
                      triangles <- rbind(triangles, PreRender(ed,nedge+1,type=2))
        }
      }
    }
 
   
  
   triangles
  }
    plotMesh.grid<-function(triangles, rot.mat=diag(rep(1,1)), dist = 0.1, fill,alpha) 
    ## rot.mat: 4x4 transformation matrix
    ## dist: controls perspective, 0 = none
 {  
    x <- ltransform3dto3d(triangles, rot.mat, dist = dist) 
    id <- seq(length = ncol(x) / 3) 
    ord <- order(x[3, id * 3] + x[3, id * 3 - 1] + 
                 x[3, id * 3 - 2])
    
    xscale <- range(x[1,]) 
    yscale <- range(x[2,]) 
    md <- max(diff(xscale), diff(yscale)) 
    pushViewport(viewport(w = 0.9 * diff(xscale) / md, 
                          h = 0.9 * diff(yscale) / md, 
                          xscale = xscale,
                         yscale = yscale))
    id <-
        as.vector(outer(1:3, (id[ord]-1) * 3, "+"))
    grid.polygon(x = x[1,id], 
                 y = x[2,id],
                 default.units = "native",
                 gp = gpar(fill = fill[ord], alpha=alpha[ord]), 
                 id = rep(id[ord], each = 3)) 
 }

    Render <- function(f,level,x,y,z, add, color, alpha, engine,...){
        

	   triangles <- matrix(0, ncol=3)
           l <- rep(0,length(level))
           for (i in seq(along = level)) {
                temp <- GetTriangles(f,level[i],x,y,z) 
                l[i] <- nrow(temp)
                triangles <- rbind(triangles, temp)
                
           }
           triangles <- triangles[-1,]
           if(engine==2){
             color <- rep(color,l/3)
             alpha <- rep(alpha,l/3)
           }
           else{
             color <- rep(color,l)
             alpha <- rep(alpha,l)
           }
        
	if (engine==1){
       		if (!add) 
			rgl.clear()
	        rgl.triangles(triangles[,1], triangles[,3], -triangles[,2],color=color,
                              alpha=alpha,...)  
   	}
	if (engine==2){

		if (!add) 
       			grid.newpage() 
        	plotMesh.grid(t(triangles), rot.mat, dist = 0.3, fill= color, alpha=alpha)
        }
      }

    if (typeof(f) == "closure") 
        interface <- 1
    else if (is.array(f) && length(dim(f)) == 3) {
        interface <- 2
    }
    else stop("vol has to be a function or a 3-dimensional array")	

    if( length(level)!= length(color) || length(level)!= length(alpha))
       stop("length of level, color, alpha has to be matched.")
    
    
    Render(f, level, x,y,z, add, color, alpha, engine,...)
    
}
