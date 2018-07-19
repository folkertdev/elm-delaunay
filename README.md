# Elm SweepHull 

The sweep-hull algorithm for delaunay triangulation in elm. 

## Performance 

Poor, even compared even with the python implementation. 

In the python implementation, triangles are triplets of indices into the vertex array. 
In contrast, the elm implementation stores triangles as triples of edges which have points: data is duplicated a lot. 
More allocation means more garbage means slower execution 

Additionally, finding shared edges between triangles (given a list of triangles) is currently implemented in an `O(n^2)` way. 
There are some ways to do better than that potentially, more so when we get a `HashMap`.

Some numbers: it does over 600 runs per second for 10 random points, but just 80 for 15 random points. 
Scaling is obviously awful.
