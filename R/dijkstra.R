#' Dijkstra's Shortest Path Algorithm
#'
#' Calculates the shortest path from an initial node to every other node in a graph
#' using Dijkstra's algorithm.
#'
#' @param graph A data.frame with three variables (v1, v2, w) representing edges
#'   from v1 to v2 with weight w
#' @param init_node A numeric scalar representing the starting node
#' @return A numeric vector with shortest distances from init_node to all other nodes
#' @references \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
#' @export
#' @examples
#' wiki_graph <- data.frame(
#'   v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'   v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'   w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)
#' )
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)

dijkstra <- function(graph, init_node) {
  # check the input df for graph structure
  stopifnot(is.data.frame(graph))
  stopifnot(all(c("v1", "v2", "w") %in% names(graph)))
  stopifnot(is.numeric(init_node), length(init_node) == 1)

  # Get all unique nodes
  nodes <- unique(c(graph$v1, graph$v2))

  # check the init_node exists in graph
  stopifnot(init_node %in% nodes)

  # Initialize distances
  dist <- rep(Inf, max(nodes))
  dist[init_node] <- 0
  visited <- rep(FALSE, max(nodes))

  # Dijkstra's algorithm
  for (i in 1:length(nodes)) {
    # Find unvisited node with minimum distance
    unvisited_nodes <- which(!visited & dist != Inf)
    if (length(unvisited_nodes) == 0) break

    current <- unvisited_nodes[which.min(dist[unvisited_nodes])]
    visited[current] <- TRUE

    # Update distances to neighbors
    neighbors <- graph[graph$v1 == current, ]
    for (j in 1:nrow(neighbors)) {
      neighbor <- neighbors$v2[j]
      weight <- neighbors$w[j]

      if (dist[current] + weight < dist[neighbor]) {
        dist[neighbor] <- dist[current] + weight
      }
    }
  }

  return(dist[nodes])
}
