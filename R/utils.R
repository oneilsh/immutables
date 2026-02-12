
#' Build graph data frames for a finger tree
#'
#' @param t FingerTree.
#' @return A list with edge and node data frames for igraph plotting.
#' @examples
#' t <- tree_from(letters[1:4])
#' gdf <- get_graph_df(t)
#' names(gdf)
#' nrow(gdf[[1]])
#'
#' # Works for deeper trees too
#' t2 <- tree_from(letters[1:10])
#' gdf2 <- get_graph_df(t2)
#' nrow(gdf2[[2]])
#' @export
# Runtime: O(n) over tree nodes/elements.
get_graph_df <- function(t) {
  
  EDGE_STACK <- rstack()
  NODE_STACK <- rstack()
  
# Runtime: O(n) worst-case in relevant input/subtree size.
  add_edges <- function(t, path) {
    
    if(is_structural_node(t) && t %isa% Empty) {
      parentid <- path
      parenttype <- class(t)[1]
      parentlabel <- ""
      NODE_STACK <<- insert_top(NODE_STACK, list(node = parentid, type = parenttype, label = parentlabel))
    }
    else if(!is_structural_node(t)) {
      parentid <- path
      parenttype <- "Element"
      parentlabel <- paste0(as.character(unlist(t)), collapse = ", ")
      NODE_STACK <<- insert_top(NODE_STACK, list(node = parentid, type = parenttype, label = parentlabel))
    } else {
      if(!is.null(names(t))) {
        # rev() here and below determines the order of addition to the data and thus (apparently)
        # the node ordering 
        for(subthing_name in rev(names(t))) {
          subthing <- .subset2(t, subthing_name)
          parentid <- path
          childid <- paste(path, subthing_name, sep = ":")
          EDGE_STACK <<- insert_top(EDGE_STACK, list(parent = parentid, child = childid, label = subthing_name))
          
          parenttype <- class(t)[1]
          parentlabel <- ""
          NODE_STACK <<- insert_top(NODE_STACK, list(node = parentid, type = parenttype, label = parentlabel))
          
          add_edges(subthing, childid)
        }
        
      } else {
        index <- 1
        for(subthing in rev(t)) {
          parentid <- path
          childid <- paste(path, index, sep = ":")
          EDGE_STACK <<- insert_top(EDGE_STACK, list(parent = parentid, child = childid, label = index))
          
          parenttype <- class(t)[1]
          parentlabel <- ""
          NODE_STACK <<- insert_top(NODE_STACK, list(node = parentid, type = parenttype, label = parentlabel))
          
          add_edges(subthing, childid)
          index <- index + 1
        }
      }
    }
    return(invisible())
  }
  
  add_edges(t, "root")
  
  return(list(
    as.data.frame(EDGE_STACK, stringsAsFactors = FALSE),
    as.data.frame(NODE_STACK, stringsAsFactors = FALSE)
  ))
}



#' Plot a finger tree with igraph
#'
#' @param t1 FingerTree.
#' @param vertex.size Vertex size passed to `igraph::plot.igraph`.
#' @param edge.width Edge width passed to `igraph::plot.igraph`.
#' @param label_edges Whether to draw edge labels.
#' @param title Optional plot title.
#' @param node_label Node label mode: value, type, both, or none.
#' @param ... Additional arguments passed to `igraph::plot.igraph`.
#' @examples
#' \dontrun{
#' t <- tree_from(letters[1:8])
#' plot_tree(t, title = "Finger tree")
#'
#' t2 <- tree_from(letters[1:12])
#' plot_tree(t2, node_label = "both", vertex.size = 8)
#' }
#' @export
# Runtime: O(n) to build graph structures prior to plotting.
plot_tree <- function(t1, vertex.size = 4, edge.width = 1, label_edges = FALSE, title = NULL,
                      node_label = c("value", "type", "both", "none"), ...) {
  t1_edge_df <- get_graph_df(t1)[[1]]
  t1_node_df <- get_graph_df(t1)[[2]]
  t1_node_df$color <- NA
  t1_node_df$color[t1_node_df$type == "Element"] <- "#ffffb3"
  t1_node_df$color[t1_node_df$type == "Digit"] <- "#8dd3c7"
  t1_node_df$color[t1_node_df$type == "Deep"] <- "#bebada"
  t1_node_df$color[t1_node_df$type == "Empty"] <- "#fb8072"
  t1_node_df$color[t1_node_df$type == "Single"] <- "#80b1d3"
  t1_node_df$color[t1_node_df$type == "Node3"] <- "#fdb462"
  t1_node_df$color[t1_node_df$type == "Node2"] <- "#b3de69"
  
  vertices_df <- unique(t1_node_df)
  g <- graph_from_data_frame(t1_edge_df, vertices = vertices_df, directed = TRUE)
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(lheight = 0.3)
  node_label <- match.arg(node_label)
  if(node_label == "none") {
    vlabels <- rep("", vcount(g))
  } else if(node_label == "type") {
    vlabels <- V(g)$type
  } else if(node_label == "both") {
    vlabels <- ifelse(V(g)$label == "", V(g)$type, paste0(V(g)$type, "\n", V(g)$label))
  } else {
    vlabels <- V(g)$label
  }

  plot(g, 
       layout = layout_as_tree(g), 
       #layout = layout.reingold.tilford, 
       vertex.label=vlabels, 
       #vertex.color = as.integer(as.factor(V(g)$type)),
       vertex.size = vertex.size,
       edge.arrow.size = 0.4,
       asp = 0.4, 
       edge.arrow.mode = 0,
       edge.width = edge.width,
       edge.label = ifelse(label_edges, t1_edge_df$label, ""),
       main = title,
       #vertex.label.family = "Sans Serif",
       ...
  )
}


# convert a flat list into measured Node2/Node3 list for concatenation.
# Runtime: O(k), where k = length(l).
measured_nodes(l, monoids) %::% list : list : list
measured_nodes(l, monoids) %as% {
  if(length(l) == 2) { return(list(
    measured_node2( l[[1]], l[[2]], monoids )
  ))}
  if(length(l) == 3) { return(list(
    measured_node3( l[[1]], l[[2]], l[[3]], monoids )
  ))}
  if(length(l) == 4) { return(list(
    measured_node2( l[[1]], l[[2]], monoids ),
    measured_node2( l[[3]], l[[4]], monoids )
  ))}
  
  first = measured_node3( l[[1]], l[[2]], l[[3]], monoids )
  rest = measured_nodes(l[4:length(l)], monoids)
  rest = list.prepend(rest, first)
  return(rest)
}
