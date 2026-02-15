
#' Build graph data frames for a finger tree
#'
#' @param t FingerTree.
#' @return A list with edge and node data frames for igraph plotting.
#' @examples
#' \dontrun{
#' t <- as_flexseq(letters[1:4])
#' gdf <- get_graph_df(t)
#' names(gdf)
#' }
#' @keywords internal
# Runtime: O(n) over tree nodes/elements.
get_graph_df <- function(t) {
  edge_rows <- list()
  node_rows <- list()

  add_edge_row <- function(parent, child, label) {
    edge_rows[[length(edge_rows) + 1L]] <<- list(
      parent = as.character(parent),
      child = as.character(child),
      label = as.character(label)
    )
  }

  add_node_row <- function(node, type, label) {
    node_rows[[length(node_rows) + 1L]] <<- list(
      node = as.character(node),
      type = as.character(type),
      label = as.character(label)
    )
  }
  
# Runtime: O(n) worst-case in relevant input/subtree size.
  add_edges <- function(t, path) {
    
    if(is_structural_node(t) && t %isa% Empty) {
      parentid <- path
      parenttype <- class(t)[1]
      parentlabel <- ""
      add_node_row(parentid, parenttype, parentlabel)
    }
    else if(!is_structural_node(t)) {
      parentid <- path
      parenttype <- "Element"
      parentlabel <- paste0(as.character(unlist(t)), collapse = ", ")
      add_node_row(parentid, parenttype, parentlabel)
    } else {
      if(!is.null(names(t))) {
        # rev() here and below determines the order of addition to the data and thus (apparently)
        # the node ordering 
        for(subthing_name in rev(names(t))) {
          subthing <- .subset2(t, subthing_name)
          parentid <- path
          childid <- paste(path, subthing_name, sep = ":")
          add_edge_row(parentid, childid, subthing_name)
          
          parenttype <- class(t)[1]
          parentlabel <- ""
          add_node_row(parentid, parenttype, parentlabel)
          
          add_edges(subthing, childid)
        }
        
      } else {
        index <- 1
        for(subthing in rev(t)) {
          parentid <- path
          childid <- paste(path, index, sep = ":")
          add_edge_row(parentid, childid, index)
          
          parenttype <- class(t)[1]
          parentlabel <- ""
          add_node_row(parentid, parenttype, parentlabel)
          
          add_edges(subthing, childid)
          index <- index + 1
        }
      }
    }
    return(invisible())
  }
  
  add_edges(t, "root")

  edge_df <- if(length(edge_rows) == 0L) {
    data.frame(parent = character(0), child = character(0), label = character(0), stringsAsFactors = FALSE)
  } else {
    do.call(rbind.data.frame, c(lapply(edge_rows, as.data.frame, stringsAsFactors = FALSE), list(stringsAsFactors = FALSE)))
  }
  node_df <- if(length(node_rows) == 0L) {
    data.frame(node = character(0), type = character(0), label = character(0), stringsAsFactors = FALSE)
  } else {
    do.call(rbind.data.frame, c(lapply(node_rows, as.data.frame, stringsAsFactors = FALSE), list(stringsAsFactors = FALSE)))
  }

  return(list(edge_df, node_df))
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
#' t <- as_flexseq(letters[1:8])
#' plot_tree(t, title = "Finger tree")
#'
#' t2 <- as_flexseq(letters[1:12])
#' plot_tree(t2, node_label = "both", vertex.size = 8)
#' }
#' @keywords internal
# Runtime: O(n) to build graph structures prior to plotting.
plot_tree <- function(t1, vertex.size = 4, edge.width = 1, label_edges = FALSE, title = NULL,
                      node_label = c("value", "type", "both", "none"), ...) {
  if(!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for plot_tree(). Install it with install.packages('igraph').")
  }

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
  g <- igraph::graph_from_data_frame(t1_edge_df, vertices = vertices_df, directed = TRUE)
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(lheight = 0.3)
  node_label <- match.arg(node_label)
  if(node_label == "none") {
    vlabels <- rep("", igraph::vcount(g))
  } else if(node_label == "type") {
    vlabels <- igraph::V(g)$type
  } else if(node_label == "both") {
    vlabels <- ifelse(igraph::V(g)$label == "", igraph::V(g)$type, paste0(igraph::V(g)$type, "\n", igraph::V(g)$label))
  } else {
    vlabels <- igraph::V(g)$label
  }

  plot(g, 
       layout = igraph::layout_as_tree(g), 
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
