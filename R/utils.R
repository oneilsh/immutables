
# a fancy recursive function that returns two data frames for building an igraph object out of;
# takes a fingertree, returns a list of edge_dataframe and node_dataframe with edge and node information
get_graph_df <- function(t) {
  
  EDGE_STACK <- rstack()
  NODE_STACK <- rstack()
  
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
          subthing <- t[[subthing_name]]
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



# plotting a tree with igraph, using the get_graph_df() helper
plot_tree <- function(t1, vertex.size = 4, edge.width = 1, label_edges = FALSE, title = NULL, ...) {
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
  
  g <- graph_from_data_frame(t1_edge_df, vertices = unique(t1_node_df), directed = TRUE)
  par(lheight = 0.3)
  plot(g, 
       layout = layout_as_tree(g), 
       #layout = layout.reingold.tilford, 
       vertex.label=V(g)$label, 
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


# takes a list of elements such as: a, b, c, d, e, f, g
# returns a list of nodes such as: list(Node3(a, b, c), Node2(d, e), Node2(f, g))
# a pretty inefficient recursive implementation at the moment
# this was the name used in the paper, but it's pretty generic
nodes(l) %::% list : list
nodes(l) %as% {
  if(length(l) == 2) { return(list(
    Node2( l[[1]], l[[2]] )
  ))}
  if(length(l) == 3) { return(list(
    Node3( l[[1]], l[[2]], l[[3]] )
  ))}
  if(length(l) == 4) { return(list(
    Node2( l[[1]], l[[2]] ),
    Node2( l[[3]], l[[4]] )
  ))}
  
  first = Node3( l[[1]], l[[2]], l[[3]] )
  rest = nodes(l[4:length(l)])
  rest = list.prepend(rest, first)
  return(rest)
}




as.FingerTree(l) %::% . : FingerTree
as.FingerTree(l) %as% {
  l <- as.list(l)
  t <- Empty()
  for(el in l) {t <- add_right(t, el)}
  return(t)
}


as.FingerTree(l, v) %::% . : . : FingerTree
as.FingerTree(l, v) %as% {
  l <- as.list(l)
  v <- as.list(v)
  if(length(l) != length(v)) {
    stop("length of entries and values lists given to as.FingerTree not equal.")
  }
  t <- Empty()
  for(i in 1:length(l)) {
    el <- l[[i]]
    value <- v[[i]]
    attr(el, "value") <- value
    t <- add_right(t, el)
  }
  return(t)
}
