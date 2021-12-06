
#' @rdname textplot_entity
#' @export
textplot_entity <- function(x, ...){
  UseMethod("textplot_entity")
}

#' @export
textplot_entity.default <- function(x,...) {
  stop(
    "`textplot_entity()` is not defined for a '", class(x)[1], "'.",
    call. = FALSE
  )
}

#' @title Plot output of a named-entity extraction
#' @description Plot output of a named-entity extraction.
#' This plot highlight the text with the associated entity,
#' the words, the parts of speech tag and the dependency relationship between the words.
#' @param x a data.frame as returned by a call to \code{\link[udpipe]{udpipe}} containing 1 sentence
#' @param title character string with the title to use in the plot
#' @param subtitle character string with the title to use in the plot
#' @param vertex_color character with the color of the label of each node. Defaults to darkgreen.
#' @param edge_color character with the color of the edges between the nodes. Defaults to red.
#' @param size size of the labels in the plot. Defaults to 3.
#' @param base_family character passed on to \code{theme_void} setting the base font family
#' @param ... not used yet
#' @return an object of class ggplot
#' @seealso \code{\link[spacyr]{spacyr}}
#' @export
#' @examples
#' \dontshow{
#' if(require(spacyr) && require(ggplot2) && require(igraph))
#' \{
#' }
#' library(spqcyr)
#' library(ggplot2)
#' library(igraph)
#' \donttest{
#' txt1 <- c(doc1 = "I would have accepted without question the information that Gatsby sprang from the swamps of Louisiana or from the lower East Side of New York.",
#'           doc2 = "I graduated from New Haven in 1915, just a quarter of a century after my father, and a little later I participated in that delayed Teutonic migration known as the Great War.")
#'
#' entity_df <- entity_consolidate(spacy_parse(txt1), concatenator = " ")
#' textplot_entity(entity_df)
#' }
#' @export
textplot_entity.data.frame <- function(x, doc_id="doc_id", token="token", entity_type="entity_type") {
  doc_id <- x[,rlang::eval_tidy(rlang::enquo(doc_id), data=x),drop=F]
  tokens <- x[,rlang::eval_tidy(rlang::enquo(token), data=x),drop=F]
  entity_type <- x[,rlang::eval_tidy(rlang::enquo(entity_type), data=x)]
  entity_factor <- entity_type %>% as.factor %>% relevel("")
  entity_color <- c("#FFFFFF",viridisLite::turbo(nlevels(entity_factor)-1))
  entity_factor <- entity_type %>% as.factor %>% relevel("")
  entity_bg_color <- c("#FFFFFFFF",viridisLite::turbo(nlevels(entity_factor)-1))

  clean_x <- data.frame(doc_id, token, entity_type, color=entity_bg_color[entity_factor]) %>%
    dplyr::mutate(
      token_color = ifelse(entity_type=="",
                           token,
                           paste0("<span style='background_color:",entity_bg_color,";'>", token, "</span>") ))

  str_view_widget( str_c(clean_x$token_color, collapse = " "))


}


