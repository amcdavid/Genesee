#' Split a matrix in a list of matrices by levels of a factor / integer
#'
#' This function also works with [SummarizedExperiment::SummarizedExperiment()] / [SingleCellExperiment::SingleCellExperiment()]
#' @param m `matrix`-like
#' @param i `factor`-like
#'
#' @return list of matrices of `length(unique(i))`
#' @export
#' @examples
#' m = matrix(1:12, nrow = 3)
#' s_col = split_matrix_col(m, c('A', 'A', 'B', 'B'))
#' s_row = split_matrix_row(m, c('B', 'A', 'B'))
#' scaled = lapply_attr(s_col, function(x) x/sum(x))
#' unsplit_matrix(scaled)
#' stopifnot(all.equal(unsplit_matrix(s_row), m))
split_matrix_col = function(m, i){
  if( length(dim(m)) != 2 ){
    stop('`m` must have two-dimensions.')
  }
  stopifnot(length(i) == ncol(m))
  si = split(seq_along(i), i)
  out = lapply(si, function(ii) m[,ii,drop = FALSE])
  structure(out, class = 'MatrixColList', original_idx = si)
}

#' @describeIn split_matrix_col lapply and preserve attributes.  Necessary for `unsplit_matrix`.
#' @inheritParams base::lapply
#' @export
lapply_attr = function(X, FUN, ...){
  out = lapply(X, FUN, ...)
  attributes(out) = attributes(X)
  out
}


#' @describeIn split_matrix_col split a matrix by row
#' @export
split_matrix_row = function(m, i){
  if( length(dim(m)) != 2 ){
    stop('`m` must have two-dimensions.')
  }
  stopifnot(length(i) == nrow(m))
  si = split(seq_along(i), i)
  out = lapply(si, function(ii) m[ii,,drop = FALSE])
  structure(out, class = 'MatrixRowList', original_idx = si)
}

#' @describeIn split_matrix_col reverse a split (cbind/rbind) and reordering
#' @export
unsplit_matrix = function(x){
  if(inherits(x, 'MatrixColList')){
    out = do.call(SingleCellExperiment::cbind, x)
    xi = do.call(c, attr(x, 'original_idx'))
    return(out[,order(xi)])
  } else if(inherits(x, 'MatrixRowList')){
    out = do.call(rbind, x)
    xi = do.call(c, attr(x, 'original_idx'))
    return(out[order(xi),])
  } else{
    stop('Expecting `MatrixColList` or `MatrixRowList`.')
  }
}
