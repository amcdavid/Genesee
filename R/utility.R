# From pkgload:::shim_system.file
# needed when calling system.file onto other devtools loaded packages...
#' @inherit pkgload:::system.file
#' @export
system_file_ext = function (..., package = "base", lib.loc = NULL, mustWork = FALSE) {
  if (!(package %in% devtools::dev_packages())) {
    base::system.file(..., package = package, lib.loc = lib.loc,
                      mustWork = mustWork)
  }
  else {
    pkg_path <- find.package(package)
    files_inst <- file.path(pkg_path, "inst", ...)
    present_inst <- file.exists(files_inst)
    files_top <- file.path(pkg_path, ...)
    present_top <- file.exists(files_top)
    files <- files_top
    files[present_inst] <- files_inst[present_inst]
    files <- files[present_inst | present_top]
    if (length(files) > 0) {
      normalizePath(files, winslash = "/")
    }
    else {
      if (mustWork) {
        stop("No file found", call. = FALSE)
      }
      else {
        ""
      }
    }
  }
}

#' Return mtime if file exists
#'
#' @param file length one `character`
#' @export
#' @examples
#' file_mtime_exists("Doesn't exist")
#' file_mtime_exists(system.file("DESCRIPTION", package = 'Genesee'))
file_mtime_exists = function(file){
  if(is.null(file) || !file.exists(file)) return(file)
  else file.mtime(file)
}

##' Selectively muffle warnings based on output
##'
##' @param expr an expression
##' @param regexp a regexp to be matched (with str_detect)
##' @return the result of expr
##' @export
##' @examples
##' Genesee:::hushWarning(warning('Beware the rabbit'), 'rabbit')
##' Genesee:::hushWarning(warning('Beware the rabbit'), 'hedgehog')
hushWarning = function(expr, regexp){
  withCallingHandlers(expr, warning=function(w){
    if(grepl(regexp, conditionMessage(w))) invokeRestart("muffleWarning")
  })
}

convert_config = function(path, overwrite = FALSE){
  # convert fields in an old Config to a New Config
}


#' Splice in default arguments into a function
#'
#' Arguments in ... supercede in case of collisions with `extra`
#' @param f `function`
#' @param ... key-value (named) arguments
#' @param extra named `list` of default arguments
#'
#' @return  value of `f` with supplied arguments
#' @export
#'
#' @examples
#' call_intercalate(sum, 3, 4, NA, extra = list(na.rm = TRUE))
#' call_intercalate_left(sum, 3, NA, na.rm = FALSE, extra = list(na.rm = TRUE))
#' call_intercalate_right(sum, 3, NA, na.rm = FALSE, extra = list(na.rm = TRUE))
#' meld_list_left(list(A=1, B=2), list(A = 0))
call_intercalate = function(f, ..., extra){
  nargs = meld_list_left(list(...), extra)
  if(length(nargs) != (length(list(...)) + length(extra))) warning("Duplicated arguments")
  do.call(f, nargs)
}

#' @describeIn call_intercalate don't warn with collision
#' @export
call_intercalate_left = function(f, ..., extra){
  nargs = meld_list_left(list(...), extra)
  do.call(f, nargs)
}

#' @describeIn call_intercalate arguments in `extra` take presidence
#' @export
call_intercalate_right = function(f, ..., extra){
  nargs = meld_list_left(extra, list(...))
  do.call(f, nargs)
}

#' @describeIn call_intercalate combine lists, preferentially taking elements from x if there are duplicate names
#' @param x list
#' @param y list
#' @export
meld_list_left = function(x, y){
  unite = c(x, y)
  dups = nchar(names(unite)) & duplicated(names(unite))
  unite[!dups]
}
