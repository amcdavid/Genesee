#' Create a local project for testing purposes
#'
#' Deleted after the environment named in `env` goes out of scope.
#' @param dir `character` naming directory for local project
#' @param env `environment` to clean up after
#' @param skeleton_args `list` passed to `genesee_skeleton`
#'
#' @return `character` pointing to root of created project
create_local_project = function(dir = tempdir(), env = parent.frame(), skeleton_args = list()){
  oldwd = getwd()
  withr::defer({
    setwd(oldwd)
    unlink(project_dir, recursive = TRUE)
  }, envir = env)

  project_dir = call_intercalate(genesee_skeleton, genesee_root = dir, extra = skeleton_args)
  setwd(project_dir)
}

#' @describeIn create_local_project create a project and copy in some additional files (eg example data)
create_exampleproject = function(dir = tempdir(), env = parent.frame(), skeleton_args = list(project_type = 'scRNA')){
  project_dir = create_local_project(dir, env, skeleton_args)
  pkg_dir = sprintf('%s_projectexample/', skeleton_args$project_type)
  cfg = get_config(pkg_dir)
  pkg_contents = file.path(system.file(pkg_dir, package = cfg$Genesee.template.package), '.')
  file.copy(pkg_contents, project_dir, recursive = TRUE)
  # only contents, not leading directory pkg_dir.
  # Can't figure out how to get file.copy to do this
  # system2('cp', args = c('-R', shQuote(pkg_contents), shQuote(project_dir)))
}

#' @describeIn create_local_project create and destroy files with a prefix
#' @param prefix prefix of files to be destroyed on exit
create_temp_prefix = function(prefix = tempfile(), env = parent.frame()){
  withr::defer({
    unlink(paste0(prefix, '*'), expand = TRUE)
  }, envir = env)
  prefix
}


#' "Funcionalize" parameters in a knitr script
#'
#' @param name `character` name of function (name of script)
#' @param pars `list` of knitr parameters as returned by `knitr::knit_params`
#'
#' @return `function` with formals set according to `pars` and a body with a usage message.
generate_knitr_fun = function(name, pars){
  kv_list = list()
  for(p in pars){
    if(is.null(p$value)){
      kv_list[p$name] = list(NULL)
      } else{
      kv_list[[p$name]] = p$value
      }
  }
  f = function() {}
  body(f) = substitute(message(msg), list(msg = glue::glue('Usage: rmarkdown::render("{name}", params = {deparse1(kv_list)})')))
  formals(f) = kv_list
  f
}

#' Write R script that functionalizes the knitr templates for a project type
#'
#' The functions will be appended to a file in `<dev_dir>/scaffold_<project_type>.R`.
#' The roxygen skeleton should be modified as necessary to keep in date with the arguments.
#' @param dev_dir `character` naming path to write the R scripts
#' @param project_type `character` project type to write
#'
#' @return
#' @export
build_markdown_scaffold = function(dev_dir = getwd(), project_type = 'scRNA'){
  path_to_write = file.path(dev_dir, 'R', sprintf('scaffold_%s.R', project_type))
  on.exit(close(con))
  con = file(path_to_write, open = 'a')
  proj_dir = create_local_project(skeleton_args = list(authors = 'you and me', project_type = project_type, investigator = 'alligator', project_title = 'schit'))
  markdowns = list.files(proj_dir, pattern = '*.Rmd')
  pars = list()
  writeLines(sprintf('## Auto-generated on %s\n', date()), con)
  for(m in markdowns){
    script = readLines(m)
    pars[[m]] = knitr::knit_params(script)
    this_fun = generate_knitr_fun(m, pars[[m]])
    writeLines(glue::glue("`{m}` = {deparse1(this_fun)}\n"), con)
    writeLines("\n", con)
  }
  message("Now update documentation in ", path_to_write)
}
