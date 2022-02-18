render_updir = function(..., devtools_pkg_paths){
  withr::with_dir('..', {
    purrr::map(devtools_pkg_paths, devtools::load_all)
    rmarkdown::render(...)
  })
}


#' Call [rmarkdown::render()] as a new slurm job
#'
#' @param devtools_pkgs `character` names of packages that are currently loaded with devtools
#' (and should be loaded on slurm instance).  The paths to these packages will be passed on down.
#' @param ... arguments passed to [rmarkdown::render()]
#' @inherit rslurm::slurm_call
#' @inheritDotParams rmarkdown::render
#' @export
#'
#' @examples
#' \dontrun{
#' create_exampleproject(skeleton_args = list(authors = 'you and me', project_type = 'scRNA', investigator = 'alligator', project_title = 'schit', navigate_rawdata = FALSE))
#' render_batch(slurm_options = list(time = "1:00:00", "mem-per-cpu" = "16gb", partition = "amcdavid", "cpus-per-task" = 1),
#' input = "01qc.Rmd",
#' params = list(tenx_root = NULL, tenx_h5 = 'scratch/AGG1/raw_feature_bc_matrix.h5', auto_filter = FALSE,
#' output_root='refined/01qc_nofilter', batch_var = 'tissue_source', citeseq_str = '_TotalA'),
#' output_file = '01qc_nofilter', output_dir = 'reports', quiet = TRUE)
#' }
render_batch = function(..., devtools_pkgs = devtools::dev_packages(), global_objects = NULL, pkgs = rev(.packages()), slurm_options = list()){
  devtools_pkg_paths = purrr::map_chr(devtools_pkgs, ~ stringr::str_remove(system.file(package = .x), "inst.?$"))
  rslurm::slurm_call(render_updir,
                     params = meld_list_left(list(...), list(devtools_pkg_paths = devtools_pkg_paths)),
                     global_objects = global_objects, pkgs = pkgs, slurm_options = slurm_options)
}

#' @describeIn render_batch call rmarkdown::render using sbatch, or not, ignoring sbatch specific arguments
#' @param use_sbatch `logical` whether to use [render_batch()] or [rmarkdown::render()]
#' @export
render = function(use_sbatch = FALSE, ...){
  if(use_sbatch){
    render_batch(...)
  } else{
    dots = list(..., envir = globalenv())
    if(any(nchar(names(dots))==0)) stop("Sorry, I can't figure out how to pass positional arguments here. All arguments must be named.")
    dots = dots[setdiff(names(dots), names(formals(render_batch)))]
    do.call(rmarkdown::render, dots)
  }
}
