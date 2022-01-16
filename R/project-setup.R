genesee_packages = dplyr::tribble(~type, ~package, ~repo,
                                  'scRNA', 'GeneseeSC', 'amcdavid/GeneseeSC',
                                  'scVDJ', 'GeneseeSC', 'amcdavid/GeneseeSC',
                                  'RNA', 'GeneseeBulk', 'amcdavid/GeneseeBulk')

# how shall these be set otherwise? environmental variables? resource files?
git_server = "gitlab-public.circ.rochester.edu"
git_user = "rnaseq-analysts"

get_template_package = function(project_type){
  pkg_info = dplyr::left_join(tibble::tibble(type = project_type), genesee_packages, by = 'type')
  if(is.na(pkg_info$package)) stop("Unsupported `project_type` ", project_type)
  if(!requireNamespace(pkg_info$package)) stop("Install ", pkg_info$package, 'e.g. from ', pkg_info$repo, '.')
  pkg_info
}

get_config = function(Genesee.project, Genesee.type){
  if(is.null(Genesee.project)){
    Genesee.project = getwd()
  }
  cfg = try({
    yaml::read_yaml(file.path(Genesee.project, 'config.yml'))
  })
  if(inherits(cfg, 'try-error')) stop("Unable to load 'config.yml' from ", Genesee.project)
  if(!missing(Genesee.type) && !is.null(Genesee.type)) cfg$Genesee.type = Genesee.type
  cfg$Genesee.project = Genesee.project
  cfg$Genesee.version = as.character(packageVersion(cfg$Genesee.template.package))
  cfg
}


write_config = function(project_directory, authors, project_title, project_type, template_package) {
  # options
  config = list(Genesee.project = project_directory,
                Genesee.authors = paste(authors, collapse = ' and '),
                Genesee.title = project_title,
                Genesee.type = project_type,
                Genesee.template.package = template_package,
                Genesee.version = as.character(packageVersion(template_package))
  )
  yaml::write_yaml(config, file.path(project_directory, 'config.yml'))
  # may not need to write all of these to global R options, but Genesee.project would be nice
  do.call(options, config)
}


setup_renv = function(project_directory) {
  renv::activate(project_directory)
  # perhaps also
  # copy down .Renviron which sets cache path, eg ?renv::paths
  # RENV_PATHS_CACHE
}



setup_git = function(project_title){
  if(!dir.exists('.git')){
  git2r::init()
  git2r::checkout(branch = 'main', create = TRUE)
  git2r::remote_add(name = 'gitlab',
                    url = sprintf('git@%s:%s/%s.git', git_server, git_user, project_title))
  message('Initializing git')
  TRUE
  } else{
    message("git repo already exists at ", getwd())
    FALSE
  }
}

#' Interpolate and copy template files
#'
#' @param init_markdown `logical` Should markdown be copied?
#' @param init_sh `logical` Should shell script be copied?
#' @param init_gitfiles `logical` Should files managing git configuration be copied (eg .gitignore)
#' @param init_R `logical` should R scripts be copied
#' @inheritParams make_sample_sheet
#'
#' @return `logical` upon success
#' @export
#'
interpolate_and_copy = function(project_directory = NULL, init_markdown = TRUE, init_sh = TRUE, init_gitfiles = TRUE, init_R = TRUE, type = NULL){
  config = get_config(project_directory, type)
  type = config$Genesee.type
  project_directory = config$Genesee.project
  template_package = config$Genesee.template.package
  if(dir.exists('.git')){
    status = git2r::status()
    git_dirty = unlist(status)
  } else{
    git_dirty = character(0)
  }
  if (init_markdown) {
    mdf = list.files(system_file_ext(stringr::str_c(type, '_markdown/'), package = template_package), full.names = TRUE)
    lapply(mdf, function(x) interpolate_and_copy_file(x, project_directory, config, git_dirty = git_dirty))
  }

  if(init_sh){
    shf = list.files(system_file_ext(stringr::str_c(type, '_sh/'), package = template_package), full.names = TRUE)
    lapply(shf, function(x) interpolate_and_copy_file(x, project_directory, config, git_dirty = git_dirty))
  }

  if(init_gitfiles){
    ghf = list.files(system_file_ext(stringr::str_c(type, '_git/'), package = template_package), full.names = TRUE)
# rewrite _ to -> .
    lapply(ghf, function(x) interpolate_and_copy_file(x, project_directory, config, rename_ = TRUE,  git_dirty = git_dirty))
  }

  if(init_R){
    rf = list.files(system_file_ext(stringr::str_c(type, '_R/'), package = template_package), full.names = TRUE)
    lapply(rf, function(x) interpolate_and_copy_file(x, project_directory, config, git_dirty = git_dirty))
  }
  TRUE
}

interpolate_and_copy_file = function(src, dest, config, rename_ = FALSE, git_dirty){
  inf = readLines(src)
  g = purrr::map_chr(inf, function(line) glue::glue_data(config, line, .open = '{{', .close = '}}', .trim = FALSE))
  output = sub('^_', '.', basename(src))
  if(output %in% git_dirty) stop("Will not overwrite ", output, " with uncommitted changes. Stash or commit this file.")
  writeLines(g, file.path(dest, output))
}

is_packagelike = function(path){
  desc_file = file.path(path, 'DESCRIPTION')
  file.exists(desc_file) && stringr::str_detect(desc::desc(file = desc_file)$get_field('Package'), 'Genesee')
}

create_directory = function(path){
  if (!dir.exists(path)) {
    message('Creating ', path)
    dir.create(path)
  }
}

#' Initialize a Genesee project
#'
#' This creates a set of directories, activates `renv`, copies down templated
#' shell scripts.
#' The next step is to run make_sample_sheet.
#'
#' @param genesee_root `character` naming a containing all genesee projects
#' @param investigator `character` naming PI of project
#' @param project_title `character` naming project
#' @param init_templates `logical` Should template markdown, shell scripts, etc, be copied?
#' @param use_renv `logical` Should renv be initialized?
#' @param project_type `character`, one of `scRNA`, `RNA` or `scVDJ`
#' @param init_git `logical` should a git repo be initialized?
#' @param authors `character` containing the authors/editors of the project
#' @param force `logical` force creation of a project
#' @param navigate_rawdata `logical` should we shift focus to the rstudio terminal opened on the rawdata directory
#'
#' @return TRUE on success
#' @export
genesee_skeleton = function(genesee_root,
                              investigator,
                              project_title,
                              init_templates = TRUE,
                              init_git = TRUE,
                              use_renv = FALSE,
                              project_type = 'scRNA',
                              authors = 'default',
                              force = FALSE,
                              navigate_rawdata = TRUE) {
  project_type = match.arg(project_type, c('scRNA', 'scVDJ', 'RNA'))
  pkg_info = get_template_package(project_type)


  investigator_directory = file.path(genesee_root, investigator)
  project_directory = file.path(investigator_directory, project_title)

  if(is_packagelike(project_directory) && !force){
    stop(project_directory, ' looks like a path to a package.  Do you need to set project_title to a new directory?  Set force=TRUE if you are sure you want to make a project here.')
  }

  rawdata_directory = file.path(project_directory, 'rawdata')
  refined_directory = file.path(project_directory, 'refined')
  create_directory(investigator_directory)
  create_directory(project_directory)
  create_directory(rawdata_directory)
  create_directory(refined_directory)

  if(use_renv){
    setup_renv(project_directory)
    message('Initializing renv')
  }

  write_config(project_directory, authors, project_title, project_type, pkg_info$package)

  if (init_templates){
    interpolate_and_copy(project_directory)
  }
  setwd(project_directory)

  if (init_git) setup_git(project_title)




  message("Now symlink or copy raw data to ",
          rawdata_directory,
          ' and run make_sample_sheet.')

  if(rstudioapi::isAvailable() && navigate_rawdata){
    tid = rstudioapi::terminalCreate()
    rstudioapi::terminalActivate(tid)
    rstudioapi::terminalSend(tid, sprintf('cd %s\n', rawdata_directory))
    rstudioapi::terminalSend(tid, 'echo "symlink or copy raw data here"\n')
  }
  invisible(project_directory)
}

get_cellranger_molecule_info = function(project_directory) {
  tenx_cellranger_outputs = list.files(file.path(project_directory, 'rawdata'), full.names = TRUE)
  ss_list = lapply(seq_along(tenx_cellranger_outputs), function(i) {
    molecule_h5 = list.files(
      tenx_cellranger_outputs[i],
      full.names = TRUE,
      recursive = TRUE,
      pattern = 'molecule_info.h5'
    )
    data.frame(molecule_h5, rawdata_batch = i)
    #data.frame(molecule_h5 = file.path('rawdata', molecule_h5), rawdata_batch = i)
  })
  # keep = grep('outs/molecule_info.h5', molecule_h5)
  dplyr::bind_rows(ss_list)
}

get_vdj_contigs = function(project_directory) {
  tenx_cellranger_outputs = list.files(file.path(project_directory, 'rawdata'), full.names = TRUE)
  ss_list = lapply(seq_along(tenx_cellranger_outputs), function(i) {
    contig = list.files(
      tenx_cellranger_outputs[i],
      full.names = TRUE,
      recursive = TRUE,
      pattern = 'all_contig_annotations.csv'
    )
    data.frame(contig, rawdata_batch = i)
    #data.frame(molecule_h5 = file.path('rawdata', molecule_h5), rawdata_batch = i)
  })
  # keep = grep('outs/molecule_info.h5', molecule_h5)
  dplyr::bind_rows(ss_list)
}


#' Make a sample sheet reflecting the samples apparent in rawdata
#'
#' The sample sheet `all_samples_files.csv` maps between sequencing runs and
#' other covariates.
#'
#' At the moment, Cellranger mRNA (`type = 'scRNA'`) and Cellranger VDJ (`type = 'scVDJ'`) are supported.
#'
#' @param project_directory directory of the Genesee project. If set to `NULL` then will use working directory as long as a `config.yml`` is present.
#' @param overwrite `logical` should an existing sheet be overwritten?
#' @param type `character` The type of project.  One of `scRNA` and `scVDJ`.  If set to `NULL` then will attempt to read from `config.yml`.
#' @param write `logical` should the sample sheet be written to file, or just returned for further messaging
#' @return path of sample sheet (for additional editing) or sample sheet itself
#' @import stringr
#' @importFrom utils write.csv packageVersion write.table
#' @importFrom methods as
#' @export
make_sample_sheet = function(project_directory = NULL, write = TRUE, overwrite = FALSE, type = NULL) {
  cfg = get_config(Genesee.project = project_directory, Genesee.type = type)
  project_directory = cfg$Genesee.project
  type = cfg$Genesee.type
  ss_loc = file.path(project_directory, "all_samples_file.csv")
  if(file.exists(ss_loc) && write && !overwrite){
    stop("Will not overwrite sample sheet unless overwrite = TRUE.")
  }

  if(type == 'scRNA') {
    sample_sheet = get_cellranger_molecule_info(project_directory)
  } else if(type == 'scVDJ'){
    sample_sheet = get_vdj_contigs(project_directory)
  }

  sample_sheet$library_id = seq_len(nrow(sample_sheet))
  sample_sheet$treatment = NA_character_
if(type == 'scRNA'){
  #reorder
  sample_sheet = dplyr::select(sample_sheet, 'library_id', 'molecule_h5', dplyr::everything())
}
  if(write){
  write.csv(
    sample_sheet,
    file = ss_loc,
    row.names = FALSE
  )
  message('Wrote sample sheet to ',
          ss_loc)
  message('Edit file to fix covariates (at least treatment!) and then run_cellranger_aggr.sh')
  if(rstudioapi::isAvailable()){
    rstudioapi::navigateToFile(ss_loc)
  }
  return(ss_loc)
  } else{
    message('Edit sample sheet and write to ',
            ss_loc)
    return(sample_sheet)
  }
}
