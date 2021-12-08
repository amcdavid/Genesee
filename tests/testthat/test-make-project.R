## Add tests here eventually
## See https://github.com/r-lib/devtools/blob/master/tests/testthat/test-build-readme.R for examples of how to test file creation operations,etc

test_that('project_type crosswalk', {
  expect_equal(get_template_package('scRNA')$package, 'GeneseeSC')
  expect_error(get_template_package('foo'))
})

test_that('Create an scRNAseq project', {
  create_local_project(skeleton_args = list(authors = 'you and me', project_type = 'scRNA', investigator = 'alligator', project_title = 'schit',  navigate_rawdata = FALSE))
  local = readLines('01qc.Rmd')
  pkg = readLines(system.file('scRNA_markdown', '01qc.Rmd', package = 'GeneseeSC'))
  expect_true(any(stringr::str_detect(head(local), 'you and me')))
  expect_equal(tail(local), tail(pkg))
  expect_error(interpolate_and_copy(), 'commit')
})
