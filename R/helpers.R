timestamp = function() Sys.time() %>% stringr::str_replace(' ', '--')

repo_hash = function(
  root = workflow::project_dir()
) system2(command = "git", args = glue::glue("-C {root} log -n1 --format=%H"))




