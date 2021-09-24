timestamp = function() Sys.time() %>% stringr::str_replace(' ', '--')

repo_hash = function() system2(command = "git", args = "log -n1 --format=%H")




