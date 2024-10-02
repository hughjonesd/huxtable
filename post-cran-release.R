

revdepcheck::revdep_reset()
v <- devtools::as.package('.')$version
newtag <- paste0('v', v, '-rc')
tags <- git2r::tags()
tags <- grep(newtag, names(tags), fixed = TRUE, value = TRUE)
tags <- sort(tags)
stopifnot(length(tags) > 0)
tag <- tags[length(tags)]
gh::gh("POST /repos/hughjonesd/huxtable/releases",
       name = v,
       tag_name = tag,
       body = sprintf("Version %s on CRAN", v))
