#!/usr/local/bin/Rscript

library(rhub)
library(devtools)
library(git2r)
library(glue)
library(rstudioapi)


# Check version number is up to date----------------------------------------------------------------

v <- devtools::as.package('.')$version
if (grepl("9000", v)) stop('Still using development version. Use usethis::use_version()')


# Check git is up to date --------------------------------------------------------------------------

gdiff <- git2r::diff(git2r::repository())
if (length(gdiff) > 0) stop('Working tree differs from last commit, please make commits!')
# nb this may fail for some reason



# Build vignettes manually ----------------------------------------------

pdf_output_formats <- list(
  "design-principles.Rmd" = pdf_document(latex_engine = "xelatex"),
  "huxreg.Rmd"            = pdf_document(latex_engine = "xelatex"),
  "huxtable.Rmd"          = pdf_document(
                              latex_engine = "xelatex",
                              toc = TRUE,
                              toc_depth = 2
                            )
)
for (f in list.files("docs", pattern = "*.Rmd", full.names = TRUE)) {
  filename <- basename(f)
  message("Rendering ", f)
  filename_no_ext <- sub("\\.Rmd$", "", filename)
  rmarkdown::render(f,
                    output_format = "html_document",
                    output_dir    = "vignettes",
                    output_file   = paste0(filename_no_ext, "-html"))
  rmarkdown::render(f,
                    output_format = pdf_output_formats[[filename]],
                    output_dir    = "vignettes",
                    output_file   = paste0(filename_no_ext, "-pdf"))
}
setwd("vignettes")
knitr::knit("../docs/themes.Rhtml", "themes-html.html")
setwd("..")



# Run R CMD check ----------------------------------------------------------------------------------

# this automatically builds the package
chk <- devtools::check(
        env_vars = c('RSTUDIO_PANDOC' = '/Applications/RStudio.app/Contents/MacOS/pandoc'),
        document = FALSE,
        remote   = TRUE
      )


# manual section --------

# may not work; if so just run the script in the main window.
rstudioapi::jobRunScript("check-reverse-dependencies.R", exportEnv = "revdep_results")

# run checks
devtools::check_mac_release()

# asks manual questions
devtools::check_win_devel()

# asks manual questions
devtools::check_win_release()



# update CRAN-comments.md

# Tag new version

newtag <- paste0('v', v, '-rc')
tags <- git2r::tags()
tags <- grep(newtag, names(tags), fixed = TRUE, value = TRUE)
tags <- as.numeric(gsub(newtag, '', tags))
rc <- if (length(tags)) as.integer(max(tags) + 1) else 1L
newtag <- paste0(newtag, rc)
cat('\nTagging current version as: ', newtag, '\n')
tag(repository('.'), newtag, message = paste('CRAN release candidate for', v))
system2('git', c('push', '--tags'))



# now release!

devtools::release()




# Alternatively:
# Uncomment !CRAN lines in huxtable vignette and save
system("mkdir -p inst/doc")
devtools::build_vignettes()
# Copy built vignettes from doc/ to inst/doc:
system("cp doc/* inst/doc")
# Comment out !CRAN lines in huxtable vignette and save
# copy to inst/doc:
system("cp vignettes/huxtable.Rmd inst/doc")
# Do this to avoid R CMD check spotting newer files in vignettes than inst/doc:
system("touch inst/doc/*")
# Ensure there's a vignette index in build/vignettes.rds
# NB NOT Meta/vignettes.rds! devtools::build_vignettes() will put it there,
# you can move it
system("mv Meta/vignette.rds build")
system("rmdir Meta")
# Build a version for CRAN:
pkgbuild::build(clean_doc = FALSE, manual = TRUE, vignettes = FALSE)
# Check you have build/vignettes.rds in the tarfile
system("tar -ztvf ../huxtable_x.y.z.tar.gz | grep build")
# Submit via web form. (You could also run through the devtools::release()
# questions just to be safe!)

# after release:
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
