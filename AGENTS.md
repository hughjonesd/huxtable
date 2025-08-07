At the end of any task, if you've made changes, run the tests in tests/testthat 
to make sure everything is still working. If there are errors due to your code, 
fix them before completing. You don't have to run every test; in particular those
in test-yy-end-to-end.R and test-zz-fuzz.R may take a long time so you may want 
to skip those, or even just run a single test file if you're confident. (Github 
will run a full suite of tests later.) But if you make big architectural changes,
run all or almost all tests. You can filter out fuzz tests using:
`devtools::test(filter = "^(?!.*fuzz)", perl = TRUE)`.

Don't change the API without asking first.

If you write a new function, give it Roxygen documentation using the `@noRd` tag.
Keep it simple and developer-focused. Similarly, if you edit an existing internal
function, take the chance to document it.

Not all packages in Suggests: in the DESCRIPTION file are installed. You
can install them if you need to. But, if tests break because a suggested
package is not installed, then that is a bug and you should probably fix it by
using `skip_if_not_installed()` - in a separate PR if possible. 

If you want to build vignettes, you'll need the R.rsp package. Alternatively,
don't build vignettes when you build the package, by passing the 
`--no-build-vignettes` flag to `devtools::build()`.

Style your code with scripts/style.R before preparing a pull request.

If you make user-visible changes, add a note to NEWS.md. Follow the formatting
of the existing entries. Run `devtools::document()` afterwards, because the news
is copied into the help file `?huxtable-news`.
