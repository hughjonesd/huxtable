
Read design.md to get an overview of huxtable's architecture. Read agent-notes.md
to read the notes of previous llms.

Use short branch names. Start with "feature", "bugfix", "refactor" or "chore". 

Don't change the API without asking first.

If you find a user-visible bug, then consider writing a test for it. Title the
test "Bugfix: ..." and put it in the most relevant existing test file. Then 
use it to confirm you've fixed the bug.

If you write a new function, give it Roxygen documentation. If it's internal,
use the `@noRd` tag, and keep it simple and developer-focused. Similarly, 
if you edit an existing internal function, take the chance to document it.

If you understand more about the architecture of huxtable, update 
`design.md`. For more general notes that might be useful to you or future llms
working on the project, write in `agent-notes.md`. Don't forget to add any
suggested changes to the PR.

If you've written user-visible Roxygen, run `devtools::document()`. You can 
ignore errors due to missing packages ("flextable", "lmtest") unless you're working
on those particular parts of the system. If you are, then just install the
missing packages.

At the end of any task, if you've made changes, run the tests in tests/testthat 
to make sure everything is still working. If there are errors due to your code, 
fix them before completing. You don't have to run every test; in particular those
in test-yy-end-to-end.R and test-zz-fuzz.R may take a long time so you may want 
to skip those, or even just run a single test file if you're confident. (Github 
will run a full suite of tests later.) But if you make big architectural changes,
run all or almost all tests. You can filter out fuzz tests using:
`devtools::test(filter = "^(?!.*fuzz)", perl = TRUE)`.

You can install quarto, typst, or latex by running the ./install-*.sh files in your
home directory. They are not installed automatically to save setup time. Install
them if you need to test the relevant features.

Not all packages in Suggests: in the DESCRIPTION file are installed. You
can install them if you need to. But, if tests break because a suggested
package is not installed, then that is a bug and you should probably fix it by
using `skip_if_not_installed()` - in a separate PR if possible. 

If you need to install big, well-known packages like dplyr, try to install them 
via  `apt-get --no-install-recommends install r-cran-dplyr` before using 
`install.packages()` from R.

If you want to build vignettes, you'll need the R.rsp package. Alternatively,
don't build vignettes when you build the package, by passing the 
`--no-build-vignettes` flag to `devtools::build()`.

If you make important user-visible changes, add an item to NEWS.md. Follow the formatting
of the existing entries. Run `devtools::document()` afterwards, because the news
is copied into the help file `?huxtable-news`. Do not update NEWS.md for unimportant changes, 
like tweaks to documentation, or if the broad change is already mentioned.
