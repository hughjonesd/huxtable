---
title: "Quarto tester"
format: html
keep-md: true
---



# Kable table

Reference to @tbl-kable.


::: {#tbl-kable .cell tbl-cap='Kable Cars'}

```{.r .cell-code}
library(knitr)
knitr::kable(head(cars))
```

::: {.cell-output-display}
| speed| dist|
|-----:|----:|
|     4|    2|
|     4|   10|
|     7|    4|
|     7|   22|
|     8|   16|
|     9|   10|
:::
:::


# Hux table

Reference to @tbl-hux.


::: {#tbl-hux .cell tbl-cap='Huxtable Jams'}

```{.r .cell-code}
library(huxtable)
jams
```

::: {.cell-output-display}
preservedeb033b68d6f0d28
:::
:::


# Hux table, own label and caption

Reference to @tbl-hux-internal.


::: {.cell}

```{.r .cell-code}
caption(jams) <- "Caption from within R"
label(jams) <- "tbl-hux-internal"
jams
```

::: {.cell-output-display}
preserve8bc75f07408f7a68
:::
:::



# Hux table, what if we did both?

R label: @tbl-hux-internal-2. Quarto label: @tbl-hux-quarto


::: {#tbl-hux-quarto .cell tbl-cap='Caption from quarto'}

```{.r .cell-code}
caption(jams) <- "Caption from within R"
label(jams) <- "tbl-hux-internal-2"
jams
```

::: {.cell-output-display}
preserveab4086e45a1ec6e7
:::
:::




# Hux table, complicated

R label: @tbl-hux-complex


::: {#tbl-hux-complex .cell tbl-cap='Caption from quarto, complex table'}

```{.r .cell-code}
jams |> 
  set_caption("Caption from R, complex table") |> 
  set_background_color(2, 2, "red") |> 
  set_bold(1, 2) |> 
  set_colspan(3, 1, 2) |> 
  set_bottom_border(1, everywhere) |> 
  set_width(0.9)
```

::: {.cell-output-display}
preservee8a3d87a45ea262f
:::
:::
