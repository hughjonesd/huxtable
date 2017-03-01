
<!-- README.md is generated from README.Rmd. Please edit that file -->
huxtable is a package for creating HTML and LaTeX tables. It provides similar functionality to xtable, with a simple interface. Here's a quick example. Github overrides styling when it parses HTML in README files, so to see it in its full glory, check out [the website](http://hughjonesd.github.io/huxtable).

``` r
library(huxtable)

ht <- huxtable(a = 1:5, b = letters[1:5])
align(ht)[,2] <- 'right'
background_color(ht) <- 'yellow'
bottom_border(ht) <- c(1, 0, 0, 0, 1)
cat(to_html(ht))
```

<table class="huxtable" style="width: 100%; margin-left: auto; margin-right: auto;">
<col style="width: NA;">
<col style="width: NA;">
<tr>
<td style="vertical-align: top; text-align: center; border-width:0px 0px 1px 0px; border-style: solid; background-color: rgb(255, 255, 0); ">
1
</td>
<td style="vertical-align: top; text-align: right; border-width:0px 0px 1px 0px; border-style: solid; background-color: rgb(255, 255, 0); ">
a
</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: center; background-color: rgb(255, 255, 0); ">
2
</td>
<td style="vertical-align: top; text-align: right; background-color: rgb(255, 255, 0); ">
b
</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: center; background-color: rgb(255, 255, 0); ">
3
</td>
<td style="vertical-align: top; text-align: right; background-color: rgb(255, 255, 0); ">
c
</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: center; background-color: rgb(255, 255, 0); ">
4
</td>
<td style="vertical-align: top; text-align: right; background-color: rgb(255, 255, 0); ">
d
</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: center; border-width:0px 0px 1px 0px; border-style: solid; background-color: rgb(255, 255, 0); ">
5
</td>
<td style="vertical-align: top; text-align: right; border-width:0px 0px 1px 0px; border-style: solid; background-color: rgb(255, 255, 0); ">
e
</td>
</tr>
</table>
Or the same thing with a more dplyr-ish syntax:

``` r
library(huxtable)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

ht <- huxtable(a = 1:5, b = letters[1:5]) %>% 
      set_align(1:5, 2, 'right') %>% 
      set_background_color(1:5, 1:2, 'yellow') %>% 
      set_bottom_border(1:5, 1:2, c(1, 0, 0, 0, 1))

cat(to_html(ht))
```

<table class="huxtable" style="width: 100%; margin-left: auto; margin-right: auto;">
<col style="width: NA;">
<col style="width: NA;">
<tr>
<td style="vertical-align: top; text-align: center; border-width:0px 0px 1px 0px; border-style: solid; background-color: rgb(255, 255, 0); ">
1
</td>
<td style="vertical-align: top; text-align: right; border-width:0px 0px 1px 0px; border-style: solid; background-color: rgb(255, 255, 0); ">
a
</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: center; background-color: rgb(255, 255, 0); ">
2
</td>
<td style="vertical-align: top; text-align: right; background-color: rgb(255, 255, 0); ">
b
</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: center; background-color: rgb(255, 255, 0); ">
3
</td>
<td style="vertical-align: top; text-align: right; background-color: rgb(255, 255, 0); ">
c
</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: center; background-color: rgb(255, 255, 0); ">
4
</td>
<td style="vertical-align: top; text-align: right; background-color: rgb(255, 255, 0); ">
d
</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: center; border-width:0px 0px 1px 0px; border-style: solid; background-color: rgb(255, 255, 0); ">
5
</td>
<td style="vertical-align: top; text-align: right; border-width:0px 0px 1px 0px; border-style: solid; background-color: rgb(255, 255, 0); ">
e
</td>
</tr>
</table>
