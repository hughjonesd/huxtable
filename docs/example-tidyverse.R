
library(magrittr)

ht <- hux(
        Employee = c('John Smith', 'Jane Doe', 'David Hugh-Jones'),
        Salary = c(50000, 50000, 40000),
        add_colnames = TRUE
      )

ht                                        %>%
      set_bold(1, everywhere, TRUE)       %>%
      set_bottom_border(1, everywhere, 1) %>%
      set_align(everywhere, 2, 'right')   %>%
      set_right_padding(10)               %>%
      set_left_padding(10)                %>%
      set_width(0.35)                     %>%
      set_number_format(2)
