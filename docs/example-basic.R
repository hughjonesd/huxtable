
ht <- hux(
        Employee = c('John Smith', 'Jane Doe', 'David Hugh-Jones'),
        Salary = c(50000, 50000, 40000),
        add_colnames = TRUE
      )

bold(ht)[1,]           <- TRUE
bottom_border(ht)[1,]  <- 1
align(ht)[,2]          <- 'right'
right_padding(ht)      <- 10
left_padding(ht)       <- 10
width(ht)              <- 0.35
number_format(ht)      <- 2

ht
