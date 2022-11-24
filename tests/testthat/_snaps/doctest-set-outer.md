# Example: set-outer

    Code
      set_outer_borders(ht2)
    Output
                                   ┌───────────────────┐
                                   │       1         1 │
                                   │       2         2 │
                                   │       3         3 │
                                   └───────────────────┘
      
      Column names: a, b

---

    Code
      set_outer_borders(ht2, 2:3, 1:2)
    Output
                                           1         1  
                                   ┌───────────────────┐
                                   │       2         2 │
                                   │       3         3 │
                                   └───────────────────┘
      
      Column names: a, b

