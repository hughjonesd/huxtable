# Example: spans

    Code
      set_rowspan(letter_hux, 1, 1, 2)
    Output
                                 ┌───────┬───────┬───────┐
                                 │ A     │ D     │ G     │
                                 │       ├───────┼───────┤
                                 │       │ E     │ H     │
                                 ├───────┼───────┼───────┤
                                 │ C     │ F     │ I     │
                                 └───────┴───────┴───────┘
      
      Column names: V1, V2, V3

---

    Code
      set_colspan(letter_hux, 1, 1, 2)
    Output
                                 ┌───────────────┬───────┐
                                 │ A             │ G     │
                                 ├───────┬───────┼───────┤
                                 │ B     │ E     │ H     │
                                 ├───────┼───────┼───────┤
                                 │ C     │ F     │ I     │
                                 └───────┴───────┴───────┘
      
      Column names: V1, V2, V3

