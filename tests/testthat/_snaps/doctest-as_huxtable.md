# Example: as_huxtable

    Code
      as_huxtable(dfr)
    Output
                                           1   a        
                                           2   b        
                                           3   c        
                                           4   d        
                                           5   e        
      
      Column names: a, b

---

    Code
      as_huxtable(mx, add_colnames = FALSE)
    Output
                                   a       e       i      
                                   b       f       j      
                                   c       g       k      
                                   d       h       l      
      
      Column names: V1, V2, V3

---

    Code
      as_huxtable(tbl)
    Output
                                           L      M   H     
                                    A      9      9   9     
                                    B      9      9   9     
      
      Column names: rownames, L, M, H

