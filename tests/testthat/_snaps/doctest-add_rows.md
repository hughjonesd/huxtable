# Example: add_rows

    Code
      add_rows(jams, ht)
    Output
                                    Type           Price  
                                    Strawberry      1.90  
                                    Raspberry       2.10  
                                    Plum            1.80  
                                    Gooseberry      2.15  
      
      Column names: Type, Price

---

    Code
      add_rows(jams, ht, after = 1)
    Output
                                    Type           Price  
                                    Gooseberry      2.15  
                                    Strawberry      1.90  
                                    Raspberry       2.10  
                                    Plum            1.80  
      
      Column names: Type, Price

---

    Code
      add_columns(jams, mx)
    Output
                          Type         Price    Sugar   Weight (g)  
                          Strawberry    1.90   50.00%       300.00  
                          Raspberry     2.10   60.00%       250.00  
                          Plum          1.80   40.00%       300.00  
      
      Column names: Type, Price, , .1

