# Example: format.huxtable

    Code
      cat(format(jams, output = "screen"))
    Output
                                    Type           Price  
                                    Strawberry      1.90  
                                    Raspberry       2.10  
                                    Plum            1.80  
      
      Column names: Type, Price

---

    Code
      cat(format(jams, output = "md"))
    Output
      -----------------------
       Type            Price 
      ----------- -----------
       Strawberry       1.90 
                             
       Raspberry        2.10 
                             
       Plum             1.80 
      -----------------------
      

