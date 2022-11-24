# Example: insert_column

    Code
      insert_row(jams, c("Gooseberry", 2.15), after = 1)
    Output
                                    Type           Price  
                                    Gooseberry      2.15  
                                    Strawberry      1.90  
                                    Raspberry       2.10  
                                    Plum            1.80  
      
      Column names: Type, Price

---

    Code
      insert_column(jams, c("Sugar", "50%", "60%", "40%"), after = "Price")
    Output
                                Type         Price    Sugar  
                                Strawberry    1.90   50.00%  
                                Raspberry     2.10   60.00%  
                                Plum          1.80   40.00%  
      
      Column names: Type, Price,

---

    Code
      insert_column(jams, "Sugar", after = "Price", fill = "50%")
    Output
                                Type         Price    Sugar  
                                Strawberry    1.90   50.00%  
                                Raspberry     2.10   50.00%  
                                Plum          1.80   50.00%  
      
      Column names: Type, Price,

---

    Code
      insert_row(jams, "Jams and prices", fill = "", colspan = 2)
    Output
                                    Jams and prices       
                                    Type           Price  
                                    Strawberry      1.90  
                                    Raspberry       2.10  
                                    Plum            1.80  
      
      Column names: Type, Price

