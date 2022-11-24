# Example: column_to_header

    Code
      column_to_header(jams, "Type")
    Output
                                                Price  
                                       Strawberry      
                                                 1.90  
                                       Raspberry       
                                                 2.10  
                                       Plum            
                                                 1.80  
      
      Column names: Price

---

    Code
      column_to_header(jams, "Price", number_format = 2, italic = TRUE, glue = "Price: {value}")
    Output
                                       Type            
                                       Price: 1.90     
                                       Strawberry      
                                       Price: 2.10     
                                       Raspberry       
                                       Price: 1.80     
                                       Plum            
      
      Column names: Type

