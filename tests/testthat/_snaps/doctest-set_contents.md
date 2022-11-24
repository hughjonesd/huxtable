# Example: set_contents

    Code
      set_contents(jams, 2, 1, "Blackcurrant")
    Output
                                   Type             Price  
                                   Blackcurrant      1.90  
                                   Raspberry         2.10  
                                   Plum              1.80  
      
      Column names: Type, Price

---

    Code
      map_contents(jams, by_regex(`.*berry` = "Snodberry"))
    Output
                                    Type          Price  
                                    Snodberry      1.90  
                                    Snodberry      2.10  
                                    Plum           1.80  
      
      Column names: Type, Price

