# print.glysmith_result works correctly

    Code
      print(result)
    Message
      
      -- GlySmith Analysis Result 
      Plots: 2, Tables: 1, Data: 2

# print.glysmith_result handles empty plots and tables

    Code
      print(result)
    Message
      
      -- GlySmith Analysis Result 
      Plots: 0, Tables: 0, Data: 0

# print.glysmith_result handles NULL plots and tables

    Code
      print(result)
    Message
      
      -- GlySmith Analysis Result 
      Plots: 0, Tables: 0, Data: 0

# cast_table works

    Code
      cast_table(result, "table2")
    Condition
      Error in `cast_table()`:
      ! Table 'table2' not found in the result.
      i Available tables: "table1"

# cast_plot works

    Code
      cast_plot(result, "plot3")
    Condition
      Error in `cast_plot()`:
      ! Plot 'plot3' not found in the result.
      i Available plots: "plot1" and "plot2"

