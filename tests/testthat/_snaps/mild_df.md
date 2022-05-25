# Printing methods work as expected

    Code
      print(df)
    Output
      # An MILD data frame: 6 x 4 with 2 bags, 3 instances
      # and instance labels: 0, 0, 1, 1, 0, ...
        bag_label bag_name instance_name    X1
        <fct>     <chr>    <chr>         <dbl>
      1 1         bag_1    bag_1_inst_1  -0.4 
      2 1         bag_1    bag_1_inst_1  -0.35
      3 1         bag_1    bag_1_inst_2   0.4 
      4 1         bag_1    bag_1_inst_2   0.5 
      5 0         bag_2    bag_2_inst_1   2   
      6 0         bag_2    bag_2_inst_1   2.1 

---

    Code
      print(df)
    Output
      # An MILD data frame: 6 x 4 with 2 bags, 3 instances
        bag_label bag_name instance_name    X1
        <fct>     <chr>    <chr>         <dbl>
      1 1         bag_1    bag_1_inst_1  -0.4 
      2 1         bag_1    bag_1_inst_1  -0.35
      3 1         bag_1    bag_1_inst_2   0.4 
      4 1         bag_1    bag_1_inst_2   0.5 
      5 0         bag_2    bag_2_inst_1   2   
      6 0         bag_2    bag_2_inst_1   2.1 

---

    Code
      print(df)
    Output
      # An MILD data frame: 6 x 4 with 2 bags, 3 instances
        bag_label bag_name instance_name    X1
        <fct>     <chr>    <chr>         <dbl>
      1 1         bag_1    bag_1_inst_1  -0.4 
      2 1         bag_1    bag_1_inst_1  -0.35
      3 1         bag_1    bag_1_inst_2   0.4 
      4 1         bag_1    bag_1_inst_2   0.5 
      5 0         bag_2    bag_2_inst_1   2   
      6 0         bag_2    bag_2_inst_1   2.1 

---

    Code
      print(df, n = 2)
    Output
      # An MILD data frame: 6 x 4 with 2 bags, 3 instances
        bag_label bag_name instance_name    X1
        <fct>     <chr>    <chr>         <dbl>
      1 1         bag_1    bag_1_inst_1  -0.4 
      2 1         bag_1    bag_1_inst_1  -0.35
      # ... with 4 more rows

