# Printing methods work as expected

    Code
      print(df)
    Output
      # An MI data frame: 3 x 3 with 2 bags
      # and instance labels: 0, 1, 0
        bag_label bag_name    X1
        <fct>     <chr>    <dbl>
      1 1         bag_1     -0.4
      2 1         bag_1      0.5
      3 0         bag_2      2  

---

    Code
      print(df)
    Output
      # An MI data frame: 6 x 3 with 2 bags
      # and instance labels: 0, 1, 0, 0, 1, ...
        bag_label bag_name    X1
        <fct>     <chr>    <dbl>
      1 1         bag_1     -0.4
      2 1         bag_1      0.5
      3 0         bag_2      2  
      4 1         bag_1     -0.4
      5 1         bag_1      0.5
      6 0         bag_2      2  

---

    Code
      print(df)
    Output
      # An MI data frame: 3 x 3 with 2 bags
        bag_label bag_name    X1
        <fct>     <chr>    <dbl>
      1 1         bag_1     -0.4
      2 1         bag_1      0.5
      3 0         bag_2      2  

---

    Code
      print(df)
    Output
      # An MI data frame: 3 x 3 with 2 bags
        bag_label bag_name    X1
        <fct>     <chr>    <dbl>
      1 1         bag_1     -0.4
      2 1         bag_1      0.5
      3 0         bag_2      2  

---

    Code
      print(df, n = 2)
    Output
      # An MI data frame: 3 x 3 with 2 bags
        bag_label bag_name    X1
        <fct>     <chr>    <dbl>
      1 1         bag_1     -0.4
      2 1         bag_1      0.5
      # ... with 1 more row

