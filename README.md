# drawgrid

This is a R package to generate some n*n grids and randomly generate n points within the grids.

## Why need this?
For academic research:
The figures (grids with points) generated by this function can be used to conduct cognitive load task (e.g., dot memorization task) in a experiment/survey experiment, which can burden participants' congitive resources.

## Installing
```R
devtools::install_github("Jacobzwj/drawgrid")
```

## Dependencies:
`ggplot2`  
`progress`  

## How to use
```R
library(drawgrid)
#example: generate a 4*4 empty grids, and randomly generate 5 points in the grids, suppose we need 6 figures
draw(n_grid =4,n_point =5,n_figure =6)
#or just run → draw(4,5,6)
```
![1](https://user-images.githubusercontent.com/60833574/188264215-d2b5f4c5-207d-4bc3-8e4a-491c1d82241b.png)
![2](https://user-images.githubusercontent.com/60833574/188264216-3ba1e7b3-aad4-461f-9e49-70d8c91db3b6.png)
![3](https://user-images.githubusercontent.com/60833574/188264217-1ad81d31-93de-46d6-9e16-8ec188990206.png)
![4](https://user-images.githubusercontent.com/60833574/188264219-66f276da-af6c-4973-b859-369a0be22a81.png)
![5](https://user-images.githubusercontent.com/60833574/188264220-1e393e60-155c-4e22-aa3a-fa53f51968ab.png)
![6](https://user-images.githubusercontent.com/60833574/188264221-640097fa-4840-4f17-b7c2-1e65fae2811b.png)

