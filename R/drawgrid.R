#' @export
draw <- function(n_grid,n_point,n_figure){
  pb <- progress::progress_bar$new( format = "  generating grid [:bar] :percent eta: :eta",
                          total = n_figure, clear = FALSE, width= 60)
  for(i in c(1:n_figure)){
    pb$tick()
    m <- matrix(c(0), nrow=n_grid, ncol=n_grid)
    df <- expand.grid(x=1:ncol(m),y=1:nrow(m))
    point <- df[sample(nrow(df), n_point), ]
    F<-ggplot2::ggplot(df, ggplot2::aes(x=x, y=y)) + 
      ggplot2::geom_tile(fill='transparent', colour = 'black') + 
      ggplot2::scale_y_reverse() +
      ggplot2::theme_classic() + 
      ggplot2::theme(axis.text  = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            axis.line  = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            axis.title = ggplot2::element_blank()) +
      ggplot2::geom_point(data = point, col = 'black',size =5)
    
    png(file=paste0(i,'.png'))
    print(F)
    dev.off()
  }
}