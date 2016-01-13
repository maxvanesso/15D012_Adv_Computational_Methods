#EXERCISE 1
library(ggplot2)

genData<-function(class1, class2, var, save.data=FALSE, save.plot=FALSE){
  
  # Generating data
  
  x1<-seq(-1,1,length.out = class1)+rnorm(class1,0,var)
  y1<-seq(1,-1,length.out = class1)
  
  x2<-seq(1,-1,length.out = class2)
  y2<-seq(1,-1,length.out = class2)+rnorm(class2,0,var)
  
  x<-c(x1,x2)
  y<-c(y1,y2)
  target<-c(rep(0,class1), rep(1,class2))
  
  data<-data.frame(x=x,y=y,target=target)
  data$target<-as.factor(data$target)
  
  # Save data
  if(save.data==TRUE){
    write.table(data, 'dataset.csv', row.names = FALSE, sep=";")
  }
  
  # Create plot
  if(save.plot==TRUE){
    
    
    pdf(file='dataPlot.pdf')
    
    ggplot(data, aes(x = x, y = y, colours(distinct=TRUE), fill=target)) +     
      geom_point(size=2) +
      theme_bw() 
    
    dev.off()
    
  }
  return(data)
}

genData(300,300,0.01, save.data = TRUE, save.plot = TRUE)

