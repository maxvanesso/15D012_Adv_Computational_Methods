sigmaXY <- function(rho, sdX, sdY) {
  covTerm <- rho * sdX * sdY
  VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 2, 2, byrow = TRUE)
  return(VCmatrix)
}
genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
  if(!is.na(seed)) set.seed(seed)
  rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
  return(rdraws)
}
# creating a function for all of this
loanData <- function(noApproved, noDenied, muApproved, muDenied, sdApproved, 
                     sdDenied, rhoApproved, rhoDenied, seed=1234) {
  sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
  sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
  approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
  denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
  loanDf <- as.data.frame(rbind(approved,denied))
  deny <- c(rep("Approved", noApproved), rep("Denied", noDenied))
  target = c(rep(0, noApproved), rep(1, noDenied))
  loanDf <- data.frame(loanDf, deny, target)
  colnames(loanDf) <- c("PIratio", "solvency", "class", "target")
  return(loanDf)
}

shinyServer(function(input, output) {

  output$data_plot <- renderPlot({
   
    loanDf <- loanData(noApproved=50, noDenied=50, c(input$meanSolApp, input$meanPIApp), c(input$meanSolDen, input$meanPIDen), c(input$sdSolApp, input$sdPIApp), c(input$sdSolDen, input$sdPIDen), -0.1, 0.6, 1221)
    
    datafit <- lm(target ~ solvency + PIratio + 1, data=loanDf)
    weights <- coef(datafit)[c("solvency", "PIratio")] 
    bias <- coef(datafit)[1]
    
    intercept <- (-bias + 0.5)/weights["PIratio"] 
    slope <- -(weights["solvency"]/weights["PIratio"])
    
    x <- seq(min(loanDf["PIratio"]), max(loanDf["PIratio"]),length.out = nrow(loanDf))
    y <- -(weights["PIratio"]/weights["solvency"])*x + (0.5-bias)/weights["solvency"]
    # careful, colnames have to match!
    boundaryDf <- data.frame(PIratio=x, solvency=y, class=rep("Boundary", length(x)))
    
    ggplot(data = loanDf,
          aes(x = solvency, y = PIratio, colour = factor(class))) +
          geom_point(shape='x', size = 5) + xlab("solvency") + ylab("PI ratio") + 
          theme_bw() + geom_line(data=boundaryDf) +
          scale_color_manual(values = c("blue", "grey", "black"))
    
  })
   output$confusion_matrix <- renderTable({
     
     loanDf <- loanData(noApproved=50, noDenied=50, c(input$meanSolApp, input$meanPIApp), c(input$meanSolDen, input$meanPIDen), c(input$sdSolApp, input$sdPIApp), c(input$sdSolDen, input$sdPIDen), -0.1, 0.6, 1221)
     
     datafit <- lm(target ~ solvency + PIratio + 1, data=loanDf)
     
     predictedLabels <- ifelse(predict(datafit) < 0.5, "Approved", "Denied")
     # confusion matrices
     confMatrixFreq <- table(loanDf$class, predictedLabels)
     
     return(confMatrixFreq)
  })
})