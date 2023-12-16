
library(shiny)


ui <- fluidPage(

  
  inputPanel(
    
    shiny::selectInput("IC","Selectionnez un intervalle de confiance",
                       choices = c("Intervalle non asymptotique(Bienaymé Tchebychev)","Intervalle non asymptotique(Hoeffding)","Intervalle asymptotique")),
    
    
    
    shiny::actionButton(inputId = "courbe",label="Courbes des bornes de l'intervalle"),
    cat("/n"),
    shiny::actionButton(inputId = "courbe2",label="Illustration des performances"),  
    
    
    
    br(),
    br(),
    
    sliderInput("alpha" ,"Choisir le niveau alpha",min=0.0,max=1.0,value=0.05,width=NULL),
    sliderInput("Samplesize","Selectionnez la taille",
                       min=5,max=100000, value=50,step=75),
    sliderInput("t" ,"Choisir un theta",min=0.0,max=1.0,value=0.5,width=NULL),
    sliderInput("MC","Le nombre de simulation de Monte Carlo",
                min=100,max=100000, value=200,step=75),
    
  ),
  
  mainPanel(plotOutput("p1"),
            verbatimTextOutput('ICa')
            
            )
  
)


server <- function(input, output) {
  
  values<-shiny::reactiveValues(un=1,deux=0)
  observeEvent(input$courbe,{
    values$un<-1
    values$deux<-0})
  
  observeEvent(input$courbe2,{
    values$un<-0
    values$deux<-1})
  
  
  output$p1<-renderPlot({
    
    if(values$un==1 && input$IC=='Intervalle non asymptotique(Bienaymé Tchebychev)'){
      
      size <-input$Samplesize
      
      theta <- input$t
      
      alpha <- input$alpha
      
      M <- input$MC
      
      real <- function(size,theta){
      u <- runif(size)
      B <- u < theta
      return (B)
      }
      
      obs <- real(size,theta)
      
      IC_bc <- function(obs,alpha){
        n <- length(obs)
        m <- mean(obs)
        i <- m - 1/(2*sqrt(n*alpha))
        s <- m + 1/(2*sqrt(n*alpha))
        return (list(inf_bc = i, sup_bc=s))
      }
      
      S <- NULL
      I <- NULL
      for(k in 1:length(obs)){
        a = IC_bc(obs[1:k],0.05)
        I[k] = a$inf_bc
        S[k] = a$sup_bc
        
      }
      
      
      simul <- function(size,theta,M,alpha){
        P <- 0
        for(i in 1:M){
          obs <- real(size,theta)
          IC_bc <- IC_bc(obs,alpha)
          if(IC_bc$inf_bc <= theta && theta <= IC_bc$sup_bc ) { P <- P + 1}
        }
        return (P/M)
      }
        
      niveau <- simul(size,theta,M,alpha)
      
      
      plot(obs, ylab = NA, pch = 3, cex=.5, col="grey", main = "Illustration IC par Bienaymé Tchebychev, loi Bernoulli")
      lines(I,col="black")
      lines(S,col="red")
      abline(h=theta,col="blue")
      legend("top", "top" , legend=c("inf", "sup", "theta"),
             col=c("black", "red", "blue"),lty=1:3, cex=0.8, text.font=4)
      
      output$ICa <- renderPrint({     
        n <-input$Samplesize
        m <- mean(obs)
        i <- m - 1/(2*sqrt(n*alpha))
        s <- m + 1/(2*sqrt(n*alpha))
        cat("Intervalle de confiance non asymptotique par Bienaymé Tchebychev : [ ")
        cat(i)
        cat(" , ")
        cat(s)
        cat(" ]")
        cat("\n")
        cat("La proportion qui contient theta vaut : ")
        cat(niveau)
      })
      
    }
    
    if(values$un==1 && input$IC=='Intervalle non asymptotique(Hoeffding)'){
      
      size <-input$Samplesize
      
      theta <- input$t
      
      alpha <- input$alpha
      
      M <- input$MC
      
      real <- function(size,theta){
        u <- runif(size)
        B <- u < theta
        return (B)
      }
      
      obs <- real(size,theta)
      
      IC_hoef <- function(obs,alpha){
        n <- length(obs)
        m <- mean(obs)
        i <- m - sqrt(-log(alpha/2)/(2*n))
        s <- m + sqrt(-log(alpha/2)/(2*n))
        return (list(inf_H = i, sup_H=s))
      }
      
      S <- NULL
      I <- NULL
      for(k in 1:length(obs)){
        a = IC_hoef(obs[1:k],0.05)
        I[k] = a$inf_H
        S[k] = a$sup_H
        
      }
      
      simul <- function(size,theta,M,alpha){
        P <- 0
        for(i in 1:M){
          obs <- real(size,theta)
          IC_hoef <- IC_hoef(obs,alpha)
          if(IC_hoef$inf_H <= theta && theta <= IC_hoef$sup_H ) { P <- P + 1}
        }
        return (P/M)
      }
      
      niveau <- simul(size,theta,M,alpha)
      
      
      plot(obs, ylab = NA, pch = 3, cex=.5, col="grey", main = "Illustration IC par Hoeffding, loi Bernoulli")
      lines(I,col="black")
      lines(S,col="red")
      abline(h=theta,col="blue")
      legend("top", "top" , legend=c("inf", "sup", "theta"),
             col=c("black", "red", "blue"),lty=1:3, cex=0.8, text.font=4)
      
      output$ICa <- renderPrint({     
        n <-input$Samplesize
        m <- mean(obs)
        i <- m - sqrt(-log(alpha/2)/(2*n))
        s <- m + sqrt(-log(alpha/2)/(2*n))
        cat("Intervalle de confiance non asymptotique par Hoeffding : [ ")
        cat(i)
        cat(" , ")
        cat(s)
        cat(" ]")
        cat("\n")
        cat("La proportion qui contient theta vaut : ")
        cat(niveau)
      })
      
    }
    
    if(values$un==1 && input$IC=='Intervalle asymptotique'){
      
      size <-input$Samplesize
      
      theta <- input$t
      
      alpha <- input$alpha
      
      M <- input$MC
      
      real <- function(size,theta){
        u <- runif(size)
        B <- u < theta
        return (B)
      }
      
      obs <- real(size,theta)
      
      IC_Asym <- function(obs,alpha){
        n <- length(obs)
        m <- mean(obs)
        q <- qnorm(1- alpha/2)
        i <- m - q/(2*sqrt(n))
        s <- m + q/(2*sqrt(n))
        return (list(inf_A = i, sup_A=s))
      }
      
      S <- NULL
      I <- NULL
      for(k in 1:length(obs)){
        a = IC_Asym(obs[1:k],0.05)
        I[k] = a$inf_A
        S[k] = a$sup_A
        
      }
      
      simul <- function(size,theta,M,alpha){
        P <- 0
        for(i in 1:M){
          obs <- real(size,theta)
          IC_Asym <- IC_Asym(obs,alpha)
          if(IC_Asym$inf_A <= theta && theta <= IC_Asym$sup_A ) { P <- P + 1}
        }
        return (P/M)
      }
      
      niveau <- simul(size,theta,M,alpha)
      
      
      plot(obs, ylab = NA, pch = 3, cex=.5, col="grey", main = "Illustration IC asymptotique, loi Bernoulli")
      lines(I,col="black")
      lines(S,col="red")
      abline(h=theta,col="blue")
      legend("top", "top" , legend=c("inf", "sup", "theta"),
             col=c("black", "red", "blue"),lty=1:3, cex=0.8, text.font=4)
      
      output$ICa <- renderPrint({     
        n <-input$Samplesize
        m <- mean(obs)
        q <- qnorm(1- alpha/2)
        i <- m - q/(2*sqrt(n))
        s <- m + q/(2*sqrt(n))
        cat("Intervalle de confiance asymptotique : [ ")
        cat(i)
        cat(" , ")
        cat(s)
        cat(" ]")
        cat("\n")
        cat("La proportion qui contient theta vaut : ")
        cat(niveau)
      })
      
    }
    
    if(values$deux==1){
      
      size <-input$Samplesize
      
      theta <- input$t
      
      alpha <- input$alpha
      
      M <- input$MC
      
      real <- function(size,theta){
        u <- runif(size)
        B <- u < theta
        return (B)
      }
      
      obs <- real(size,theta)
      
      IC_bc <- function(obs,alpha){
        n <- length(obs)
        m <- mean(obs)
        i <- m - 1/(2*sqrt(n*alpha))
        s <- m + 1/(2*sqrt(n*alpha))
        return (list(inf_bc = i, sup_bc=s))
      }
      
      IC_hoef <- function(obs,alpha){
        n <- length(obs)
        m <- mean(obs)
        i <- m - sqrt(-log(alpha/2)/(2*n))
        s <- m + sqrt(-log(alpha/2)/(2*n))
        return (list(inf_H = i, sup_H=s))
      }
      
      IC_Asym <- function(obs,alpha){
        n <- length(obs)
        m <- mean(obs)
        q <- qnorm(1- alpha/2)
        i <- m - q/(2*sqrt(n))
        s <- m + q/(2*sqrt(n))
        return (list(inf_A = i, sup_A=s))
      }
      
      I1 = NULL
      S1 = NULL
      I2 = NULL
      S2 = NULL
      I3 = NULL
      S3 = NULL
      
      for(k in 1:length(obs)){
        a = IC_bc(obs[1:k],0.05)
        b = IC_hoef(obs[1:k],0.05)
        f = IC_Asym(obs[1:k],0.05)
        I1[k] = a$inf_bc
        S1[k] = a$sup_bc
        I2[k] = b$inf_H
        S2[k] = b$sup_H
        I3[k] = f$inf_A
        S3[k] = f$sup_A
      }
      
      simul <- function(size,theta,M,alpha){
        P_bc <- 0
        P_H <- 0
        P_A <- 0
        
        for(i in 1:M){
          obs <- real(size,theta)
          IC_bc <- IC_bc(obs,alpha)
          if(IC_bc$inf_bc <= theta && theta <= IC_bc$sup_bc ) { P_bc <- P_bc + 1}
          
          IC_hoef <- IC_hoef(obs,alpha)
          if(IC_hoef$inf_H <= theta && theta <= IC_hoef$sup_H ) { P_H <- P_H + 1}
          
          IC_as <- IC_Asym(obs,alpha)
          if(IC_as$inf_A <= theta && theta <= IC_as$sup_A ) { P_A <- P_A + 1}
          
        }
        
        return (list(prop_BC = P_bc/M, prop_Hoef=P_H/M, prop_Asym = P_A/M))
      }
      
      niveau <- simul(size,theta,M,alpha)
      
      
      plot(obs, ylab = NA, pch = 3, cex=.5,col="grey", main = "Illustration, loi Bernoulli")
      lines(I1,col="red")
      lines(S1,col="red")
      lines(I2,col="black")
      lines(S2,col="black")
      lines(I3,col="yellow")
      lines(S3,col="yellow")
      abline(h=theta,col="blue")
      legend("top", "top" , legend=c("inf_bt", "sup_bt","inf_hoef", "sup_hoef","inf_asymp", "sup_asymp", "theta"),
             col=c("red", "red", "black","black", "yellow", "yellow","blue"),lty=1:3, cex=0.8, text.font=4)
      
      output$ICa <- renderPrint({     
        n <-input$Samplesize
        m <- mean(obs)
        q <- qnorm(1- alpha/2)

        i1 <- m - 1/(2*sqrt(n*alpha))
        s1 <- m + 1/(2*sqrt(n*alpha))
        
        i2 <- m - sqrt(-log(alpha/2)/(2*n))
        s2 <- m + sqrt(-log(alpha/2)/(2*n))
        
        i3 <- m - q/(2*sqrt(n))
        s3 <- m + q/(2*sqrt(n))
        
        
        
        cat("Intervalle de confiance non asymptotique par Bienaymé Tchebychev : [ ")
        cat(i1)
        cat(" , ")
        cat(s1)
        cat(" ]")
        
        cat("\n")
        
        cat("Intervalle de confiance non asymptotique par Hoeffding : [ ")
        cat(i2)
        cat(" , ")
        cat(s2)
        cat(" ]")
        
        cat("\n")
        
        cat("Intervalle de confiance asymptotique : [ ")
        cat(i3)
        cat(" , ")
        cat(s3)
        cat(" ]")
        cat("\n")
        cat("La proportion qui contient theta par B-T vaut :")
        cat(niveau$prop_BC)
        cat("\n")
        cat("La proportion qui contient theta par Hoeffding vaut :")
        cat(niveau$prop_Hoef)
        cat("\n")
        cat("La proportion qui contient theta par IC asymptotique vaut :")
        cat(niveau$prop_Asym)
      })
      
      
    }
    
    
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
