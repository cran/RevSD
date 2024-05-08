#' Standard Deviation Visualization
#' @export

revsd <- function(data, shouldInstallAllPackages) {
  sI = FALSE;
  if(!missing(shouldInstallAllPackages)) {
    sI = shouldInstallAllPackages
  }
  message("Installation of these packages are recommended prior to the use of RevSD: shiny, shinydashboard, plotrix, and shinywidgets.")

  if (sI) {
    install.packages("shiny");
    install.packages("shinydashboard");
    install.packages("plotrix");
    install.packages("shinyWidgets");
  }

  # Step 0: Preliminary data preparation
  data<-sort(data)
  n<-length(data)
  f<-rep(1/n,n)
  F<-cumsum(f)

  # Different versions of function that draws a cylinder
  cyl <- function(x, y, lr, sr, t) {
    draw.ellipse(x, y, lr, sr, angle = 0, col = "white",deg = TRUE, segment = c(0, -180), lwd = 1.5)
    rect(x-lr, y,x+lr, y+t,lwd=1.5,lty=1,col = "white")
    segments(x-lr, y, x+lr,y, lwd = 1.5, xpd=T, col = "white")
    segments(x-lr, y+t, x+lr,y+t, lwd = 1.5, xpd=T, col = "white")
    segments(x-lr, y, x-lr, y+t,lwd = 1.5, lty=1,xpd=T)
    segments(x+lr, y, x+lr, y+t,lwd = 1.5, lty=1,xpd=T)
    draw.ellipse(x, y+t, lr, sr, angle = 0, col = "#d3d3d3", lwd = 1.5)
  }
  cyl1 <- function(x, y, lr, sr, t) {
    draw.ellipse(x, y, lr, sr, angle = 0, col = "white",deg = TRUE, segment = c(0, -180), lwd = 1.5)
    rect(x-lr, y,x+lr, y+t,lwd=1.5,lty=1,col = "white")
    segments(x-lr, y, x+lr,y, lwd = 1.5, xpd=T, col = "white")
    segments(x-lr, y+t, x+lr,y+t, lwd = 1.5, xpd=T, col = "white")
    segments(x-lr, y, x-lr, y+t,lwd = 1.5, lty=1,xpd=T)
    segments(x+lr, y, x+lr, y+t,lwd = 1.5, lty=1,xpd=T)
  }
  cyl2 <- function(x, y, lr, sr, t) {
    rect(x-lr, y,x+lr, y+t,lwd=1.5,lty=1,col = "white")
    segments(x-lr, y, x+lr,y, lwd = 1.5, xpd=T, col = "white")
    segments(x-lr, y+t, x+lr,y+t, lwd = 1.5, xpd=T, col = "white")
    segments(x-lr, y, x-lr, y+t,lwd = 1.5, lty=1,xpd=T)
    segments(x+lr, y, x+lr, y+t,lwd = 1.5, lty=1,xpd=T)
  }
  cyl3 <- function(x, y, lr, sr, t) {
    rect(x-lr, y,x+lr, y+t,lwd=1.5,lty=1,col = "white")
    segments(x-lr, y, x+lr,y, lwd = 1.5, xpd=T, col = "white")
    segments(x-lr, y+t, x+lr,y+t, lwd = 1.5, xpd=T, col = "white")
    segments(x-lr, y, x-lr, y+t,lwd = 1.5, lty=1,xpd=T)
    segments(x+lr, y, x+lr, y+t,lwd = 1.5, lty=1,xpd=T)
    draw.ellipse(x, y+t, lr, sr, angle = 0, col = "#d3d3d3", lwd = 1.5)
  }
  cylSd <- function(x, y, lr, sr, t) {
    draw.ellipse(x, y, lr, sr, angle = 0,deg = TRUE,lty=2,lwd = 1.5, border = "blue")
    segments(x-lr, y, x-lr, y+t,lwd = 1.5, lty=2,xpd=T, col = "blue")
    segments(x+lr, y, x+lr, y+t,lwd = 1.5, lty=2,xpd=T, col = "blue")
    draw.ellipse(x, y+t, lr, sr, angle = 0, lty=2, lwd = 1.5, border = " blue")
  }
  cylSD <- function(x, y, lr, sr, t) {
    draw.ellipse(x, y, lr, sr, angle = 0, col = "white",deg = TRUE, segment = c(0, -180), lwd = 1.5, border = "#05445E")
    draw.ellipse(x, y, lr, sr, angle = 0, col = "white",deg = TRUE, segment = c(0, 180), lty = 2, lwd = 1.5, border = "#05445E")
    segments(x-lr, y, x+lr,y, lwd = 1.5, xpd=T, col = "white")
    segments(x-lr, y+t, x+lr,y+t, lwd = 1.5, xpd=T, col = "white")
    segments(x-lr, y, x-lr, y+t,lwd = 1.5, lty=1,xpd=T, col = "#05445E")
    segments(x+lr, y, x+lr, y+t,lwd = 1.5, lty=1,xpd=T, col = "#05445E")
    draw.ellipse(x, y+t, lr, sr, angle = 0, col = "#FABEC0", lwd = 1.5, border = "#05445E")
  }

  # Define UI for application that draws a histogram
  if(sI) {
    ui <- dashboardPage(skin = "yellow",
                        dashboardHeader(title = "RMSD from ECDF"),
                        dashboardSidebar(
                          sliderInput(inputId = "l",
                                      label = "Value of L:",
                                      min = min(data),
                                      max = max(data),
                                      value = min(data),
                                      step = 0.01),
                          sliderInput(inputId = "r",
                                      label = "Tilt:",
                                      min = 0.1,
                                      max = 0.5,
                                      value = 0.1,
                                      step = 0.1),
                          checkboxInput(inputId = "style",
                                        label = "Show RMSD(L) cylinder",
                                        value = FALSE),
                          width = 300
                        ),

                        dashboardBody(
                          fluidRow(
                            plotOutput("distPlot"),
                            style = "margin-top : -70px;"
                          ),
                          fluidRow(
                            plotOutput("cylinders"),
                            style = "margin-top : -80px;"
                          ),
                        )
    )

    # Define server logic required to draw a histogram
    server <- function(input, output) {

      # Output the ECDF
      output$distPlot <- renderPlot({
        l <- input$l
        m<-round(mean(data), digits = 2)
        r <- input$r

        # Plot the ECDF of the data
        if(abs(min(data)-max(data))*0.5/(n) > 0.27){
          plot(data,F,type="s", xlab="", ylab="",frame.plot = FALSE, las=1, xaxs="r",yaxs="r",ylim = c(0-abs(min(data)-max(data))*0.5/(n),1+abs(max(data)-min(data))*0.5/(n)), xlim = c(min(data)-max(data), 2*max(data) - min(data)),xaxt="n", yaxt="n")
        } else {
          plot(data,F,type="s", xlab="", ylab="",frame.plot = FALSE, las=1, xaxs="r",yaxs="r",ylim = c(-0.27,1.27), xlim = c(min(data)-max(data), 2*max(data) - min(data)),xaxt="n", yaxt="n")
        }
        axis(1, pos=0, at=c(seq(0,max(data),1)))
        arrows(0, 0, max(data)+2, 0, code=2, length = 0.15)
        text(max(data)+3.5,0, expression("x"), xpd=TRUE, cex = 1.2)
        axis(2, pos=0, at=c(round(seq(0,1,1/5),digits = 1)), las=1)
        arrows(0,-0.05,0,1.1, code=2,length=0.15)
        text(0,1.2,expression("ECDF"), cex=1.1)
        segments(0,1,max(data)+2,1,lty=2)
        segments(max(data),1, max(data)+1,1,lty=1)
        segments(min(data),F[1],min(data),0,lty = 1)

        # Shading the ECDF
        for(i in c(1:(length(data)-1))){
          if(data[i] < data[i+1]){
            if(data[i] <= m){
              segments(data[i],F[i],0,F[i],lty = 2, col = "gray")
            }
            else {
              segments(l,F[i],0,F[i],lty = 2, col = "gray")
            }
          }
        }
        s <- sd(data)
        if(abs(m - l) > 0.3 * s) {
          r <- "#FABEC0"
          b <- "#C2E2F5"
        } else if (abs(m - l) < 0.1 * s){
          r <- "Red"
          b <- "Blue"
        } else {
          r <- "#F37970"
          b <- "#79A9F5"
        }
        step <- 0.01
        if(l<m) {
          for(i in c(1:length(data))){
            if(data[i] <= l){
              for (j in c(seq(data[i],l,step))) {
                segments(j,F[i],j,0,lty = 1,col = b)
              }
            }
            else {
              for (j in c(seq(l,data[i],step))) {
                segments(j,F[i-1],j,1,lty = 1,col = r)
              }
              segments(data[i],F[i],data[i],0,lty = 2, col = "gray")
            }
          }
          for(i in c(1:length(data))){
            if(data[i] <= m){
              segments(data[i],F[i],data[i+1],F[i],lty = 1, lwd = 1.5, col = "black")
              if(i > 1){
                segments(data[i],F[i],data[i],F[i-1],lty = 1, lwd = 1.5, col = "black")
              }
              else{
                segments(data[i],F[i],data[i],0,lty = 1, lwd = 1.5, col = "black")

              }
            } else {
              segments(data[i-1],F[i-1],data[i],F[i-1],lty = 1, lwd = 1.5, col = "black")
              segments(data[i],F[i],data[i],F[i-1],lty = 1, lwd = 1.5, col = "black")
            }
          }
          text(l,-0.25,expression("L"),xpd=TRUE, cex=1.1)
        } else if(l == m) {
          for(i in c(1:length(data))){
            if(data[i] <= m){
              for (j in c(seq(data[i],m,step))) {
                segments(j,F[i],j,0,lty = 1,col = "#49FF00")
              }
            }
            else {
              for (j in c(seq(m,data[i],step))) {
                segments(j,F[i-1],j,1,lty = 1,col = "#49FF00")
              }
              segments(data[i],F[i],data[i],0,lty = 2, col = "gray")
            }
          }
          for(i in c(1:length(data))){
            if(data[i] <= m){
              segments(data[i],F[i],m,F[i],lty = 1, lwd = 1.5, col = "black")
              if(i > 1){
                segments(data[i],F[i],data[i],F[i-1],lty = 1, lwd = 1.5, col = "black")
              }
              else{
                segments(data[i],F[i],data[i],0,lty = 1, lwd = 1.5, col = "black")
                segments(data[i],0,m,0,lty = 1, col = "black")
              }
            } else {
              segments(data[i],F[i-1],m,F[i-1],lty = 1, lwd = 1.5, col = "black")
              segments(data[i],F[i],data[i],F[i-1],lty = 1, lwd = 1.5, col = "black")
            }
          }
          text(l,-0.25,expression(paste("L = ",bar("x"))),xpd=TRUE, cex=1.1)
        } else {
          for(i in c(1:length(data))){
            if(data[i] <= l){
              for (j in c(seq(data[i],l,step))) {
                segments(j,F[i],j,0,lty = 1,col = r)
              }
            }
            else {
              for (j in c(seq(l,data[i],step))) {
                segments(j,F[i-1],j,1,lty = 1,col = b)
              }
              segments(data[i],F[i],data[i],0,lty = 2, col = "gray")
            }
          }
          for(i in c(1:length(data))){
            if(data[i] <= m){
              segments(data[i],F[i],data[i+1],F[i],lty = 1, lwd = 1.5, col = "black")
              if(i > 1){
                segments(data[i],F[i],data[i],F[i-1],lty = 1, lwd = 1.5, col = "black")
              }
              else{
                segments(data[i],F[i],data[i],0,lty = 1, lwd = 1.5, col = "black")
                segments(data[i],0,m,0,lty = 1, col = "black")
              }
            } else {
              segments(data[i-1],F[i-1],data[i],F[i-1],lty = 1, lwd = 1.5, col = "black")
              segments(data[i],F[i],data[i],F[i-1],lty = 1, lwd = 1.5, col = "black")
            }
          }
          text(l,-0.2,expression("L"),xpd=TRUE, cex=1.1)
        }
        segments(l,0,l,1.1,lty = 1, lwd = 1.5)
        segments(0,0,data[-1],0,lty = 1)
      })

      # Output the cylinders
      output$cylinders <- renderPlot({
        l <- input$l
        m<-round(mean(data), digits = 2)
        r <- input$r

        # Check if the checkbox is checked or not
        selected <- input$style
        if(abs(min(data)-max(data))*0.5/(n) > 0.27){
          plot(data, F, type = "n", ylim = c(0-abs(min(data)-max(data))*0.5/(n),1+abs(max(data)-min(data))*0.5/(n)), xlim = c(min(data)-max(data), 2*max(data) - min(data)),xlab="",ylab="",xaxt="n", yaxt="n",frame.plot = FALSE )
        } else {
          plot(data, F, type = "n", ylim = c(-0.27,1.27), xlim = c(min(data)-max(data), 2*max(data) - min(data)),xlab="",ylab="",xaxt="n", yaxt="n",frame.plot = FALSE )
        }
        s = 0
        for(i in c(1:length(data))) {
          s<- s+ abs(data[i]-l) * abs(data[i]-l) * 1/n
        }
        s <- sqrt(s)

        # Constructing the cylinders
        if(!selected){
          y = 0
          for(i in c(1:length(data))){
            sr<-abs(data[i]-l)*r/(n)

            if(data[i] != l){
              if(i == 1 && data[i] == data[i+1]) {
                cyl1(l, y, abs(data[i]-l), sr, 1/n)
              } else if (i > 1 && i < length(data)){
                if(data[i] == data[i+1]){
                  if(data[i]==data[i-1]){
                    cyl2(l, y, abs(data[i]-l), sr, 1/n)
                  } else {
                    cyl1(l, y, abs(data[i]-l), sr, 1/n)
                  }
                } else if (data[i] == data[i-1]) {
                  cyl3(l, y, abs(data[i]-l), sr, 1/n)
                }
                else {
                  cyl(l, y, abs(data[i]-l), sr, 1/n)
                }
              } else {
                cyl(l, y, abs(data[i]-l), sr, 1/n)
              }
            } else {
              segments(l, y, l, y+1/n,lwd = 1.5, lty=1)
            }
            y<- y+1/n
          }
          cylSd(l,0,s,s*r/(n),1)
        } else {
          cylSD(l,0,s,s*r/(n),1)
          segments(l,0,l,1,lty=2)
          arrows(l,0.5,l+s,0.5,code=3,col = "red",lwd = 2, length = 0.1)
          if(l==m){
            text(l+s/2,0.6,paste("RMSD(L)=SD=",round(s,digits = 4)), cex = 0.8, col = "red")
          } else {
            text(l+s/2,0.6,paste("RMSD(L)=",round(s,digits=4)), cex = 1, col = "red")
          }
        }
      })
    }

    # Run the application
    shinyApp(ui = ui, server = server)
  } else {
    message("All packages above recommended to run the application.")
  }
}
