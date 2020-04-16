#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# initialization 
t <- seq(0, 180, by=1) # time points
DoI <- 9 # average duration of infection
N <- 8.57e6 # population Switzerland
I0 <- 100 # infectious population at time zero
R0 <- 3 # recoverd population at time zero
S0 <- N-I0-R0 # susceptible population at time zero

y0 <- c(S=S0, I=I0, R=R0)
y.names <- c("susceptible ", "infectious", "recoverd")
parms <- c(R0=R0, N=N, DoI=DoI)
par_names <- c("Reproductive number, R0", 
               "Number of individuals", 
               "Average duration of infection")
par_min <- c(1, N/1000, 1)
par_max <- c(20, N*10, 20)

#' These function returns the delta of the classic [SIR model](https://www.idmod.org/docs/hiv/model-sir.html#sir-model) without births or deaths.
#' The package deSolve requires t, y, parms a parameter.
#' @param t vector of time steps
#' @param y vector of model variables S,I,R.
#' @param parms vector of model parameters R0, DoI, N.
#' @return equation list
#' 
SIR <- function(t, y, parms) {
    
    if (!all(c("R0", "DoI", "N") %in% names(parms))) {
        stop("The SIR model requires R0 (recoverd population at time zero), DoI (duration, D, of infection) and N (total population).")
    }
    
    # constant rates
    gamma <- 1/parms["DoI"]
    beta <- parms["R0"] * gamma / parms["N"]
    
    # Delta Susceptibles S
    dS <- - beta * y["S"] * y["I"]
    
    # Delta Infecteds I
    dI <- beta * y["S"] * y["I"] -
        gamma * y["I"]
    
    # Delta Recovereds R
    dR <- gamma * y["I"]
    
    dy <- c(dS, dI, dR)
    names(dy) <- c("dS","dI","dR")
    return(list(dy))
    
}


library(shiny)

# User Interface (UI)
ui <- pageWithSidebar(
    headerPanel("SIR model", windowTitle = "SIRmodel"),
    sidebarPanel(
        lapply(seq_along(parms),
               function(x) sliderInput(inputId = names(parms)[x], label = par_names[x],
                                       value = parms[x], min = par_min[x], max = par_max[x])
        )
    ),
    mainPanel(
        plotOutput("plot_model"),
        
        DiagrammeR::DiagrammeROutput(outputId = "diagram", width = "500", height = "auto"),
        
        withMathJax(),
        helpText('\\(\\beta\\) is the transmission rate and 
                 \\(\\beta SI\\) represents the number of susceptible individuals that become infected per day'),
        helpText('\\(\\gamma\\)  is the recovery rate and 
                 \\(\\gamma I\\) represents the number of infected individuals that recover per day'),
        helpText('\\(\\frac1\\gamma\\) is the infectious period i.e. the average duration of time an individual remains infected.'),
        
        tableOutput("table_rates")
    )    
)

# Define server logic required to draw a line plot
server <- function(input, output){
    
    output$plot_model <- renderPlot({
        
        # Get parameters from user input
        parms_active <- unlist(reactiveValuesToList(input))
        
        # Run ODE solver
        y.mat <- deSolve::lsoda(y = y0,
                                times = t,
                                func = SIR,
                                parms = parms_active)
        y.mat <- y.mat[,2:4]

        # draw the line plot
        matplot(t, y.mat, type = "l", xlab = "Time", ylab = par_names[2], col=1)
        legend("right", y.names , lty=5:1, col=1, bty="n", title = "population")
    })
    
    output$table_rates <- renderTable({
        
        # Get parameters from user input
        parms_active <- unlist(reactiveValuesToList(input))
        
        data.frame(
            Rate = c("beta", "gamma"),
            Value = c(parms_active["R0"] * (1/parms_active["DoI"]) / parms_active["N"],
                      1/parms_active["DoI"])
        )
        
    }, digits = -1, bordered = TRUE)
    
    output$diagram <- 
        DiagrammeR::renderDiagrammeR({
            DiagrammeR::mermaid("
                graph LR
                A(Susceptible)-->B(Infectious)
                B-->C(Recovered)
            ")
        })
    
}



# Run the application 
shinyApp(ui = ui, server = server)
