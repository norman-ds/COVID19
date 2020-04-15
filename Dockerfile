FROM rocker/rstudio:3.6.3
LABEL maintainer="Norman Bieri <norman.bieri@puntaminar.ch>"

# Install R packages
## deSolve: Solvers for Initial Value Problems of Differential Equations ('ODE', 'DAE', 'DDE')
RUN R -e "install.packages('deSolve', repos='https://cran.rstudio.com/')"

# Install shiny server
RUN export ADD=shiny && bash /etc/cont-init.d/add
