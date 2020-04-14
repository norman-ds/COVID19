# Can we afford to have too little data?

We know little about the speed at which viruses are prepared in our society. The technology is available, but the implementation leaves large gaps. How many more gaps can we afford ? An overview of the spread of COVID-19 in Switzerland and a possible mutation should indicate the extent.

## How to use Git-Hub as CI/CD

Continuous Integration and Deployment serve


## How to create a reproduceable Shiny-App

Start [RStudio](https://rstudio.com) in a [docker](https://www.docker.com), which always guarantees you the same ecos-system [rocker/rstudio:3.6.3](https://github.com/rocker-org/rocker). Link a local volume (the current working directory, $(pwd)) to the rstudio container. Running the following command will start RStudio in http://localhost:8787:

**package deSolve and tidyr   !!!**


``` yaml
docker run -d -p 8787:8787 -v $(pwd):/home/rstudio -e PASSWORD=mypwd rocker/rstudio:3.6.3
```

## Models

[SIR Model](https://www.idmod.org/docs/hiv/model-sir.html#sir-model)
A generic epidemiological model that provides a simplified means of describing the transmission of an infectious disease through individuals where those individuals can pass through the following five states: susceptible, infectious, and recovered.