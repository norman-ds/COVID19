# Can we afford to have too little data?

We know little about the speed at which viruses are prepared in our society. The technology is available, but the implementation leaves large gaps. How many more gaps can we afford ? An overview of the spread of COVID-19 in Switzerland and a possible mutation should indicate the extent.

## Data

[Der Spiegel](https://www.spiegel.de/wissenschaft/medizin/corona-pandemie-was-uns-die-zahl-der-toten-verraet-a-ca5dc909-716c-44ac-806f-530a10916121)

## Models

[SIR Model](https://www.idmod.org/docs/hiv/model-sir.html#sir-model)
A generic epidemiological model that provides a simplified means of describing the transmission of an infectious disease through individuals where those individuals can pass through the following three states: susceptible, infectious, and recovered.

## How to use GitHub as CI/CD

GitHub is the world's leading software development platform with services for CI (Continuous Integration) and CD (Continuous Deployment). To use GitHub you must have an account and a development enviroment. You can find an easy tutorial written ba Alex [How to Properly Setup Your Github Repository](https://medium.com/@aklson_DS/how-to-properly-setup-your-github-repository-mac-version-3a8047b899e5). After that you have a prepared directory for software development.


## How to create a reproduceable Shiny-App

Start [RStudio](https://rstudio.com) in a [docker](https://www.docker.com), which always guarantees you the same ecos-system [rocker/rstudio:3.6.3](https://github.com/rocker-org/rocker). Link a local volume (the current working directory, $(pwd)) to the rstudio container. Running the following command will start RStudio in http://localhost:8787. In addition, to speed up loading at startup, we have also added the shiny software to the docker image.

To solve the model we will use a package named *deSolve* for that we are going to build an own image with a `Dockerfile`. A nice visualisation of the model with *DiagrammeR* will help.

```
FROM rocker/rstudio:3.6.3
LABEL maintainer="Norman Bieri <norman.bieri@puntaminar.ch>"

# Install R packages
## deSolve: Solvers for Initial Value Problems of Differential Equations ('ODE', 'DAE', 'DDE')
RUN R -e "install.packages('deSolve', repos='https://cran.rstudio.com/')"

# Install shiny server
RUN export ADD=shiny && bash /etc/cont-init.d/add

## Build graph/network visualization
RUN R -e "install.packages('DiagrammeR', repos='https://cran.rstudio.com/')"
```

To build the docker image run the next command:

```
docker build -f Dockerfile -t puntaminar/covid:0.3.0 -t puntaminar/covid:latest .
```

The next docker command starts in a terminal the latest image as a virtual machine. By running the RStudio in a browser in http://localhost:8787, you will be provided with Shiny, deSolver, DiagrammeR and all the packages they depends on.

```
docker run -d -p 8787:8787 -v $(pwd):/home/rstudio -e PASSWORD=pwd puntaminar/covid
```

After that to find out which packages are loaded use the *sessionInfo()* function.

```{r}
sessionInfo()
loadedNamespaces()
packageVersion('deSolve')
```

## A public Shiny-App

[shinyapps.io](https://normantest.shinyapps.io/SIRmodel/)
