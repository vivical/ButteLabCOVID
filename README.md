# How to Launch Covid Counties 

This tutorial is for volunteers interested in [Covid Counties](www.covidcounties.org) project. We explain how to run the website locally so that you can make changes to it. By far the easiest way to instantly start working on this project is to download it using [Docker](https://www.docker.com/why-docker). Docker has quickly become one of the most popular ways to share code, because it makes it easy to run any code on any computer instantly! 

### Step 1: Install Docker

You can quickly run our covid counties code on any Linux/Mac/Windows laptop using docker. You can [install docker here](https://www.docker.com/get-started). I also recommend watching a docker tutorial and installation video on [youtube](https://www.youtube.com/results?search_query=docker+tutorial).

### Step 2: Download and Run Covid Counties

Once you have docker, open your computer terminal and run the following code to download the Covid Counties project:

`docker pull pupster90/covid_tracker`

Run this next line of code to start [RStudio](https://rstudio.com/) in your browser:

`docker run -d -e PASSWORD=covidcounties --user root --name covid -p 80:8787 -p 8888:8888  pupster90/covid_tracker /init `

Then open an internet browser (chrome, safari,...) and go to the url `localhost` . You will now be asked to sign into Rstudio. To sign in use:

**username:** rstudio

**password:** covidcounties

You have now launched Rstudio with everything you need to run Covid Counties already installed! To run Covid Counties click the "Run App" button. The website will most likely instantly pop up. If it doesn't you can find the site at `localhost:8888`. The `app.R` file contains the vast majority of the code for the site. We recommend you look there first to start learning and changing the code.

### Step 3: Contact Us

After you have made changes you can either branch [our Github repo](https://github.com/vivical/ButteLabCOVID) or file a merge request. However it's probably easier to simply [contact us](mailto:covid.tracker@bakar.institute) about the changes you made so we can patch them into the site. Good luck coding and thank you for your contributions!
