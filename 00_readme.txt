Welcome to the GitHub Repository for the Data Science Project "(International) User Preferences on Spotify".
Here, you find all the relevant datasets and R-Scripts that we used for developing the application.

The scripts server.R, ui.R and global.R are the scripts that set up the application. They have to be located in the same repository to ensure that they interact properly.

All the other scripts are to be executed according to their enumeration and deliver the fundament of the application. The datasets have to be located in the repository "./data/" such that the scripts can process the data.

The overall folder structure that we use on the server is as follows:

- Main repository that holds the application: "/srv/shiny-server/dsp/"
- Location of the data: "/srv/shiny-server/dsp/data/"
- Location of the models: "/srv/shiny-server/dsp/models/"
- Location of visualization: "/srv/shiny-server/dsp/output/"
- Location of the coefficient plots: "/srv/shiny-server/dsp/www/"
- global.R, server.R, ui.R are stored in "/srv/shiny-server/dsp/"
