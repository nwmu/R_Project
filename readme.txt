Required Packages

Impute      - To handle NA values by imputing the means 
dplyr       - For data wrangling and Data Cleaning
cluster     - For Cluster Analysis
ggplot2     - To draw Plots in Exploratory data analysis
factoextra  - Clustering Analysis
corrplot    - Corelation Analysis
shiny       - To Create interface of shiny app
leaflet     - To Pull MAp on Shiny app Interface
e1071       - Prediction and probablity Analysis in Shiny App
rpart       - Classification and regression analysis
gmodel      - For Creating regression models

To run the code of the project one should install all of the mentioned packages.

If the packages are already installed you pull them using the following syntax
Syntax: library(package)
                  
                     or

If the packages are not installed then you can install them using the following syntax

Syntax: install.Packages("")



Instructions To Download the Project Files:

RMD of analysis of crash data : SafeDrive.rmd

Html knit : SafeDrive.html 

Published html  : index.html

Data : crash_data.scv(as the data is more than 25 mb we could upload the data there though you can access the data through this link : https://drive.google.com/file/d/136t0aPpPde7NBwBNcmY1H_D36seKyj0E/view?usp=share_link)

Processed data : df.csv 

It is a better practice to download all the files in a single directory rather than downloading them separately. 

If you do download them separately then the code might not work as there will be change in working directory.

Now check the working directory in the R-studio using getwd()

If the downloaded crash data is not in the path please use setwd(path/to/file) to route the data file 

As for shinnyApp we were able create the working SafeDrvie application but there is error in pulling the graph so the app isn't responding as expected , but we are providing the versions we worked on the files names are : 
SafeDriveV1 
and 
SafeDrive2 

These are two versions of ShinyApp we have worked on 

 






 




