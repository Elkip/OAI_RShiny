# The OAI Shiny App
The data in the project is a subset of the [Osteoarthritis Initiative](https://nda.nih.gov/oai/). The research was centered around building a predicitve model to predict whether a knee replacement is needed in a subset of men and women with knee osetoarthritis. To do so a multinomial model is used with the outcome being:
    - 1) No event of interest
    - 2) Drop out before event of interest
    - 3) Death
    - 4+) A numbered random forest cluster of those who received knee replacements

Run the docker container with:
> docker build -t oai_app . 
 
> docker run -p 3838:3838 oai_app
