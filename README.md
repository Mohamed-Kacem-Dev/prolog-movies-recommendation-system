# prolog-movies-recommendation-system
Movies and tv shows recommendation system written in prolog with admin functionalities using a csv database
Database source : https://www.kaggle.com/datasets/shivamb/netflix-shows?resource=download

1.	The system is a content-based recommendation system, we are taking user preferences as an input and generate suggestions based on it. (with the feature of suggesting another movie if the user liked the first one).
2.	The code starts by reading in a dataset of movies and their features from a CSV file.  Transforms the data into prolog facts and loads it into the memory, then takes user preferences as an input (genre, duration, age restrictions, release date), then uses prolog rules to match the movies that meets user preferences and returns it as an output, if the user likes the movie the system regenerate another title using another with the same characteristics of the suggested movie.
3. The system includes admin functionalities like login, delete a movie, add a movie which involves manipulating the csv file.
