Final Project for MA615 (BU)
=============
Sentiment analysis on Secretary of State Candidates using twitter API & R shiny

Motivation
-------

President Elect Donald Trump is to choose Secretary of State serving in his cabinet for his presidency. In the past few weeks, he has met with many politicians, business tycoons, and talented individuals to build his team to make America great again. This project, thanks to Prof. Haviland Wright in the Mathematics & Statistics department at Boston Univeristy, is possible and serves to show some indications of what sentiments the general public have in response to the candidates that Mr. Trump has been met and tweeted about. 


How it works
-------

The code is written in R and required of many packages such as "ggmap","plyr","shiny", etc.

Essentially, the app looks for geocoded tweets originated from the U.S. Then it parses them and extracts the words that matches the list of "positive" words and "negative" words. A simple algorithm (Jeffrey Breen's) is run to calculate the sentiment scores, which are finally aggregated on the state level and are charted. These compiled list of words expressing positive and negative sentiments (over 6800 altogrther) are contributed by Hu and Liu (http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html)

The UI developed in this project has a limitation of only comparing two known candidates. Since the decision process is still on going, the twitter sentiment varies as the sampled tweets change everyday. I indended to save the tweets as json and store daily tweets as one sample point, but I adopted a simplied version where it pools a user-defined tweets from the newest tweets. 

The UI includes the following features:

0. scatterplot of the sentiment scores versus tweet number for each candidate.
0. histograms of the sentiment scores from the number of tweets defined by the user.
0. a table of sample tweets and its corresponding sentiment scores.
0. wordcloud for each candidate, capturing what key words being twitter the most.
0. tweet geolocation map (only those twitter users who provided their location).
0. sample tweets from each candidate.

Running the code
-------
the main scripts are server.R and ui.R, providing the shiny user interface. There are default names in the input. Users are free to change any candidates they are interested. The other input is the number of tweets (for each candidate). User can change the number of tweets from 10 to 1000. The features are arranged in Five tabs: the first tab summarizes features 1-3 (described in How it works section); the second tab for feature 4; third tab for feature 5; and 4th and 5th tab for feature 6.

Feel free to leave feedbacks!!


KenZ34601

C
