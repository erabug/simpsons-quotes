simpsons-quotes
---------------
To practice cleaning data in R, I decided to build a Simpsons quote generator. First, I collected quotes from episode transcripts on the internet then I cleaned and sorted them into a dataframe. My goal is to create a simple web interface to generate random quotes or search for specific quotes.

**To do**
* [x] write script to scrape html for the first few seasons from simpsonsarchive.com
* [x] write script to clean and separate quotes, speakers, episode title from the raw html into a list of lists
* [x] convert the quote lists to a dataframe, then convert and save as a JSON file
* [ ] write simple web page to query JSON file and return random quote
* [ ] build user input forms such as number of quotes, season, and episode

**Questions**
* Can I host this web page on git?
* Is JSON the most appropriate format for a simple (and static) text-based database?
