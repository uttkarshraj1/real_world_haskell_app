# Real World Haskell-App


1) In this group project we have developed a real-world Haskell app that performs HTTP requests and parses an input from string to a custom Haskell data type and stores it into a database.

2) The database we have picked is the UK food hygiene rating database. We chose it because there is a lot of useful data in this source, but it’s not structured in a very user-friendly fashion. We are also interested in deriving generalisations and statistics that could help people navigate through local food scenes.
 
=> The Food Hygiene Rating Scheme (FHRS) and Food Hygiene Information Scheme (FHIS) data is published at www.food.gov.uk/ratings

=> The FHRS is run in England, Wales and Northern Ireland, and the FHIS is run in Scotland.

=> The rating value is the result of an inspection for a business where ‘FHRS’ uses a rating between 0 and 5 and ‘FHIS’ uses ‘Pass’, ‘Pass and Eat safe’ or ‘Improvement Required’

**Extracting the Information**

a) We are extracting the information from the web source and from there we are putting it into our database.

b) We also check the URL in the database, if it is present then we are not downloading the data.

c) We are initialising the database and saving the establishment.

d) We also maintain the error handling for the modules.
