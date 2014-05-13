## Data Warehousing and Data Mining - Assignment 1

#### Objective

To apply problem identification in data cleansing, and refresh on basic SQL commands and interpreting data

#### Tasks

##### 1. Clean and prepare the file “Marital status of men and women.xls”, and document your actions.

- See task 3 to decide which attributes to select or reject
- You may use any tools you like

##### 2. Create an SQLite database with your data and the CrimeRates data

- You may need to convert your data into a Comma Separated Value (CSV) file for ease of processing
- Once you have your CSV file, it can be inserted into SQLite using:

	`.mode list `

	`.separator , `

	`.import <filename> <tablename>`

- In the report, please include the final schema of your database

##### 3. Using SQL, query the data that may tell you

	The effects of crime rates on the percentage of married men and women aged 20-24 and 25-29

- Please include the final SQL query you used to extract the necessary data
- Present your conclusion in any form easy to understand (like graphs)
- Your interpretation should be backed by the data
- "Inconclusive" is also a viable answer