## AdWords Analysis

This project contains code related to running PPC analysis.  As of this writing, that includes only AdWords.

The analysis is broken down into X distinct steps:

1. Get the data
2. Format and enrich the data (including the creation of an `event log` aka `elog`)
3. Create data frames for tabulation/visualization
4. Create tables (.csv files) and graphical plots

# The Data

Here's a short glossary of data that we pull from various sources.

## AdWords
	
	To be written...

## Mixpanel

	To be written...

## Transactions (from Heroku Database)
	
	To be written...

# Configuration Files

Several functions within this project pull data from external APIs such as Mixpanel and AdWords.  To manage the credentials for these APIs, we keep all credentials in YAML files to read the data. Since these YAML files contain sensitive data, they are not included in our git repository.

To run the code within this project, you will need the following files. They can be obtained by reaching out to Chris (@chris on Slack; chris@mypowersupply.com via email):

1. adwords_credentials.yml
2. mixpanel_credentials.yml