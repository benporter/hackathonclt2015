# HackathonCLT 2015

**1st Place submission to <a href="http://www.hackathonclt.org/">HackathonCLT 2015</a> with my wife, Neli.**

**Code Overview:**

  - hive.txt:  have and bash commands run from the console to extract data
  - ShinyApp.R:  a standalone Shiny Application using R, with a dash of javascript for extra functionality.

**General flow of the analysis:**
  - Create summarized datasets or samples using Hive. (hive.txt)
  - Pull those datatsets out of HDFS, and download them locally (hive.txt)
  - Use R to munge data, Shiny and rCharts/ggplot to visualize data (ShinyApp.R)

**The flow of the presentation:**

Find the right store to start with:

<img src="https://github.com/benporter/hackathonclt2015/blob/master/screenshots/1%20-%20Stores%20Page.png?raw=true"/>

Then choose which customers that you want to start with, in this case the 214 Tier 1 customers that have used the Express Lane at least once in the last 6 months.

<img src="https://github.com/benporter/hackathonclt2015/blob/master/screenshots/2a%20-%20Customers.png?raw=true"/>

<img src="https://github.com/benporter/hackathonclt2015/blob/master/screenshots/2b%20-%20Customers.png?raw=true"/>

Then apply the "Clair" algorithm to identify which products to send, and to which customers, while choosing your accepted level of false positives. Every customer to the right of the organe line is omitted because we assume they are on vacation, or went to another grocery store for their last visit.

<img src="https://github.com/benporter/hackathonclt2015/blob/master/screenshots/3a%20-%20Products.png?raw=true"/>

The exact customers and products to offer (dynamically changes as the false positive rate is adjusted)

<img src="https://github.com/benporter/hackathonclt2015/blob/master/screenshots/3b%20-%20Products.png?raw=true"/>

Profitability (incomplete, ran out time) - a post implementation calculator to evaluate the profitability of the project once real data starts to come in.

<img src="https://github.com/benporter/hackathonclt2015/blob/master/screenshots/4%20-%20Profitability.png?raw=true"/>


Dataset overview extracted from Hive:

  - The "calc1" dataset feeds the Stores screen of the Shiny App
  - The "calc2" dataset feeds the Customers screen of the Shiny App
  - The "calc3" dataset is used to locate the 214 customers that are analyzed on the Customers tab, but this dataset is never downloaded.
  - The "calc4" dataset is the 6 month transaction history for the 214 customers identified from "calc3".

This analyis is not replicable because the data was only available during the competition.

