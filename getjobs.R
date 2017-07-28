getjobs <- function(title,
                    phrase = "",
                    direct = "",
                    areacode = "",
                    country = "",
                    state = "",
                    skill = "",
                    cityzip = "",
                    ip = "",
                    age = "",
                    diceid = "",
                    sortcode = "",
                    sortdirection = "")
{
  # Set parameters for forming a URL

  # This parameter specifies search text for the entire job description
  # The value of each parameter is a string and should be entered in quotes

  # jobphrase parameter is used to specify whether the search should be for an exact phrase
  # If job title is a phrase (e.g. "data scientist"), then the title will be encouded as a phrase into a search URL
  # Otherwise, jobs containing both words ("data" and "scientist"), in no particular order, will be retreived

  if (phrase == "1"){

    formatted_title <- paste("%22",gsub(" ", "%20",title),"%22", sep = "")
    par_title <- paste("text","=",formatted_title,sep = "")

  } else {

    formatted_title <- gsub(" ", "%20",title)
    par_title <- paste("text","=",formatted_title,sep = "")

  }

  # If the value of this parameter is "1" then jobs returned will be for direct hires

  par_direct <- paste("direct","=",direct,sep = "")

  # This parameter specifies the area code of the job

  par_country <- paste("country","=",country,sep = "")

  # This parameter specifies the US state where the job is listed

  par_state <- paste("state","=",state,sep = "")

  # This parameter specifies the search text for the skill property of each job listing

  par_skill <- paste("skill","=",skill,sep = "")

  # This parameter specifies the city where the job is listed.
  # A zip code is required for this parameter.
  # Jobs withing 40 mile radius of the zip code will be retreived

  par_cityzip <- paste("city","=",cityzip,sep = "")

  # This parameter specifies an IP address that will be used to look up a geocode which will be used in the search

  par_areacode <- paste("areacode","=",areacode,sep = "")

  # This parameter specifies an IP address that will be used to look up a geocode which will be used in the search

  par_ip <- paste("ip","=",ip,sep = "")

  # This parameter specifies the age of the posting in days

  par_age <- paste("age","=",age,sep = "")

  # This parameter specifies Dice ID of a company posting jobs
  # Only jobs from that company will be retreived

  par_diceid <- paste("diceid","=",diceid,sep = "")

  # This parameter specifies how retreived jobs will be sorted:
  # - sort=1 sorts by posted age
  # - sort=2 sorts by job title
  # - sort=3 sorts by company
  # - sort=4 sorts by location

  par_sortcode <- paste("sort","=",sortcode,sep = "")

  # This parameter specifies sort direction
  # - sd=a sort order is ASCENDING
  # - sd=d sort order is DESCENDING

  par_sortdirection <- paste("sort","=",sortdirection,sep = "")

  # Forming an intiatial URL.
  # This URL is used to retreive jobs via API based on the values of parameters supplied

  initial_url <- paste("http://service.dice.com/api/rest/jobsearch/v1/simple.json?",
                       par_title,"&",
                       par_direct,"&",
                       par_country,"&",
                       par_state,"&",
                       par_cityzip,"&",
                       par_areacode,"&",
                       par_skill,"&",
                       par_ip,"&",
                       par_age,"&",
                       par_diceid,"&",
                       par_sortcode,"&",
                       par_sortdirection,
                       sep = ""
  )

  # Loading JSON data from the URL.Note that jdata is a list of 6 elements.

  jdata <- fromJSON(curl(initial_url, handle = curl::new_handle("useragent" = "RStudio")))

  # We save the total number of job descriptions to be retreived. This value is element 1 in the jdatalist

  job_count <- as.numeric(jdata[1])

  # Number of total JSON pages is calculated by dividing the number of total jobs by 50, which is the number of jobs listed on each JSON page by default

  page_count <- ceiling(job_count/50)

  # Creating a data frame where all job descriptions will be stored

  job_table <- data.frame()

  # Creating a temporary data frame where job descriptions from each page will be temprarily stored until they are appended to the job_table data frame

  job_temp_table <- data.frame()

  # Inform the user that no jobs matching the search criteria supplied via function parameters have been found

  if (job_count == 0) {

    cat("0 jobs found")

  }

  # There are different approaches to handling output depending on the number of jobs found
  # This has to do with the fact that JSON pages have slightly different structure depending such things as:
  # - The total number of pages containing jobs matching the description
  # - Whether the page crawled is the first or last page

  # This code handles jobs fitting on less than a page
  # There are 50 jobs per page by defaul)

  else if ((job_count >= 1) & (job_count < 50)) {

    page_url <- paste(initial_url,"&page=1",sep = "")

    # Reading JSON data into a list called jdata

    jdata <- fromJSON(page_url)
    job_temp_table <- data.frame(jdata[4])

    # This is a simple status bar that shows progress in scraping jobs from each page

    message <- paste("Downloading",job_count,"jobs:", sep = " ")
    cat(message)

    for (job_id in 1:job_count){

      # If there is an error reading or scraping a particular URL, then move to next URL

      try({

        job_html <- read_html(curl(job_temp_table$resultItemList.detailUrl[job_id], handle = curl::new_handle("useragent" = "RStudio")))
        job_node <- html_node(job_html,"#jobdescSec")
        job_text <- html_text(job_node)
        job_temp_table$Description[job_id] <- job_text

        #This is a part of a simple status bar that shows progress in scraping jobs from each page

        cat("*")

      })

    }

    # Clear the screen to avoid clutter

    cat("\014")

    # Renaming columns in the dataframe

    colnames(job_temp_table) <- c("JobURL", "JobTitle", "Company", "JobLocation", "JobDate", "JobDescription")

    # Dataframe containing job descriptions and metadata is returned by the function

    job_table <- job_temp_table
    return(job_table)

    # This code handles jobs fitting on one or more pages
    # There are 50 jobs per page by default

  } else {

    for (i in 1:page_count){

      # A page URL is formed. Page number is supplied as a parameter

      page_url <- paste(initial_url,"&page=",i,sep = "")

      # Reading JSON data into a list called jdata

      jdata <- fromJSON(page_url)

      # If it's not first or last page (where the list returned contains 6 elements), save the 6th element of the data frame into the temporary data frame job_temp_table.

      if ((i != 1) && (i != page_count)) {

        job_temp_table <- data.frame(jdata[6])

        # The first and the last page are saved into a list of only 5 elements, since previous URL element is absent on the first page and next URL element is absent on the last page. Thus, the 5th element of the jdata list contains the needed job description data.

      } else {

        page_url <- paste(initial_url, "&page=",i,sep = "")
        jdata <- fromJSON(page_url)
        job_temp_table <- data.frame(jdata[5])

      }

      # Inserting a new column named Description into the temporary data frame. This is where job descriptions will be added for every job

      job_temp_table["Description"] <- ""

      # Calculating the total number of jobs to be loaded for each page using the firstDocument and lastDocument elements in the list. The number of jobs to be downloaded will be the 50 for all job poges except the last one.

      jobs_to_load <- as.integer(jdata[3]) - as.integer(jdata[2]) + 1

      # This is a part of asimple status bar. It will show the user which page is being scraped

      cat(paste("Downloading ",jobs_to_load, " jobs from page ",i," out of ",page_count,": ", sep = ""))

      # This innner loop uses job URLs to download job descriptions for each of the jobs listed on a JSON page and saves these descriptions in the "Description" column of the temporary data frame job_temp_table

      for (job_id in 1:jobs_to_load){

        # If there is an error reading or scraping a particular URL, then move to next URL

        try({

          job_html <- read_html(curl(job_temp_table$resultItemList.detailUrl[job_id], handle = curl::new_handle("useragent" = "RStudio")))
          job_node <- html_node(job_html,"#jobdescSec")
          job_text <- html_text(job_node)
          job_temp_table$Description[job_id] <- job_text

          # This is a part of a simple status bar that shows progress in scraping jobs from each page

          cat("*")

        })
      }

      # Job data from the page is appended to the global data frame where all job descriptions are stored

      job_table <- rbind.data.frame(job_table, job_temp_table)

      # Output screen is cleared to avoid clutter

      cat("\014")
    }

    # Rrenaming the columns of the data frame

    colnames(job_table) <- c("JobURL", "JobTitle", "Company", "JobLocation", "JobDate", "JobDescription")

    # Dataframe containing job descriptions and metadata is returned by the function

    return(job_table)
  }

}
