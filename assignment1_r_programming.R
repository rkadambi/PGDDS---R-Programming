# Bollywood Assignment Submission - Rupa Kadambi

#	Import the Bollywood data set in Rstudio in a variable named bollywood


  bollywood <- read.csv(file="bollywood.csv", head=TRUE, sep = ",")
  View(bollywood)


#	When you import a data set, R stores character vectors as factors (by default)
# You can check the structure of the data frame by using str()
  str(bollywood)

# You can change the attribute 'Movie' from factor to character type using the given command
  bollywood$Movie <- as.character(bollywood$Movie)
	 

#Q1.
#	Access the last 10 movies (from the bottom of the Bollywood data frame) using column bollywood$Movie
# Store the names of those movies in last_10 vector (in the same order)
     
	last_10 <- tail(bollywood$Movie,10)
	 
	  
#Q2.
#	Find out the total number of  missing values (NA) in the bollywood data frame.
# Store the result in na_bollywood vector
     
	na_bollywood <- sum(sapply(bollywood,function(x) sum(is.na(x))))
	  
	
#Q3
#	Write the command to find out which movie tops the list in terms of Total Collections
# Store the movie name in variable named top_movie
 
  top_movie <- bollywood[which.max(bollywood$Tcollection),"Movie"]

  
#Q4
#	Write the command to find out which movie comes second on the list in terms of Total Collections
# Store the movie name in variable named top_2_movie

  top_2_movie <- (bollywood[order(-bollywood$Tcollection),])[2,"Movie"] 
	  
	
# Now let's find out the movies shot by Shahrukh, Akshay and Amitabh separately.
# subset() function is used for that. The code has already been written for you. 
	
	shahrukh <- subset(bollywood, Lead == "Shahrukh")
	akshay <- subset(bollywood, Lead == "Akshay")
	amitabh <- subset(bollywood, Lead  == "Amitabh")

# You can view what the above data frames look like

		   
#Q5
#	What is the total collection of Shahrukh, Akshay and Amitabh movies individually?
# You can use	a column named 'Tcollection' for this 
 
        shahrukh_collection <- sum(subset(bollywood, Lead == "Shahrukh", select = "Tcollection"))
    
	akshay_collection <- sum(subset(bollywood, Lead == "Akshay", select = "Tcollection"))
    
	amitabh_collection <- sum(subset(bollywood, Lead == "Amitabh", select = "Tcollection"))
    
	
#Q6  
# Write command/s to find out how many movies are in Flop, Average, Hit and Superhit categories in the entire Bollywood data set.
table(bollywood$Verdict)

   
#You can use SAPPLY function if you want to apply a function specific columns in a data frame 
#You can write a command to find the maximum value of Ocollection, Wcollection, Fwcollecion and Tcollection using sapply
  
#Q7 
# Write a command to find the names of the movies which have the maximum Ocollection, Wcollection, Fwcollecion & Tcollection
# Store the names of 4 movies in same sequence in movie_result vector

clean_bw <- bollywood[!(apply(is.na(bollywood),1,any)),]
movie_result <- paste(clean_bw[which.max(clean_bw$Ocollection),"Movie"],
                    clean_bw[which.max(clean_bw$Wcollection),"Movie"],
                    clean_bw[which.max(clean_bw$Fwcollection),"Movie"],
                    clean_bw[which.max(clean_bw$Tcollection),"Movie"], sep = ",")



	

   
    


    
    
    