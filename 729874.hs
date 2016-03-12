--
-- MATHFUN
-- Functional Programming Coursework
-- 729874
--

import Text.Printf
import Data.List
import System.IO
import Control.Exception
--
-- Types
--
type Rating = (String, Int)
type Film = (String, String, Int, [Rating])
-- Database used for demo testing purposes
testDatabase :: [Film]
testDatabase = [("Blade Runner", "Ridley Scott", 1982, [("Amy",5), ("Bill",8), ("Ian",7), ("Kevin",9), ("Emma",4), ("Sam",7), ("Megan",4)]), 
                ("The Fly", "David Cronenberg", 1986, [("Megan",4), ("Fred",7), ("Chris",5), ("Ian",0), ("Amy",6)]), 
                ("Psycho", "Alfred Hitchcock", 1960, [("Bill",4), ("Jo",4), ("Garry",8), ("Kevin",7), ("Olga",8), ("Liz",10), ("Ian",9)]), 
                ("Body Of Lies", "Ridley Scott", 2008, [("Sam",3), ("Neal",7), ("Kevin",2), ("Chris",5), ("Olga",6)]), (
                "Avatar", "James Cameron", 2009, [("Olga",1), ("Wally",8), ("Megan",9), ("Tim",5), ("Zoe",8), ("Emma",3)]), 
                ("Titanic", "James Cameron", 1997, [("Zoe",7), ("Amy",1), ("Emma",5), ("Heidi",3), ("Jo",8), ("Megan",5), ("Olga",7), ("Tim",10)]), 
                ("The Departed", "Martin Scorsese", 2006, [("Heidi",3), ("Jo",8), ("Megan",5), ("Tim",3), ("Fred",5)]), 
                ("Aliens", "Ridley Scott", 1986, [("Fred",9), ("Dave",6), ("Amy",10), ("Bill",7), ("Wally",1), ("Zoe",5)]), 
                ("Kingdom Of Heaven", "Ridley Scott", 2005, [("Garry",3), ("Chris",7), ("Emma",5), ("Bill",1), ("Dave",3)]), (
                "E.T. The Extra-Terrestrial", "Steven Spielberg", 1982, [("Ian",9), ("Amy",1), ("Emma",7), ("Sam",8), ("Wally",5), 
                ("Zoe",6)]), ("Bridge of Spies", "Steven Spielberg", 2015, [("Fred",3), ("Garry",4), ("Amy",10), ("Bill",7), ("Wally",6)]), 
                ("Vertigo", "Alfred Hitchcock", 1958, [("Bill",8), ("Emma",5), ("Garry",1), ("Kevin",6), ("Olga",6), ("Tim",10)]), 
                ("The Birds", "Alfred Hitchcock", 1963, [("Garry",7), ("Kevin",8), ("Olga",4), ("Tim",8), ("Wally",3)]), 
                ("Jaws", "Steven Spielberg", 1975, [("Fred",3), ("Garry",0), ("Jo",3), ("Neal",9), ("Emma",7)]), 
                ("The Martian", "Ridley Scott", 2015, [("Emma",7), ("Sam",8), ("Wally",5), ("Dave",10)]), 
                ("The Shawshank Redemption", "Frank Darabont", 1994, [("Jo",8), ("Sam",10), ("Zoe",4), ("Dave",7), ("Emma",3), ("Garry",10), ("Kevin",7)]), 
                ("Gladiator", "Ridley Scott", 2000, [("Garry",7), ("Ian",4), ("Neal",5), ("Wally",3), ("Emma",4)]), 
                ("The Green Mile", "Frank Darabont", 1999, [("Sam",3), ("Zoe",4), ("Dave",7), ("Wally",5), ("Jo",5)]), 
                ("True Lies", "James Cameron", 1994, [("Dave",3), ("Kevin",10), ("Jo",0)]), 
                ("Super 8", "J J Abrams", 2011, [("Dave",7), ("Wally",3), ("Garry",5), ("Megan",4)]), 
                ("Minority Report", "Steven Spielberg", 2002, [("Dave",6), ("Garry",6), ("Megan",2), ("Sam",7), ("Wally",8)]), 
                ("War Horse", "Steven Spielberg", 2011, [("Dave",6), ("Garry",6), ("Megan",3), ("Sam",7), ("Wally",8), ("Zoe",8)]), 
                ("The Terminal", "Steven Spielberg", 2004, [("Olga",8), ("Heidi",8), ("Bill",2), ("Sam",6), ("Garry",8)]), 
                ("Star Wars: The Force Awakens", "J J Abrams", 2015, [("Olga",6), ("Zoe",6), ("Bill",9), ("Sam",7), ("Wally",8), ("Emma",8)]), 
                ("Hugo", "Martin Scorsese", 2011, [("Sam",9), ("Wally",3), ("Zoe",5), ("Liz",7)])]
                
-- List of Ratings used for testing purposes
testRatings :: [Rating]
testRatings = [("Amy",5), ("Bill",8), ("Ian",7), ("Kevin",9), ("Emma",4), ("Sam",7), ("Megan",4)]
--
--
---------------------------  Functional code ---------------------------
--
------------------------------
-- Add new film to database --
------------------------------

addFilm :: String -> String -> Int -> [Film] -> [Film]
addFilm name director year film  = (name, director, year, []) : film 

------------------------------------------------------
-- Display list of films as a well formatted string --
------------------------------------------------------

filmsAsString :: [Film] -> String
filmsAsString []            = ""
filmsAsString (film: films) = getFilm film ++ "\n\n\n" ++ filmsAsString films

-- Helper function used by FilmsAsString for the purpose of pulling the elements of each film as a string
getFilm :: Film -> String
getFilm (name, director, year, ratings) = "Film name: " ++ name ++ "\nDirector: " ++ director ++
                                          "\nYear of Release: " ++ show year ++  "\nWebsite Rating: " ++ 
                                          printf "%.2f" (getWebsiteRating( ratings ))

-- Helper function used for the purpose of finding a film's average rating.
getWebsiteRating :: [Rating] -> Float
getWebsiteRating rating = getRatingsTotal(rating) / getNumRatings(rating)

-- Helper function used by getWebsiteRating to find the sum of all of a films ratings
getRatingsTotal :: [Rating] -> Float
getRatingsTotal []                         = 0
getRatingsTotal ((name, rating) : ratings) = fromIntegral rating + getRatingsTotal ratings 

-- Helper function used by getWebRating to find out how many ratings a film has
getNumRatings :: [Rating] -> Float
getNumRatings []                 =  0
getNumRatings (rating : ratings) =  1 + getNumRatings ratings 

---------------------------------------------- 
-- Filtering list of films by Director name --
----------------------------------------------

filterByDirector directorName films = filter (matchDirector directorName) films

-- Helper function used as the predicate for filterByDirector to find films that have been made by the specified directors
matchDirector directorName (_, director, _, _) = director == directorName
-------------------------------------------------
-- Filtering out films with rating less than 7 --
-------------------------------------------------

filterByRating films = filter topRated films

-- Helper function used as a predicate for filterByRating to find films that have a website rating greater than 7
topRated (_, _, _, rating) = getWebsiteRating(rating) >= 7

---------------------------------------------------------
-- Display average website rating for a given Director --
---------------------------------------------------------

getAverageRatingForDirector :: String -> [Film] -> String
getAverageRatingForDirector directorName films = directorName ++ "'s average website rating is: " ++ 
                                                 printf "%.2f"(getAverageWebsiteRating(filterByDirector directorName films))

-- Helper function used by getAverageRatingForDirector to work out the average website rating of a director's films
getAverageWebsiteRating :: [Film] -> Float
getAverageWebsiteRating films = getWebRatingTotal(films) / getNumWebRatings(films)

-- Helper function used by getAverageWebsiteRating to work out the sum of all of the website ratings that a director has
getWebRatingTotal :: [Film] -> Float
getWebRatingTotal []                            = 0
getWebRatingTotal ((_, _, _, ratings) : films) = getWebsiteRating(ratings) + getWebRatingTotal(films)

-- Helper function used by getAverageWebsiteRating to work out how many website ratings a director has
getNumWebRatings :: [Film] -> Float
getNumWebRatings []             = 0
getNumWebRatings (film : films) = 1 + getNumWebRatings(films)

---------------------------------------------------------------------------------
-- Display the film Titles and rating for all ratings made by a specified user --
---------------------------------------------------------------------------------

findRating :: [Film] -> String -> [Rating]
findRating films searchUser = [(filmTitle, usersRating ratings searchUser) | (filmTitle, _, _, ratings) <- films, usersRating ratings searchUser <= 10]

usersRating :: [Rating] -> String -> Int
usersRating ((userName, userRating) : ratings) searchName = if userName == searchName 
                                                            then userRating 
                                                            else if ratings == [] 
                                                                 then 1337 
                                                                 else usersRating ratings searchName
                                                                 
----------------------------------------------------------
-- Display a list of Ratings as a well formatted string --
----------------------------------------------------------

ratingsAsString :: [Rating] -> String
ratingsAsString []                 = ""
ratingsAsString (rating : ratings) = ratingAsString(rating) ++ ratingsAsString(ratings)

-- Helper function used by ratingsAsString to turn a single film rating into a well formatted string 
ratingAsString :: Rating -> String
ratingAsString (filmName, rating) = filmName ++ ": " ++ show rating ++ "\n"

-----------------------------------------------
-- Allow a user to make a rating for a movie --
-----------------------------------------------

rateFilm :: [Film] -> String -> String -> Int -> [Film]
rateFilm films filmName userName rating 
    | isFilmRated films filmName userName == True = changeOldRating films filmName userName rating
    | otherwise = makeNewRating films filmName userName rating

-- Helper function used by rateFilm to update a users rating on a film
changeOldRating :: [Film] -> String -> String -> Int -> [Film]
changeOldRating ((filmName, director, year, ratings) : films) searchFilm ratingUser ratingScore = if filmName == searchFilm
                                                                                                  then (filmName, director, year, map (\p@(searchName, _) -> if searchName == ratingUser then (ratingUser, ratingScore) else p)ratings) : films
                                                                                                  else if films == []
                                                                                                       then (filmName, director, year, ratings) : []
                                                                                                       else (filmName, director, year, ratings) : changeOldRating films searchFilm ratingUser ratingScore

-- Helper function used by rateFilm to add a new rating to a film for the user   
makeNewRating :: [Film] -> String -> String -> Int -> [Film]
makeNewRating ((filmName, director, year, ratings) : films) searchFilm ratingUser ratingScore = if filmName == searchFilm
                                                         then (filmName, director, year, (ratingUser, ratingScore) : ratings) : films
                                                         else if films == []
                                                              then (filmName, director, year, ratings) : []
                                                              else (filmName, director, year, ratings) : makeNewRating films searchFilm ratingUser ratingScore



-- Helper function used by rateFilm to check if the film the user is rating is present in the database, 
-- and uses hasUserRated to check if the user has already rated it
isFilmRated :: [Film] -> String -> String -> Bool
isFilmRated ((filmName, _, _, ratings) : films ) searchFilm  searchUser=  if filmName == searchFilm
                                                                            then True && hasUserRated ratings searchUser
                                                                            else if films == [] then False
                                                                                                 else isFilmRated films searchFilm searchUser

-- Helper function used by isFilmRated to check if a user has already rated the film that they are rating
hasUserRated :: [Rating] -> String -> Bool
hasUserRated ((userName, _) : ratings) searchUser = if userName == searchUser
                                                    then True
                                                    else if ratings == [] then False 
                                                                          else hasUserRated ratings searchUser

--------------------------------------------------
-- Print films released in a defined time range --
--------------------------------------------------

filterByYear minYear maxYear films = sortBy sortYear (filter (matchYear (maxYear, minYear)) films)

-- Helper function used as a predicate for filterByYear to find films
-- that are within the user defined time period
matchYear (minYear, maxYear) (_, _, year, _) = minYear >= year && year >= maxYear

-- Helper function used by filterByYear for defining the ruleused by sortBy 
-- to sort each filtered film in descending order of their websiteRating
sortYear :: Film -> Film -> Ordering
sortYear (_, _, _, firstRatings) (_, _, _, secondRatings)
    | getWebsiteRating(firstRatings) < getWebsiteRating(secondRatings) = GT
    | getWebsiteRating(firstRatings) > getWebsiteRating(secondRatings) = LT
    | otherwise                                                        = GT

-------------------------------------
--------------- DEMO ----------------
-------------------------------------

-- Demo function to test basic functionality (without persistence - i.e.
-- testDatabase doesn't change and nothing is saved/loaded to/from file).

demo :: Int -> IO ()
demo 1  = putStrLn(filmsAsString(addFilm "The BFG" "Steven Spielberg" 2016 testDatabase))
demo 2  = putStrLn(filmsAsString testDatabase)
demo 3  = putStrLn(filmsAsString(filterByDirector "Ridley Scott" testDatabase))
demo 4  = putStrLn(filmsAsString(filterByRating testDatabase))
demo 5  = putStrLn(getAverageRatingForDirector "Ridley Scott" testDatabase)
demo 6  = putStrLn(ratingsAsString(findRating testDatabase "Emma"))
demo 7  = putStrLn(filmsAsString(rateFilm testDatabase "Hugo" "Emma" 10))
demo 77 = putStrLn(filmsAsString(rateFilm testDatabase "Avatar" "Emma" 10))
demo 8  = putStrLn(filmsAsString(filterByYear 2010 2014 testDatabase))


---------------------------------------------------------------------------
--------------------------- User Interface Code ---------------------------
---------------------------------------------------------------------------

------------------------------------------------
-- Function to retrieve user input as Integer --
------------------------------------------------

getInt :: IO Int
getInt = do
    str <- getLine
    if isInt str True
        then return (read str :: Int)
        else return (-1)
        
        
-- Helper function used by getInt to ensure that the user hasn't entered 
-- a string value when the program expects an integer
isInt :: String -> Bool -> Bool
isInt [] firstChar = if firstChar then False
                                  else True
isInt (digit : digits) firstChar
    | digit == '0' = True && isInt digits False
    | digit == '1' = True && isInt digits False
    | digit == '2' = True && isInt digits False
    | digit == '3' = True && isInt digits False
    | digit == '4' = True && isInt digits False
    | digit == '5' = True && isInt digits False
    | digit == '6' = True && isInt digits False
    | digit == '7' = True && isInt digits False
    | digit == '8' = True && isInt digits False
    | digit == '9' = True && isInt digits False
    | otherwise    = False

-- Helper function used throughout the program to verify integer inputs
validInt :: Int -> Bool
validInt checkInt
    | checkInt == -1 = False 
    | otherwise      = True

-- Helper function used throughout the program to display error message when 
-- invalid value entered instead of integer
intError :: IO()
intError = do
    putStrLn "Error: Invalid Input -> value entered was not a number"
    
----------------------------------------------
-- Function to load film database from file --
----------------------------------------------
loadFilms :: IO [Film]
loadFilms = do
    stringedFilms <- readFile "database.txt"
    let filmDatabase = read stringedFilms :: [Film]
    return filmDatabase
 
-----------------------------------------------------
-- Function to save list of films to database file --
-----------------------------------------------------

saveFilms :: [Film] -> IO ()
saveFilms films = do
    writeFile "database.txt" (show films)
    putStrLn("Database updated")

--------------------------------------------------    
-- User interface code for implementing addFilm --
--------------------------------------------------

ioAddFilm :: IO()
ioAddFilm = do    
    putStrLn "------------------------------------------------------"
    putStrLn "----------------------Add Film------------------------"
    putStrLn "------------------------------------------------------"
    films <- loadFilms
    putStrLn "Please enter the title of the new Film                "
    title <- getLine
    putStrLn "Please enter the name of the new film's Director      "
    director <- getLine
    putStrLn "Please enter the year of release for the new film     "
    year <- getInt
    putStrLn "\n\n"
    if validInt year
        then 
            if isFilmPresent films director
                then putStrLn "Error: A film already exists wit hthat name"
                else saveFilms(addFilm title director year films)
        else intError
    main

-- Helper function to ensure a film doesn't already exist in the database before saving it
isFilmPresent :: [Film] -> String -> Bool
isFilmPresent ((filmName, _, _, ratings) : films ) searchFilm =  if filmName == searchFilm
                                                                            then True
                                                                            else if films == [] then False
                                                                                                 else isFilmPresent films searchFilm
--------------------------------------------------------
-- User interface code for implementing filmsAsString --
--------------------------------------------------------

ioPrintFilms :: IO()
ioPrintFilms = do
    putStrLn "------------------------------------------------------"
    putStrLn "--------------------Print Films ----------------------"
    putStrLn "------------------------------------------------------"
    films <- loadFilms
    putStrLn (filmsAsString films)
    putStrLn "\n\n"
    main

-----------------------------------------------------------
-- User interface code for implementing filterByDirector --
-----------------------------------------------------------

ioFilterDirector :: IO()
ioFilterDirector = do
    putStrLn "------------------------------------------------------"
    putStrLn "------------- Filter films by director ---------------"
    putStrLn "------------------------------------------------------"
    films <- loadFilms
    putStrLn "Enter name of Director you want to filter by:         "
    director <- getLine
    putStrLn "\n"
    if filterByDirector director films == [] 
        then putStrLn "Error: There are no films by a director with that name"
        else putStrLn (filmsAsString (filterByDirector director films))
    main

---------------------------------------------------------
-- User interface code for implementing filterByRating --
---------------------------------------------------------

ioFilterRating :: IO()
ioFilterRating = do
    putStrLn "------------------------------------------------------"
    putStrLn "------------- Films rated 7 or higher ----------------"
    putStrLn "------------- Films rated 7 or higher ----------------"
    putStrLn "------------------------------------------------------"
    films <- loadFilms
    putStrLn (filmsAsString( filterByRating films))
    main

----------------------------------------------------------------------
-- User interface code for implementing getAverageRatingForDirector --
----------------------------------------------------------------------

ioDirectorAverage :: IO()
ioDirectorAverage = do
    putStrLn "------------------------------------------------------"
    putStrLn "-------- Print average rating for a director ---------"
    putStrLn "------------------------------------------------------"
    films <- loadFilms
    putStrLn "Enter the name of the director whose rating you want  "
    director <- getLine
    if filterByDirector director films == [] 
        then putStrLn "Error: There are no films by a director with that name"
        else putStrLn (getAverageRatingForDirector director films)
    main

-----------------------------------------------------    
-- User interface code for implementing findRating --
-----------------------------------------------------

ioFilterUser :: IO()
ioFilterUser = do
    putStrLn "------------------------------------------------------"
    putStrLn "----------- Filter website ratings by User -----------"
    putStrLn "------------------------------------------------------"
    films <- loadFilms
    putStrLn "Enter the name of the user whose ratings you want     "
    name <- getLine
    if findRating films name == []
        then putStrLn "Error: There are no ratings recorded under that name"
        else putStrLn (ratingsAsString(findRating films name))
    main
    
---------------------------------------------------
-- User interface code for implementing rateFilm --
---------------------------------------------------

ioRateFilm :: IO()
ioRateFilm = do
    putStrLn "------------------------------------------------------"
    putStrLn "--------------------- Rate Film ----------------------"
    putStrLn "------------------------------------------------------"
    films <- loadFilms
    putStrLn "Enter the name of the film you want to rate           "
    title <- getLine
    putStrLn "Enter the name of the user rating the film            "
    name <- getLine
    putStrLn "Enter the rating the user has given the film (0 - 10) "
    rating <- getInt
    if validInt rating
        then
            if isFilmPresent films title 
                then saveFilms(rateFilm films title name rating)
                else putStrLn "Error: There is no film in the database with that title"
        else intError
    main
-------------------------------------------------------
-- User interface code for implementing filterByYear --
-------------------------------------------------------

ioYearSort :: IO()
ioYearSort = do
    putStrLn "------------------------------------------------------"
    putStrLn "----- Print all films released in a time period ------"
    putStrLn "------------------------------------------------------"
    films <- loadFilms
    putStrLn "Enter the year at the start of the range              "
    minYear <- getInt
    putStrLn "Enter the year at the end of the range                "
    maxYear <- getInt
    if validInt minYear && validInt maxYear
        then
            putStrLn (filmsAsString (filterByYear minYear maxYear films))
        else intError
    main

-------------------------------------------------
-- User interface code for exiting the program --
-------------------------------------------------

ioExit :: IO()
ioExit = do
    putStrLn "------------------------------------------------------"
    putStrLn "-- Thank you for using the film database, goodbye! ---"
    putStrLn "------------------------------------------------------"

----------------------------------------------------------------------------------------------    
-- User interface code for refreshing the database file back to testDatabase during testing --
----------------------------------------------------------------------------------------------

ioFileRefresh :: IO()
ioFileRefresh = do
    putStrLn "------------------------------------------------------"
    putStrLn "---- Database has been restored back to test data ----"
    putStrLn "------------------------------------------------------"
    saveFilms testDatabase
    main

-------------------    
-- Main function --
-------------------

main :: IO()
main = do 
    putStrLn "------------------------------------------------------"
    putStrLn "----------Welcome to the Film Rate Database-----------"
    putStrLn "------------------------------------------------------" 
    putStrLn "Please choose an option from below:                   " 
    putStrLn "------------------------------------------------------" 
    putStrLn "1 : Add film to database                              " 
    putStrLn "2 : Print database to screen                          " 
    putStrLn "3 : Print all films made by a given Director          " 
    putStrLn "4 : Print films with an average rating of 7 or higher " 
    putStrLn "5 : Print average rating for a given director         " 
    putStrLn "6 : Print titles of films rated by a specified user   " 
    putStrLn "7 : Rate a film                                       " 
    putStrLn "8 : Print all films released in a specific time period" 
    putStrLn "9 : Exit Program                                      "
    putStrLn "------------------------------------------------------" 
    putStrLn "User Option: "
    option <- getInt
    if validInt option 
    then
        case option of
            1 -> ioAddFilm
            2 -> ioPrintFilms
            3 -> ioFilterDirector
            4 -> ioFilterRating
            5 -> ioDirectorAverage
            6 -> ioFilterUser
            7 -> ioRateFilm
            8 -> ioYearSort
            9 -> ioExit
            1337 -> ioFileRefresh -- Option added for refreshing database file during testing.
            _ -> putStrLn "Error: Case option does not exist."
    else do
            intError
            main
        