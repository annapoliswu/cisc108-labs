"""
Lab 10 python
Names: Zihan Wu , Debra Lymon
"""

"""
Problem 1.1 [5pt]: Define the function string_last,
which extracts the last character (as a string) from a non-empty string.
Note: Don’t worry about empty strings. 
"""

""" 
string_last : String -> String
consumes: String astring
produces: the last character (as a string) from a non-empty string.
"""

def string_last(astring):
    return astring[-1]
    


assert string_last("kittens") == "s"
assert string_last("puppy") == "y"
assert string_last("cisc108") == "8"

"""
Problem 1.2 [5pt]: Define the function string_join,
which consumes two strings and appends them with _ in the middle. 
"""

""" 
string_join : String String -> String
consumes: String str1, String str2
produces: strings appended with _ in the middle

"""

def string_join(str1, str2):
    return str1 + "_" + str2

assert string_join("cisc", "108") == "cisc_108"
assert string_join("","") == "_"
assert string_join("M","S") == "M_S"

"""
Problem 2.1 [5pt]: Design a function increment_timer that consumes a
TrafficTimerValue (a NatNum Interval from [0,100]) and produces the next
TrafficTimerValue. Recall that the timer resets to 0 after reaching 100.
There are two possible solutions to this problem
-using if, and
-using % [remainder]);
use either solution.
"""

""" 
increment_timer : TrafficTimerValue -> TrafficTimerValue
consumes: TrafficTimerValue ttv
produces: And produces the next TrafficTimerValue. The timer resets to 0 after reaching 100.

"""

def increment_timer(ttv):
    if ttv >= 100:
        return 0
    else:
        return ttv + 1

assert increment_timer(50) == 51
assert increment_timer(99) == 100
assert increment_timer(100) == 0
assert increment_timer(0) == 1


"""
Problem 4.1 [10pt]
Design a compound data type (in Python, a class) to represent a GeoLoc with
fields [lat] and [long].

A GeoLoc should consist of a latitude [-90,90] and longitude [-180,180]
in decimal degrees. Remember that the class MUST be named GeoLoc,
the fields are lat and long, and the selectors get_lat() and
get_long() in order for the automated grading to work.

Also design a method __str__ that produces a string representation of the given Mappable.
A GeoLoc should be displayed as follows:
"<39.689071,-75.757207>"
"""

""" 
A GeoLoc is: Float Float
Float lat, Float long
"""   

   
class GeoLoc:
    def __init__(self, lat, long ):
        self.lat = lat
        self.long = long
        
    def get_lat(self):
        return self.lat
    def get_long(self):
        return self.long
    
    def set_lat(self,n):
        self.lat = n
    def set_long(self,n):
        self.long = n    
        
    def __str__(self):
        return "<" + str( self.get_lat() ) + "," + str(self.get_long() ) + ">"
    


assert str(GeoLoc(39.689071,-75.757207)) == "<39.689071,-75.757207>"
assert str(GeoLoc(39.6837546, -75.7456899)) == "<39.6837546,-75.7456899>"
assert str(GeoLoc(39.665648,-75.749031)) == "<39.665648,-75.749031>"

"""
Problem 4.2 [10pt]
Design a data type called Restaurant to represent a place that serves food.
A Restaurant should contain fields for
-a name [name],
-a phone number [phone],
-a rating from 0 to 5 stars indicating the food
quality [rating], and
-a GeoLoc [location].

Remember that selectors/setters must be named get_name,
get_phone, etc.

Also design a method __str__ that produces a string representation of the given Mappable.
A Restaurant should be displayed as follows:
 "[3 stars] Santa Fe Mexican Grill [302-369-2500] <39.6837546,-75.7456899>"
"""

""" 
A Person is: String String Number GeoLoc
String name, String phone, Number rating, GeoLoc location
"""   

class Restaurant:
    def __init__(self, name , phone, rating, location ):
        self.name = name
        self.phone = phone
        self.rating = rating
        self.location = location
        
    def get_name(self):
        return self.name
    def get_phone(self):
        return self.phone
    def get_rating(self):
        return self.rating
    def get_location(self):
        return self.location 
    
    def set_name(self, n):
        self.name = n
    def set_phone(self, n):
        self.phone = n
    def set_rating(self, n):
        self.rating = n
    def set_location(self, n):
        self.location = n 
    
    def __str__(self):
        return "[" + str(self.get_rating() ) + " stars] " + self.get_name() + " [" + self.get_phone() + "] " + str(self.get_location() )
       

    
l1 = GeoLoc(39.6837546,-75.7456899)
r1 = Restaurant("Santa Fe Mexican Grill", "302-369-2500",3,l1)              
assert r1.get_name() == "Santa Fe Mexican Grill"
assert r1.get_phone() == "302-369-2500"
assert r1.get_rating() == 3
assert r1.get_location() == l1
assert str(r1) == "[3 stars] Santa Fe Mexican Grill [302-369-2500] <39.6837546,-75.7456899>"

"""
Problem 4.3 [10pt]
Design a data type called Person to represent your contacts.

A Person should of course include a
name [name], a phone number [phone], and GeoLoc [loc].
Also design a method __str__ that produces
a string representation of the given Mappable.
A Person should be displayed as follows:
 "Dr. Siegel [302-831-5555] <39.682531,-75.754329>" 
"""


""" 
A Person is: String String GeoLoc
String name, String phone, GeoLoc loc
"""   

class Person:
    def __init__(self, name, phone, loc ):
        self.name = name
        self.phone = phone
        self.loc = loc
        
    def get_name(self):
        return self.name
    def get_phone(self):
        return self.phone
    def get_loc(self):
        return self.loc   
    
    def set_name(self, n):
        self.name = n
    def set_phone(self, n):
        self.phone = n
    def set_loc(self, n):
        self.loc = n    
    
    def __str__(self):
        return self.get_name() + " [" + self.get_phone() + "] " + str(self.get_loc() )
    
siegel = Person("Dr. Siegel", "302-831-5555", GeoLoc(39.682531,-75.754329))
assert siegel.get_name() == "Dr. Siegel"
assert siegel.get_phone() == "302-831-5555"
assert str(siegel.get_loc()) == str(GeoLoc(39.682531,-75.754329))
assert str(siegel) == "Dr. Siegel [302-831-5555] <39.682531,-75.754329>"
