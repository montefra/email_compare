#Extract emails from csv files and compare them
Author: Francesco Montesano  
email: franz.bergesund@gmail.com  
Licence: freeBSD [the license](LICENSE) file

##Dependences

    Text.Regex.Posix

##Compilation

To compile the code simply go to the `src` directory and type

    ghc --make Main

##Synopsis

The code does provide an interactive console and no command line arguments.
After starting the code with 

    /path/to/Main
the user is greeted by

    Welcome!
    For a list of available commands type 'help'
    To exit type 'exit'
    ~> 

The available commands are 

+ **help**: print this help
+ **load tag filename**: load all the email addresses from
    file *name* and attach them to *tag* keyword.
    Existing tags will be silently overwritten.
+ **diff tag1 tag2**: show the emails that are in *tag1*
    but not in *tag2*
+ **listtags**: list the tags already present
+ **printall**: print the full dictionary
+ **print tag**: print the content of tag
+ **exit**: exit the program

##Notes

This is my first attempt at Haskell, and in general to functional programming.
So I beg forgiveness for the style. But the code works.
My plan is to slowly keep improving the code. My dream is to get to a GUI.
