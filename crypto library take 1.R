#frequency analyzer program version 0.1.1
#Andrew McCartney

#initial frequency data here:
english1<-c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
english2<-c(0.08167,0.01492,0.02782,0.04253,0.12702,0.02228,0.02015,0.06094,0.06966,0.00153,0.00772,0.04025,0.02406,0.06749,0.07507,0.01929,0.00095,0.05987,0.06327,0.09056,0.02758,0.00978,0.0236,0.0015,0.01974,0.00074)
englishnum<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26)
caesar<-data.frame(english1,englishnum)
english.base.freq<-data.frame(english1,english2)

most.common.words<-c("the","be","to","of","and","a","in","that","have","I","it","for","not","on","with","he","as","you","do","at")
most.common.letter.pairs<-c("TH","HE","AN","RE","ER","IN","ON","AT","ND","ST","ES","EN","OF","TE","ED","OR","TI","HI","AS","TO")
most.common.doubles<-c("LL","EE","SS","OO","TT","FF","RR","NN","PP","CC")
most.common.initial.letters<-c("t","o","a","w","b","c","d","s","f","m","r","h","i","y","e","g","l","n","p","u","v","j","k","q","z","x")

############
#alphabet permutations:

stand.english<-"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
reverse.english<-"ZYXWVUTSRQPONMLKJIHGFEDCBAzyxwvutsrqponmlkjihgfedcba"
english.by.frequency<-"ETAOINSHRDLCUMWFGYPBVKJXQZetaoinshrdlcumwfgypbvkjxqz"
eng.caps<-"ABCDEFGHIJKLMNOPQRSTUVWXYZ"


############
#Text examples
tamam.shud<-"WRGOABABD
WTBIMPANETP
MLIABOAIAQC
ITTMTSAMSTGAB"
#The GOlden Bug, E.A.Poe
gold.bug<-"Hereupon Legrand arose, with a grave and stately air, and brought me the beetle
from a glass case in which it was enclosed. It was a beautiful scarabaeus, and, at
that time, unknown to naturalists-of course a great prize in a scientific point
of view. There were two round black spots near one extremity of the back, and a
long one near the other. The scales were exceedingly hard and glossy, with all the
appearance of burnished gold. The weight of the insect was very remarkable, and,
taking all things into consideration, I could hardly blame Jupiter for his opinion
respecting it."



################

#analytical functions

#find the average number of characters per word:
avg.char<-function(x){
  avg<-mean(nchar(x))
  print(avg)
}
#convert a string of text into individual letter elements then print them without quotations
to.letters<-function(x){
  cat(substring(x, 1:nchar(x),1:nchar(x)))
}


#cipher functions

#an atbash cipher function
atbash<-function(x){
  cipher<-chartr(stand.english,reverse.english, x)
  return(cat(cipher))
}


# a cipher to translate each letter, by position, to the letters in order of frequency
frequencycipher<-function(x){
  cipher<-chartr(stand.english,english.by.frequency,x)
  return(cat(cipher))
}
#to decrypt the previous
frequencycipherdecrypt<-function(x){
  cipher<-chartr(english.by.frequency,stand.english,x)
  return(cat(cipher))
}


keyword.cipher<-function(txt,key){
  txt<-"This is the song that never ends"
  txt<-toupper(txt)
  txt
  key<-"oriolle"
  key<-toupper(key)
  key
  let<- print(substring(txt, 1:nchar(txt),1:nchar(txt)))
  kkey<-print(substring(key, 1:nchar(key),1:nchar(key)))
  eng.caps<-LETTERS
  eng.caps
  keyless<-gsub(key,"",eng.caps,fixed=TRUE); keyless
  toString()
  ?gsub
  crypt<-paste(kkey,keyless)
  crypt
  cipher<-chartr(LETTERS,crypt,txt)
  decipher<-chartr(crypt,LETTERS,txt)
  return(cat(cipher,decipher))
}
?strsplit
keyword.cipher(ciphertext,key="oriolle")


kelsey<-"KELSYABCDFGHIJMNOPQRTUVWXZ"



#let's put here a keyword cipher function

#and of course we'll need an affine cipher function
affine<-function(x,a,b){
  
}

a(n)+b %% 26

# a function to do rotational caesar ciphers (not my IP!)
rot <- function(ch, k = 13) {
  p0 <- function(...) paste(c(...), collapse = "")
  A <- c(letters, LETTERS, " '")
  I <- seq_len(k); chartr(p0(A), p0(c(A[-I], A[I])), ch)
}

#VIGENERE'S CIPHER CAN BE CODED!
#ONE MUST SPECIFY --DEAR GOD-- A TABULA RECTA
#AND THEN VIGENERE'S CAN CALL TO IT NUMERICALLY WITH INPUTS, E.G. [N,K]



##################################################
#       examples and code to play with           #
##################################################
noquote(strsplit("A text I want to display with spaces", NULL)[[1]])


#functions for dealing with strings of text for later use:
strsplit(x, "_")
  #where x is the name of the object of text and 
  # _ is the letter or character which causes a word split whenever it appears

#noquote(function())
#returns the results of another function of text without quotation marks around each item of text

grep("[a-z]", letters)
#replaces each letter with its corresponding number

# paste
PI = paste("The life of", pi)
# paste concatenates different data types into one string of text

# sort (increasing order)
set11 = c("today", "produced", "example", "beautiful", "a", "nicely")
sort(set11, decreasing = TRUE)
## [1] "today" "produced" "nicely" "example" "beautiful" "a"


#this should be the final call of every encryption function:
chartr("iXs", "why", x)
#where the first argument is the value of the old text (e.g. zxy, atbash 
# and the second argument is the new text (abc, atbash) and the third 
# argument is the name of the objet of strings involved. this will print
# a changed version. ) 

# replace 2nd letter with hash symbol
x = c("may", "the", "force", "be", "with", "you")
substr(x, 2, 2) <- "#"
x
## [1] "m#y" "t#e" "f#rce" "b#" "w#th" "y#u"


## -- Very simple insecure crypto --
rot <- function(ch, k = 13) {
  p0 <- function(...) paste(c(...), collapse = "")
  A <- c(letters, LETTERS, " '")
  I <- seq_len(k); chartr(p0(A), p0(c(A[-I], A[I])), ch)
}
?...

pw <- "my secret pass phrase"
(crypw <- rot(pw, 13)) #-> you can send this off

## now ``decrypt'' :
rot(crypw, 54 - 13) # -> the original:
stopifnot(identical(pw, rot(crypw, 54 - 13)))

###########################################
x <- c(as = "asfef", qu = "qwerty", "yuiop[", "b", "stuff.blah.yech")
x
# split x on the letter e
strsplit(x, "e")
