generateExperiments
===================

README is in the process of being written!  Please excuse the incomplete sections.

Target Sentences
===================
This section takes a csv file of 90 experimental sentences such as "The man saw which patient the doctor had treated in the morning") and reorganizes them for use in the rest of the script.

First it sets up the new columns for the sentences.  This includes various columns with condition information, as well as columns for block index (to keep similar sentence frames in different experimental blocks - this value is added in Excel before the csv is inputted), list number, and individual words.  Then it concatenates the sentence frames ("The man saw which patient the doctor had") and sentence continuations ("in the morning") around the target word, in this case "treated".  Before finally writing to a new csv file, it capitalizes "the" at the beginning of every sentence.


Filler sentences
===================
This function is very similar to the above function, only instead of altering experimental sentences, it alters fillers.  The filler sentences have gone through less manipulation before importing, and need more organizing in the script.

First this section separates fillers into two lists - one of sentences with congruous target words, the other with incongruous target words. (E.g. congruous = "For lunch he had grilled cheese at his favorite cafe," incongruous = "For lunch he had grilled shoe at his favorite cafe.")  Then the lists are split at line 181 (as there are two sets of fillers) and condition codes are assigned to each of the four lists.  The columns are defined (similar to above, columns with condition and indexing information).  However the frames are already concatenated, so no concatenation is needed.


Match (match)
===================
This function creates a matrix of list combinations to be used.  There are four possible lists (1,2,3,4) for filler sentences and two (1,2) for experimental sentences.  


Jumble (jumble)
===================
This function actually randomizes items in a vector.  This function is called in 'randomize', and is used to randomize the order of blocks.  


Pseudorandomize (psuedorandomize)
===================
This function pseudo-randomzies the items.  It is complicated ************


Generate Subject Files (generate.subj.files)
===================
This function generates subject files using megalist (created by calling getmegalist) and calls writefiles to write the actual .txt files.

Called by catalyst for loop at the end of script.


Get megalist (getmegalist)
===================
Takes 

Get blocks (getblocks)
===================

Randomize (randomize)
===================

Write files (write files)
===================

Write blocks (writeblock)
===================

Add target sentences (addtarg)
===================


Add fillers (addfill)
===================

Interweave triggers (interweaveT
===================


Catalyst for loop 
===================






