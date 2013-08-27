generateExperiments
===================

README is in the process of being written...  Please excuse any incomplete sections!

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
This function pseudo-randomzies the items.  Pseudorandomizing creates a mostly random list, but ensures items of the same type do not appear many times in a row.  Pseudorandomizing first randomzies the items within blocks, and then passes through the list looking for 2 or more items of the same _ and 3 or more items of the same _.  If too many of one kind of item are in a row, the item is moved to the bottom of the list.  This process is continued until the end of the block is reached, and repeated for all five blocks.


Generate Subject Files (generate.subj.files)
===================
This function generates subject files using megalist (created by calling getmegalist) and calls writefiles to write the actual .txt files.

Called by for loop at the end of script.


Get megalist (getmegalist)
===================

Generates "megalist" 24 times (once for each subject).  

Get blocks (getblocks)
===================

Writes block information to "block" column.  For experimental sentences, the block index is used to keep similar sentence frames in separate blocks.

Randomize (randomize)
===================

Calls jumble and pseudorandomize.  Pseudorandomizes items with a block and randomizes the blocks themselves.

Write files (write files)
===================

Takes subject number and megalist, writes list of files for MATLAB and PsychToolbox to call.

Write blocks (writeblock)
===================

Takes megalist, block.

Calls ...

Generates lists of items with triggers included, and writes them into blocks according to the block index.  Also inserts plausibility question after each item.  

Add target sentences (addtarg)
===================


Add fillers (addfill)
===================

Interweave triggers (interweaveT
===================


Catalyst for loop 
===================






