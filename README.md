# WHHIP
WHHIP: Walks + Hard-Hits Per 9 IP

Motivation: I personally never found much value in the WHIP stat (Walks + Hits Per Innings Pitched). 
I never understood what value of WHIP was good and whihc was bad until I fell in love with baseball again in high school.

As an introduction to the pybaseball package and Statcast data, I thought I could quickly make a crude metric that would satisfy my curiosity of 
measuring WHIP better. This is where WHHIP was born, and it hinges on the simple and now conventional premise that not all hits are created equal.
The type of hit that is worth the most in the current era of baseball is the "hard-hit" ball, or balls hit off the bat at 95.0 mph or higher. 
This crude metric intends to only factor in hard-hit balls in play rather than all hits in play.

Notes from 6/15/22: I originally intended the formula for WHHIP to include all hard hits that were scored as hits. 
After checking stats on Fangraphs, all hard-hit balls hit into play are classified as hard-hits, regardless of how they are scored.
Therefore, I corrected this to include the number of hard-hits according to Fangraphs into my formula.

I also corrected a small error 6/15/22 that left out balls hit at 95.0 mph exactly, by simplying changing to a >= sign rather than just >.
