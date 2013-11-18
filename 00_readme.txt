Hey guys,
I'm working on the color plot she suggested on Thursday, using the knn predictions.
is knn2004 your prediction for the 2012 election based on 2004 data?

it's just a factor vector of "Rep" or "Dem" and it's not paired
with any county/state information.  but I need to know what counties you
predicted for so I can test your predictions against the results.


also SEAN I ran the rpart code from the the FinalProject.R file;
it creates two identical trees side-by-side. is that right?
and it looks like the rpart code predicts one overall winner at the end; 
the "predictions2004" attribute in FinalDF are your predictions
for each county, right

SEAN: I've made it only print one tree, I was just testing with two. Yes, the final prediction was just me gauging the accuracy (it sucks!) 

I've commented out superfluous lines of code, will probably remove before final submit.



[Teresa] - Actually Sean rpart did a pretty good job -- see the 
"rpartPredMap" which tests the predictions.

[Sean] - suppose you are right! The graph looks very pretty :)

[Sean] - Teresa, please open the FinalProject-SeanTemp file and run it... I'm getting completely different results for the map then you are. Also note that it will save in your ~ directory as rpartPredMap.pdf

[Teresa] - I diff'd the files; looks like a few vector names were changed but my code didn't depend on those.  I was also working with a saved version of the FinalDF data frame to make my plot, so it could have been an outdated version (I just hate re-sourcing the whole file each time because it takes FOREVER).

aaaaaa guys you both added variables to the data frame called Winner2004/Winner2012.  One is lowercase and one is not.  But winner2012 and Winner2012 are exact opposites!! can y'all please double-check to make sure they're right!?


[Arif]-both Sean and I are well aware, winner with a lowercase, was used to certify who won the election based on the the number of of actual votes. I was going to change it, but then inn crashed so I had to rewrite some of the code. Sorry!

[Teresa] - k turns out Sean's Winner2012 is the opposite of the winner..
check with FinalDF[100:110, c("obama.votes", "romney.votes", "winner2012", "Winner2012")] it shows Dem winning when Romney got more votes than Romney.

okay I finally figured this out.  The knn plot was off because you guys ommitted na's and it made the length of your prediction vector different from the length of winner2012.  so I only generated comparisons between counties that you actually predicted for.  Adding the code to FinalProject.R now.

NEED:
- please send me 2-3 sentences explaining where your predictions were off.  I'll add them to the caption.
[[Arif]]- In both cases the populations were 
