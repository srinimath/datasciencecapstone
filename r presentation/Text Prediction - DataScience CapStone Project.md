Text Prediction - DataScience CapStone Project
========================================================
author: Srinivasa Mathkur
date: Dec 27 2016
transition: linear
font-family: 'Calibri'

Introduction
========================================================
transition: fade

This presentation is done as a part of [_DataScience Capstone Project_](https://www.coursera.org/learn/data-science-project/home/welcome) in Coursera.
As a part of this course, I am exploring possibility of using various language algorithms
to predict words that occurs after a certain phrase. 
#### Data Exploration, Analysis and Cleaning
[_Coursera-SwiftKey_](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip) is the data provided to build our corpus. It looks like below:

```
     File LineCount WordCount FileSizeMB
1 twitter   2360148  30578891     159.36
2    blog    899288  37865888     200.42
3    news     77259   2665742     196.28
```

Below are the things I did as a part of this process:
- Cleanup special and non alphanumeric characters.
- Build corpus out of the cleaned data.
- Build 1-gram,2-gram,3-gram and 4-gram datasets.

I used quanteda package to build my ngram as I saw better performace with this over tm package and I found it easier to build datasets as I needed. I did not implement stemming as this was returning weird results ("doing" stemmed to "do"..) and I also retained combination of number with character (100s, 1000s etc) just to see if it helps better for prediction. 

Once I had completed running the alogrithms on these datasets, I filtered out all data with frequency equal to 1. This helped me reduce the size of the final dataset by a huge margin (4MB against inital 10MB).

Algorithms Explored
========================================================
transition: fade
I worked on implementing the following three solutions:

**_Kneser-ney Interpolation:_** [_Wiki_](https://en.wikipedia.org/wiki/Kneser%E2%80%93Ney_smoothing) for equation. The basic understanding with this alogrithm is to predict a word not just on the basis of its current context but all contexts that the predicted word appears in. Assuming a corpus contaning "New York" appearing frequently, the probability of maximum likelihood of next word for "I am new" would be "York", eve though "here" seems a more novel match.Here, Kneser-Ney algorithm interpolates this 4-gram results with probabilities  of all lower n-grams associated with this phrase(known as continuation probability), thus providing all contexts. So, if "here" is associated more with this and its interpolated lower gram models, then it will have a higher conitnuation probability than "York". Even though this involves complex calculations, this methodology works with smaller corpus and is more accurate than other models for smaller corpus. A modified version of this model is presented by [_Chad and Goodman_](https://people.eecs.berkeley.edu/~klein/cs294-5/chen_goodman.pdf) which seems to generate more accurate results.

**_Katz Backoff:_** Best definition at [_Wiki_](https://en.wikipedia.org/wiki/Katz's_back-off_model).In here, a leftover probability is calcuated for higher gram dataset for all unknown or missing words and then passed onto the lower gram data to discount with their conditional probability.Consider phrase "your car is" and we are trying to find probability P("dirty"|"your car is") and we dont have this phrase in our corpus. In such case, it is easier to fall back to the previous n-gram(3-gram here) and calculate P("dirty"|"car is"). Though this is a good option, it is important to discount some factor from higher gram probability and set aside for unknown words that can appear in future.Katz offered a solution to discount some of the probability from higher order grams and calculate left over probability (for "your car is"), which is then distributed to lower gram data (back-off). In word prediction, when there are very few words on higher gram, backing off to lower data helps us to provide more options for users to select from, and thanks to discounting and leftover probability, with a lower probability than the higher n-grams. In this case, I have used [_Good Turing Discounting_](http://www.seas.ucla.edu/spapl/weichu/htkbook/node214_mn.html) to discount from higher order n-grams.

**_Stupid Backoff:_** [_Stupid Backoff_](http://www.aclweb.org/anthology/D07-1090.pdf) is the least complicated of the three interms of understanding and coding and works best for corpus of large size.Here, we start with highest n-gram for comparison and back off to lower n-gram models while discounting the lower n-gram condition probability by a constant factor. Based on the suggestion in the paper, I have used a constant of 0.4. So, for one lower order ngram the probability is 0.4\*p, for two orders down its 0.4\*0.4\*3 and so on..

Application
========================================================
transition: fade

[_Here_](https://srinimath.shinyapps.io/textPredictionShiny/) is the application I have built for the purposes of this project. It consists of the following two parts:
- **_Text Prediction Models:_** This app allows you to enter a phrase in a text box and predicts next words based on the number of words you want to see (defaulted to 5) generating output for **kneser-Ney**, **Katz** and **Stupid Backoff**
- **_Next Word Prediction App:_** I built this as a short demo on how we can implement the output of these models. This app allows you to select a model, then type a phrase in the text box. Once you have entered in text box, you get five top words predicted by the model as "clickable" buttons below the text box. You can click on any of these to append the word to the phrase in the text box, which will result in model generating new set of words for the new text phrase. 

As an output for this, this app also shows you a bar plots of how the predicted words will match up against their unigram probability.

#### Note:
- I have pre-calculated the outputs for all the models and stored them in their individual n-gram models.
- All predictions are done against highest n-gram of **4**. All models back-off to lower n-grams if there is no match.
- The total size of all the data files uploaded is 4.8 MB.

References and Appendix
========================================================
transition: fade
- Text Prediction App: (https://srinimath.shinyapps.io/textPredictionShiny/)
- Github: (https://github.com/srinimath/datasciencecapstone)
- KneserNey: (http://smithamilli.com/blog/kneser-ney/)
- Good Turing Example: (http://rstudio-pubs-static.s3.amazonaws.com/165358_78fd356d6e124331bd66981c51f7ad7c.html)
- Katz Backoff Example: (https://thachtranerc.wordpress.com/2016/04/12/katzs-backoff-model-implementation-in-r/)
- Stupid Backoff: (http://www.aclweb.org/anthology/D07-1090.pdf)



<style>

/* slide titles */
.reveal h3 { 
  font-size: 50px;
  color: darkblue;
}

.reveal h4 { 
  font-size: 25px;
  font-weight: bold;
  color: black;
}
.reveal .slides section .slideContent p {
   font-size: 20px;
   color: black;
}
.reveal code.r{
  font-size: 20px;
}
.reveal pre {
  font-size: 10px
}
.reveal ul, 
.reveal ol {
    font-size: 20px;
    list-style-type: arrow;
}
</style>

