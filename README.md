allrgb: experiments with color
==============================

Some lisp code for making images with a 1-1 correspondence between pixels and rgb colors (suitably quantized). Right now I only have one method implemented, the winning entry from [this codegolf challenge](https://codegolf.stackexchange.com/questions/22144/images-with-all-colors), which, given enough patiences, spits out stuff like:

![woah dude](https://raw.githubusercontent.com/kilimanjaro/allrgb/master/woah.png)



#### Todo

The current code is inefficient in a number of ways. A few candidate improvements:
+ lol don't use lists for sets
+ more careful data layout and memory use

#### Related ideas

[Tweetable mathematical art](https://codegolf.stackexchange.com/questions/35569/tweetable-mathematical-art)
