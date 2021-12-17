# Day 16

This solution is overly complicated, but this time not because I overdid it,
but because I wanted to try a certain library:
[FParsec](https://www.quanttec.com/fparsec/)
([this blog post](https://tyrrrz.me/blog/parsing-with-fparsec) is your friend)

Surprisingly, the parser worked perfectly on the first try.
The only thing that I had to change after running it for the first time with
the actual input, was that the value had to use int64 instead of int32.

As it turns out, however, turning a hex string into a binary string is a lot
harder…

