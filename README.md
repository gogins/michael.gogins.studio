# Michael Gogins Studio

Copyright (C) Michael Gogins<br>
All rights reserved<br>
http://michaelgogins.tumblr.com<br>
michael /dot/ gogins /at/ gmail /dot/ com

This is my private Git repository, intended to serve as my creative studio for computer music and other projects. All materials found herein are copyright by Michael Gogins, and are not open source or public domain, unless expressly stated otherwise.

## Methodology

My open source toolkit code and other resources for computer music, including some example pieces, may be found at https://github.com/gogins/gogins.github.io. In general, pieces that use Silencio from gogins.github.io should be created in a subdirectory of studio with a symbolic link to gogins.github.io/csound/silencio. Alternatively, just copy the silencio directory of gogins.github.io to the working directory.

To try to keep the size of this repository down, run this periodically:

```
git count-objects -v --human-readable
git reflog expire --all --expire=now
git gc --prune=now --aggressive
git count-objects -v --human-readable
```

Also, try to avoid versioning media files such as photographs; fix them up before committing them.

## Guiding Principles

Now, to head off my recurrent distraction from composing by taking on programming projects, I am renewing my focus on the following principles:

* I need to focus on composing both out of time and in time, and on visual music.

* Simple but deep algorithms are better than powerful but complex algorithms.

* For this reason I should renew my focus on parametric composition.

* I should more deeply investigate interpolations in parametric maps, and genetic algorithms, on the parameters of such systems.

* "Simpler but deep," with visual music, could mean sampling a visual animation to produce a score, as with _**Scrims**_, or transforming a sequence of notes into a visual abstraction (not an image of the score). The former is probably preferable, especially if the sampled points are visually indicated.


