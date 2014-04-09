# cayley

Cayley is a collection of functions I wrote to do some problems for
my "Modern Algebra" class in college.
Currently it can operate elements given a Cayley table, and tell if a subgroup is normal.
Built-in support for the symmetric groups coming soon.
Proving the correctness of the algorithms is left as an exercise for the
reader.
Also, the problems I used it for had very small sets, like D_4, so if you
try using cycle-to-set on a large set it just might blow up the stack.

## Usage

If you get an error about ISeqs, you probably need to have your elements in a set. (#{:my-elements}).
This library will probably not get used by anyone but myself, and only during this semester (Spring 2014), so if you do decide to
use it, just file an issue here on GitHub if you run into any problems.
## License

Copyright © 2014 Aaron Decker

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
