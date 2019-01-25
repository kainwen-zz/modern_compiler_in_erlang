Modern Compiler in Erlang
=========================

I am perusing the book *Modern Compiler in ML* by Andrew W. Appel,
the book's website is http://www.cs.princeton.edu/~appel/modern/ml/.

At first I had decided to use Ocaml to implement the compiler. However,
after looking at ocamllex, I just cannot put up with its RE's style.

So finally, I decide to use Erlang again (why use again here:-).

OK, I think that by writing detailed type spec and with the help of dialyzer,
it should help a lot to avoid type related bugs.
