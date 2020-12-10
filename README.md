# Dominions-parser

This is an attempt to build a tool to parse save files (both `.trn` and `.2h`)
for the strategy game [Dominions 5](https://www.illwinter.com/dom5/index.html),
in hopes of providing new methods of game analysis (and mod debugging).

It is currently very much in-progress. Much of the structure currently present
is based on
[dominions4.gamedata](https://github.com/blitzserver/dominions4.gamedata),
which is a fork of a similar project that started on Dominions 4 or possibly 3,
and which appears to be based on a decompilation and/or ptrace (judging by how
it parses specific sequences of values of known size and ordering but unknown
meaning).

My current plans are:

1. Finish translating what was done in the above Go project from Go to Common
	 Lisp.
2. Test it on reading current (Dominions 5) `.trn` and `.2h` save files, revise
	 until it works.
3. Investigate to determine what all the unknown values actually do.
4. Build tools for collecting data across multiple turns/players/games for
	 analysis, and for presenting that data in a more user-friendly way than the
	 Lisp REPL.
