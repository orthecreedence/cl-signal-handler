cl-signal-handler
=================

This is a small library that wraps around CFFI to provide the ability to handle
unix signals in your lisp app.

It seems to work on just about any lisp that loads CFFI and is running on \*nix:
SBCL, CCL, clisp have been tested, probably more work.

Usage
-----
Replace the current signal handler for SIGINT (2) and run the body of the 
handler for each signal processed.

    (set-signal-handler 2
	  (format t "Caught SIGINT! LOL~%"))

Execute a block of code with the specific signal handler in place, and revert
back to the original handler after executing:

    (with-signal-handler 2 (lamda () (format t "Sorry, SIGINT not allowed while I'm counting!~%"))
	  (dotimes (i 10)
	    (format t "I'm counting: ~a~%" i)
		(sleep 1)))

If you try `CTRL+c` while the above is executing, it should tell you "Sorry,
SIGINT not ..." *but* once the loop is done, pressing `CTRL+c` will open whatever
built-in debugger your implementation has.

Author's note/warning
---------------------
I'm not a C expert, not by any stretch, and unfortunately cannot as of right now
give advice for best practice uses of this library. By using this library, you
are leaving the safety of lisp and entering a world of unknowns (unless you are
a C expert ;]) and must tread carefully. Please read
[this guide on signal handling in C](http://www.gnu.org/software/libc/manual/html_node/Defining-Handlers.html#Defining-Handlers)
before using.

Chances are when things break for me when using this library, I'll update it to
make it easier to play nicely with C land. Until then, use at your own risk.
