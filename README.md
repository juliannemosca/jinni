# jinni

Just like every Jedi has to make her own lightsaber, every LISPer has to make their own LISP.

![Alt text](https://user-images.githubusercontent.com/19293817/76164826-3ef9eb00-6152-11ea-8c58-58cf8be290e2.png "wookie")

(image originally from: https://xkcd.com/297/)

Well, I'm happy I don't have to defend myself from stormtroopers with this one, or I'd be bantha fodder faster than you can say "wookie"!

**tl;dr:** This is an experimental LISP. Play around with it, but do not use it for anything too serious, like, better don't load it on your lawnmower or anything like that ;-P


### Description:

**jinni** is a LISP interpreter, loosely based on the language described in the LISP 1.5 Manual`[1]` from 1962. So, what does this mean? Perhaps the most important things to remark here are that: #1- This is totally not CL. #2- It has FEXPRs instead of macros, since macros didn't exist at the dawn of LISPs. #3- it implements some of the old crazy stuff like `LABEL` for defining symbols, and `F` as an alias of `NIL` and so on... and I guess there's some other important aspects, but if you know LISP 1.5, you kind of know what to expect, and if you don't it'd be fun to discover!


### The longer story and why does **jinni** even exist?

_(this is a long personal anecdote, so if you're only interested in the technical parts you can skip right ahead to the next section)._

As I finished reading through the LISP 1.5 Manual, my starry-eyed younger self thought it would be cool to implement the five LISP elementary functions: `cons`, `car`, `cdr`, `eq` and `atom`. And since I was there, I thought it'd be nice to also give it a shot to implement `apply` and `eval` as shown on page 13. After all, having read Alan Kay repeatedly praising this code many times, I thought it'd be worth to actually try to implement it rather than just feeling content with having read it once while lying comfortably on the armchair. And also it shouldn't be much of a stretch, right? I just needed a couple more functions... but, the real `apply` and `eval` are actually described in detail later on, on page 70 in the Appendix B section. Reading it again, but this time thinking on how I should go about implement it, I realized it was already a bit more complicated than I had anticipated.

I spent some time hacking on it, at the beginning trying to keep **jinni** close to the manual, implementing the property lists for atoms and related functions. I didn't set up proper debugging mechanisms and also I didn't write clean code from the start. Both things I know the importance of, but this was a personal project so it didn't matter I thought (oh I was SO wrong). Then things were getting nightmarish. I recoiled, but still wanted to see how far I could get. That's when I decided to let the implementation take a separate path in some aspects from the manual, so I got more creative with it, so to say, while still trying to keep the original flavor.

The fact that I worked on this project on and off for about two years, always in relatively constrained time slots, didn't really help when trying to figure out what was the last darn thing that broke and how I managed to fix it. So everything is pretty hacky in the code.

After I got FEXPRs to kind of work, and did some workarounds for memory management that would at least let me sleep at night, I decided to wrap it. Any further work on this would be a waste of time, as at this point, if I wanted to do anything more serious with it I'd have to re-write it from scratch (again, actually, since I did re-write it. Two times. So that was it.).

Finally, I'm completely sure there's still plenty of bugs lying around in the code (in fact I'm already finding one while writing this README), but those won't be fixed, since as I said before, the purpose of this program is completely fulfilled. In fact I'd even say, given the initial premise of implementing the five elemental functions and maybe a parser to play around with them, this was already waaaay too carried away... but what a fun journey has it been!

Now that you know the story, you can safely proceed to the sections below for some more technical details.


### Ok cool, so what can be done with it:

I think this is better seen in action, so here's a session that I think demonstrates some of the important features:

```
* ( LABEL "IF" ( FLAMBDA ( FORM ) ( COND ( ( EVAL ( CAR FORM ) ) ( EVAL ( CAR ( CDR FORM ) ) ) ) ( T ( EVAL ( CAR ( CDR ( CDR FORM ) ) ) ) ) ) ) )

IF
* ( LABEL "FIB" ( LAMBDA ( N ) ( IF ( ( < N 2 ) N ( + ( FIB ( - N 1 ) ) ( FIB ( - N 2 ) ) ) ) ) ) )

FIB
* (FIB 21)

10946
* 
```

Note the use of FLAMBDA. This is the keyword used in **jinni** for defining FEXPRs, which are the mechanism that **jinni** provides for special-forms.

Those are kind of a big topic in itself`[2]`, but the overly-simplified version here is that FLAMBDA is like LAMBDA, but it implicitly quotes all its arguments when being defined, meaning all arguments are not evaluated when the FEXPR is evaluated. And as it can be seen in the above example, this is very convenient, for example, for defining an "if" expression.


### What about the scoping?

Since old LISPs had dynamic scope rather than lexical scope, I wanted **jinni** to have dynamic scope too. In a nutshell, consider the following **jinni** session:

```
* ( LABEL "FUN1" ( LAMBDA () ( A ) ) )

FUN1
* ( LABEL "FUN2" ( LAMBDA ( A ) ( FUN1 ) ) )

FUN2
* ( LABEL "A" 10 )

A
* ( FUN1 )

10
* ( FUN2 20 )

20
* ( FUN2 )

NIL
* 
```

Observe that when calling FUN2 with the arg 20, and then evaluating FUN1 inside FUN2, the value of A, even though it is not passed as an argument to FUN1, still "spills" over the evaluation of it, since it is the most recent binding of the symbol A, so that's why in this case FUN1 when evaluated inside FUN2 results in 20 and not 10.

Also observe that FUN2 without arguments evaluates to NIL: because at runtime, the A arg of FUN2 is bound to NIL since no argument was provided, and so that is the most recent value bound to A at the time FUN1 is called inside FUN2.


### A note on memory management in **jinni**

When writing **jinni**, I didn't think about memory usage or garbage collection until it was too late, and so it became very complicated to deal with it. But since I didn't want to just leave it in such a mess, I tried to find a solution that, at least with some compromises, could still allow it to operate in a more or less tidy fashion.

So I set up a little vm that works this way: there's a fixed number of memory segments, each with a fixed size. When `jinni_malloc` is called, it takes memory from one of these segments. Each segment corresponds to a stack frame, so basically each time the stack is unwound, the corresponding memory segment is completely freed.

Note also that `jinni_free` is left unimplemented in fact, since individual memory blocks inside a segment are currently never freed, only the whole segment.

This means that if you redefine a symbol too many times inside the same stack frame you'll blow up the memory segment designated for it, since as stated before, there is no garbage collection.

This is embarrasingly sub-optimal, I know, and on top of it it's still buggy, but... yeah I guess I can live with it.


### References:

`[1]`: https://mitpress.mit.edu/books/lisp-15-programmers-manual  
`[2]`: https://www.nhplace.com/kent/Papers/Special-Forms.html
