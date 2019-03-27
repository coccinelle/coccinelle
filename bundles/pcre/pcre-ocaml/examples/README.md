## Examples

### cloc

This program reads C-sources from stdin and prints them to stdout with
comments and empty lines removed.  Useful for counting LOCs.

### count_hash

This program reads text from stdin, counts all equal words that are separated by
whitespace and prints the result to stdout.

### pcregrep

A grep-like program using Perl-compatible regular expressions.  Start the
program with argument `-help` to see what it does!

### subst

Substitutes text in files using Perl-compatible regular expressions and
substitution patterns.  Start the program with argument `-help` to see what it
does!

Example invocation:

```sh
subst '([Tt])ermcap' '$1ermCap' < /etc/termcap
```
