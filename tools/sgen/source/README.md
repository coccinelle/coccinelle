SGEN SOURCE CODE
================

Dependencies
------------

This is a partial (ie. not total, some can be interchanged) order of dependencies:  
globals.ml ast\_tostring.ml detect_patch.ml  
meta\_variable.ml generator\_types.ml position\_generator.ml disj\_generator.ml  
rule\_body.ml rule\_header.ml context\_rule.ml script\_rule.ml  
user\_input.ml file\_transform.ml sgen\_interactive.ml sgen\_config.ml  


Workflow
--------
main.ml contains the entry point:

 1. uses the Coccinelle parser to get parsed output, in particular the generated abstract syntax trees (AST0), a list of rulenames in the script, and a list of virtual modes in the script.
 2. uses globals.ml to check validity of virtual modes and rule names.
 3. uses detect\_patch.ml to filter out the rules to be generated, ie. any rule containing */+/- in the body. Also generates disjunction maps for each rule to be used in context rule generation.
 4. uses sgen\_interactive.ml, sgen\_config.ml, user\_input.ml to get user configuration; either through the commandline or a config file.
 5. uses context\_rule.ml to generate context rules from the list of rules output by detect\_patch. Context rules are the rules supported by *context* mode, ie. rules with \*'s and inserted metapositions.
 6. uses script\_rule.ml to generate the corresponding script rules for each context rule. These rules are supported by the virtual modes *org* and *report*.
 7. uses file\_transform.ml to modify the original script to conform with new rulenames, virtuals, and dependencies, as well as add user input header (description, author, comments, etc.). Output the generated context and script rules.

context\_rule.ml contains the main rule generation:

 1. uses meta\_variable.ml to extract all metavariables from a rule represented as AST0.
 2. uses rule\_body.ml to generate the body of the new context rule.
     - uses the AST0 visitor module (in parsing\_cocci directory) to traverse AST0 and "reparse" original patch/context rule to new context rule.
     - uses the types defined in generator\_types.ml to keep state during the traversal.
     - uses position\_generator.ml to generate metapositions at structurally suitable places in the rule (used for *org* and *report* printing modes). If the original rule was a plus rule (ie. no */-, only +), the positions dictate where the new \*'s are placed. Otherwise, the original */- dictate where the new \*'s are placed.
     - uses disjunction\_generator.ml to handle special disjunction cases that arise in converting a *patch* rule to a *context* rule.
 3. uses rule\_header.ml to generate a new rule header with the correct dependencies and metavariables.


Constraints
-----------
There are a number of constraints that the input Coccinelle script has to satisfy:

 - New rulenames cannot be "rule&lt;number&gt;", where &lt;number&gt; is any number, since this naming scheme is used by sgen to generate names for unnamed rules.
 - New rulenames as specified in config/interactive cannot contain spaces or start with numbers (as this is not allowed in Coccinelle; in fact sgen is slightly less strict than Coccinelle in terms of rulenames).
 - New rulenames cannot be any of the keywords used in sgen, ie. "description", "confidence", "author", etc.
 - Cannot contain virtual modes "patch", "context", "org", or "report".
 - etc.
