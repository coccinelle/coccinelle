SGEN SOURCE CODE
================

Dependencies
------------

**Parser dependencies (coccinelle/parsing_cocci/)**:

ast\_cocci.ml ast0\_cocci.ml visitor\_ast0.ml visitor\_ast0\_types.ml type\_cocci.ml

**Internal dependency order (partial; ie. not total, some can be interchanged)**:

globals.ml ast\_tostring.ml detect_patch.ml
meta\_variable.ml snapshot.ml user\_input.ml
position\_generator.ml disj\_generator.ml
rule\_body.ml rule\_header.ml context\_rule.ml script\_rule.ml
file\_transform.ml sgen\_interactive.ml sgen\_config.ml sgen.ml
main.ml


Workflow
--------
**main.ml contains the entry point. It calls the driver for the program which is in sgen.ml**:

 1. Parse the SmPL script by calling the Coccinelle parser. Output is the generated abstract syntax trees (AST0), a list of rulenames in the script, and a list of virtual modes in the script.

 2. Check validity of virtual modes and rulenames (globals.ml).

 3. Filter out rules to be generated, ie. any rule containing \*/+/- rules, and generate disjunction maps for each of them (detect\_patch.ml).

 4. Get user-specified metadata either through the commandline or config file (sgen\_interactive.ml, sgen\_config.ml, user\_input.ml).

 5. Generate context rules from the \*/+/- rules (context\_rule.ml). Context rules are the rules supported by *context* mode, ie. rules with \*'s and inserted metapositions.

 6. Generate script rules for each context rule (script\_rule.ml). Script rules are supported by the virtual modes *org* and *report*, ie. printing modes.

 7. Output the new generated script. First by modifying the original script to conform to new rulenames, virtuals, and dependencies as well as add preface ie. description, comments, etc. (file\_transform.ml). Then output the generated context and script rules.

**context\_rule.ml contains the main context rule generation**:

 1. Extract all metavariables from an AST0 (meta\_variable.ml).
 2. Generate the body of the new rule (rule\_body.ml).

     - traverse AST0 and "reparse" original patch/context rule to new context rule using the AST0 visitor module (parsing\_cocci/visitor\_ast0.ml).

     - use snapshot type to keep state during the traversal (snapshot.ml).

     - generate metapositions at structurally suitable places in the rule, used for *org* and *report* printing modes (position\_generator.ml).
       If the original rule was a plus rule (ie. no \*/-, only +), the positions dictate where the new \*'s are placed. Otherwise, the original \*/- dictate where the new \*'s are placed.

     - special disjunction cases that arise in converting a *patch* rule to a *context* rule are handled as well (disj\_generator.ml).

 3. Generate the header of the new rule with inserted dependencies and metapositions (rule\_header.ml).


Constraints
-----------
There are a number of constraints that the input Coccinelle script has to satisfy:

 - New rulenames cannot be "rule&lt;number&gt;", where &lt;number&gt; is any number, since this naming scheme is used by sgen to generate names for unnamed rules.
 - New rulenames as specified in config/interactive cannot contain spaces or start with numbers (as this is not allowed in Coccinelle; in fact sgen is slightly less strict than Coccinelle in terms of rulenames).
 - New rulenames cannot be any of the keywords used in sgen, ie. "description", "confidence", "author", etc.
 - Cannot contain virtual modes "patch", "context", "org", or "report".
 - etc.
