# Honeycomb

Honeycomb.io is product/service for analyzing software execution traces
in particular for complex interacting components. Basically during the
execution of something (like handling a request, or executing a query)
you annotate execution steps. Honeycomb offers an API endpoint to which
annotations are sent.

The honeycomb web interface offers graph and table visualization with
zooming and filtering options. This can give a lot of insight into
the  overall system behaviour.

An annotation has a name, a duration (how long it took to do), and
arbitrary parameters (e.g. HTTP request details, query memory use,
error codes). Any information relevant for debugging can be
included. When steps consist of substeps, this corresponds to
an annotation hierarchy.

There is a standard for such annotations, called opentracing.io.
It might become relevant in the future to generalize this library
to support more endpoint types.

# CL-Honeycomb

This module is a Lisp client for the Honeycomb API endpoint.
Lisp source forms are annotated. Then when the execution of the
outermost annotated step (corresponding to the root of the
annotation hierarchy) has finished execution, the event hierarchy
is scheduled to be sent to Honeycomb. This sending happens
asynchronous: there is a dedicate process for the HTTP traffic.

Here's a small self-contained example:

    (use-package :cl-honeycomb)
    (setf cl-honeycomb:*api-key* "...copy-from-honeycomb-account...")
    (cl-honeycomb:with-span ("component 1" "outer" :key-1 "val-1" :key-2 123)
      (sleep 1)
      (cl-honeycomb:with-span ("component 2" "inner" :count 27)
        (sleep 1))
      (sleep 1))

In the course of execution this will create two spans, in a parent-child
relation. After evaluating this form, the data is sent over to Honeycomb
and is available for inspecting in the web interface instantly.

# Interface
 * Compilation switch:
   - Variable `*include-honeycomb-code-p*`
 * Runtime switch:
   - Variable `*post-to-honeycomb-p*`
 * Configuration:
   - Variables `*global-api-key*`, `*local-api-key*`
   - Variables `*global-dataset-name*`, `*local-dataset-name*`
 * Annotations:
   - Macros `with-span`, `add-span-attributes`
 * Passing around annotation state:
   - Macros `with-saved-context`, `with-restored-context`
   - Macros `with-saved-serialized-context`, `with-restored-serialized-context`

# Variable `*include-honeycomb-code-p*`

 - If true (the default), the macros expand into forms that collect and
   transmit annotations.
 - If false, the `with-` macros act like `progn`.

This impacts the compilation (macro-expansion) of new code. Existing
compiled code is not impacted.

# Variable `*post-to-honeycomb-p*`

 - If true (the default), span data is sent via HTTP to Honeycomb.
 - If false, spans are created, but the last step of sending them
   over is not done.

# Variables `*global-api-key*`, `*local-api-key*`
Annotations are only created if an API key is set. Without an API key
there is no way to send the annotations over to the Honeycomb endpoint.

The API key is a string like `"5f6b36a9563199287790506206477390"`. It
can be found on the "Team Settings" page in Honeycomb. New API keys
(e.g. for specific projects or customers) can be created there too.

There are two variables to cover two use cases:

 * `*global-api-key*` is a *nonbindable* variable that has a global
   value. Its value can be modified, but not bound. Use this to enable
   annotations in all processes. Default: `nil`.

 * `*local-api-key*` is a dynamic variable that can be bound per process.
    Use this variable to control the annotations per process. E.g. for a
    database, the variable can be bound during handling of a single query.
    Default: `nil`.

If the *local* value is set, it is used; otherwise the *global* value is
used if it is set.

# Variables `*global-dataset-name*`, `*local-dataset-name*`
The *dataset* is the name under which all annotations are collected.
Its value is a string like `"production"`. There is no way in Honeycomb
to create datasets; instead it will appear there as soon as the first
span in it is created. Like for the API key:

* `*global-dataset-name*` is a *nonbindable* variable
* `*local-dataset-name*` is a dynamic variable;

# Macro `with-span`
Syntax:

    (with-span (component function &rest key-values)
       &body body)

This wraps *body* in the specified annotation.
Use nesting to indicate subtasks, as shown in the example above.

Both `component` and `function` must be strings. The component is
meant to identify where something takes place: a server, process,
or module identifier. The function can be a function name, or a
task description like `"collecting foo"` or `"waiting for results"`.

The `key-values` arguments are arbitrary. They could be parameters to
`function`. In the Honeycomb UI table view different keys will end up
different columns. In case of sending a lot of small information
pieces, it might be practical to combine them in one string, using a
generic key name, like: `:data "foo=1 bar=2"`. However if it's useful
to be able to group or filter based on a key, it is better to not
combine them.

In these examples the keys are always Lisp keywords, as it is handy
to have predefines keys. But the keys and values can be any object.
They are serialized to string at the start of the span, before *body*
is run.

There are some keys with special meaning, they are not passed on as
key-value attributes of the span:

 * `:max-child-spans` (a positive integer) limits the number of direct
   client spans that can be generated for `with-span` forms in the body.
   It does not limit indirect children;

 * `:flush-to-server-p` (a boolean) if true, ensures that this span
   and its children are sent to the Honeycomb server immediately after
   the evaluation of the body has finished. This is a way to get
   incremental data loaded in Honeycomb for longer-running processes.
   Usually the spans are sent to Honeycomb once the top span has
   finished executing.

# Macro `add-span-attributes`
Syntax:

    (add-span-attributes &rest key-values)

This adds the given key/value attributes to the enclosing span.
These two forms:

     (with-span ("c" "f" :key-1 "val-1")
       (foo)
       (add-span-attributes :key-2 (bar))
    
    (with-span ("c" "f" :key-1 "val-1" :key-2 (bar))
       (foo))

differ in whether `(bar)` is evaluted before or after `(foo)`.

This function can be used to add attributes to a span based on what
happens inside the span. For example if a span wraps a database query,
after fetching the query results you could add e.g. the number of
matches to the enclosing span:

    (with-span ("database" "query" :query "select ...")
      (let ((results (do-query ...)))
        (add-span-attributes :num-results (length results))
        (handle-results results)))

# Macros `with-saved-context`, `with-restored-context`
Syntax:

    (with-saved-context-context ()
      ...
      (with-restored-context ()
        ...))
A `with-span` form will automatically end up as child annotation of a
dynamically enclosing `with-span`. This is implemented using dynamic
variables. These variables are bound per thread. That means in a
multiprocessing context the parent-child relation would be missing.
In the Honeycomb interface the spans of different processes end
up in distinct trees.

To resolve this, use the macro `with-saved-context`
and lexicaly inside its body use `with-restored-context`.
For example:

    (defun handler (..)
      (with-saved-context ()
        ...
        (mp:process-run-function ..
          (lambda ()
         (with-restored-context ()
           (with-span (..)
             ..)))

# Macros `with-saved-serialized-context`, `with-restored-serialized-context`
Syntax:

    (with-saved-serialized-context (context-str)
      ...)
    
    (with-restored-serialized-context (context-str)
      ...)

This is a generalization of `with-saved-context` and `with-restored-context`
where the relevant span state is serialized to a string suitable for sending
to other processes in different Lisp images. For example the state could be
sent in an HTTP header.

Example:

    ;; process 1:
    (with-span (...) ;; span #1
      (with-saved-serialized-context (context-str)
        ...) ;; Send the string CONTEXT-STR to process 2
    
    ;; process 2:
    (let ((context-str ...)) ;; receive CONTEXT-STR from process 1
      (with-restored-serialized-context (context-str)
        (with-span (...) ;; # span #2, becomes a child of span #1
          ...)))
