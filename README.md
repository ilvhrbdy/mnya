## Mnya
Means "make nya", where "nya" is the filetype.  
Idk what the hell is this and why would you use it. I needed it for note taking with some scriptable features.  
Basically, it turned to be some sort of crippled bash preprocessor, yeah.  

So look, let's say you wrote this note:
```
MY DAYLY TODOS:

  [X] survive
  [ ] take a shower
```

and you want this chunk of text to be present in the next note,
in this case we can enclose text into `::` delimiters,
to tell `mnya` to include this chunk into the new note:
```
:: MY DAYLY TODOS:

  [X] survive
  [ ] take a shower
::
```
now, as it is your dayly tasks, you need to uncheck them for each new note,
we can declare a bash script that will do that:
```
@ uncheck_boxes: sed s/\[X\]/\[ \]/
```
where everything after `@` and until a newline or `::` is treated as command to `mnya`.
so you tell that you want to declare a label with the name `uncheck_boxes` with the provided bash script, after the `:`

now you want to run it on our chunk of text to modify it
```
@ uncheck_boxes
:: MY DAYLY TODOS:

  [X] survive
  [ ] take a shower
::
```

the text chunk that is enclosed into `::` piped into your script,
so we can process input as fallowed:
```
@@ process_text:
  while read line; do
    do_something
  done
@@
```
as you can see we have a multiline script within `@@` delimiters


labels can also be passed as arguments for other labels:
```
@ one: 1
@ two: 2
@ print: printf "$@"


```

