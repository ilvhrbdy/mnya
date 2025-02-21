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
@ uncheck_boxes: sed "s/\[X\]/\[ \]/"
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
    input=$(cat)
    printf %s "aboba1 $input aboba3"
:: 
```

as you can see we have a multiline script with `@@` delimiter,
until `@@` if the chunk contains only labels declaration
or until `::` if the command is calling a label


labels can also be passed as arguments for other labels:
```
@ one: 1
@ two: 2

-- I don't use 'echo', cause it adds a newline --
@ print: a="$@"; printf " $a "

@ print one two ::::
```
which will result into this:
```
...all the commands are preserved by default, obviously

@ print one two :: 1 2 ::
```

there is also a way to call a script without declaration, using `@:` or `@@:` for multiline script:
```
@: printf " hello " :: ::
```
will result into this:
```
@: printf " hello " :: hello ::
```

oh, yeah, in the `.nya` files you must have the index command `@# <index>`,
where `index` is an integer that will be used to sort file in working directory.  

`NYA_DIRECTORY` env is used to set a working directory for `mnya`, there it will evaluate commands
in the last file and create the new one with the given name

you just need to set an index for first file in directory and forget about it
```
@# 1
```

spaces doesn't really matters in commands:
```
@a:1
@ b : 2
@echo: echo $@

@@echo
a
b:: ::

@@x:
input=$(cat)
# doing something with input...
@@



^
this newlines are preserved in new file

^
after first non-command-chunk non-text-chunk non-whitespace
everything will be discarded


:: static text that will be preserved ::
```


bla bla lba i wanna sleep..

