## Mnya
Means *"make nya"*, where *"nya"* is the file suffix, which is the sound that anime girls do.

Idk what the hell this is and why you would use it. I needed it for note-taking with some scriptable features.
Basically, it turned to be some sort of crippled shell script preprocessor+executor, yeah.  

To create notes with `mnya`, set `NYA_DIRECTORY` to the folder where your `.nya` notes will be stored.
Then run it, providing a name for the new note:
```
mnya I got divorsed
```
`mnya` will concatenate arguments, replacing spaces with `_`.
if the folder doesn't contain any `.nya` file it will initialize it for you;
otherwise it will evaluate the last file and insert result into the new note, outputting path to it.

All syntax errors and warnings will be written to `stderr`,
while the new note's path will be written to `stdout` on success.

### What does "evaluate" means?

So look, let's say you wrote this note:
```
MY DAILY TODOS:

  [X] survive
  [ ] take a shower
```

And you want this chunk of text to be present in the next note,
in this case we can enclose text into `::` delimiters,
to tell `mnya` to include this chunk into the new note:
```
:: MY DAYLY TODOS:

  [X] survive
  [ ] take a shower
::
```
Now, as it's your daily tasks, you need to uncheck them for each new note,
we can define a shell script that will do that:
```
@ uncheck_boxes: sed "s/\[X\]/\[ \]/"
```
Everything after `@` and until a newline or `::` is treated as command to `mnya`.
So you tell that you want to define a label called `uncheck_boxes` with the provided shell script, after the `:`.

Now you can run the labeled script on a chunk of text like this:
```
@ uncheck_boxes
:: MY DAYLY TODOS:

  [X] survive
  [ ] take a shower
::
```

Or you can even even chain scripts:
```
@ uncheck_boxes
@ do_other_stuf
@ and_so_on
:: input text ::
```
Which will act the same way as in shell script here:
``` bash
echo " input text " | uncheck_boxes | do_other_stuf | and_so_on
```

You can also use labels as arguments when calling a labeled script,
they are not going to be evaluated, but passed as a strings:
```
@ print_hello: printf " hello "
@ eval: eval $1

you can access name of the label by reading $0 variable
but i don't know why would you want to do that
in this case $0 = "eval"
@ eval print_hello :: ::

```
Which will result in:
```
...all the commands are preserved by default, obviously

@ eval print_hello :: hello ::
```

There is also a way to evaluate a script without defining a label, using `@:` syntax:
```
@: printf %s " $(date +%F) " :: ::
```
which will result in:
```
@: printf %s " $(date +%F) " :: 2025-01-08 ::
```

you can define or call a label via multiline syntax using `@@` instead of `@`:
```
@ hello: printf " hello "
@@ hello_mul:
    printf " hello "
@@

@ hello :: ::
@@
  hello
:: ::

@: printf " hello "
:: ::

@@:
  printf " hello "
  printf " sailor "
:: ::

```


In the `.nya` files you must have the index command `@# <index>`,
where `index` is an integer that will be used to sort file in the working directory:
```
@# 1
```
You can define index once and forget it; `mnya` will update it for each new note.

### Other stuff...

Spaces between tokens doesn't matter:
```
@ hi : printf "oh hi mark" :: ::
@hi:printf "oh hi mark":: ::

@@x:
input=$(cat)
# doing something with input...
@@
```

Playing with piped text via `read` is a bit tricky and annoying,
so I prefer to use `cat` to capture input text:
```
@@ process_text:
    input=$(cat)
    printf %s "aboba1 $input aboba3"
@@ 
```

You can do whatever you would do in command line.
You can call everything that in your `PATH`
```
@: dying ::

        ( ͡° ͜ʖ ͡°)

Hello, little reminder
    time left:
        in years  - 54
        in months  - 651
        in weeks  - 2606
        in days   - 18248
::
```
Or specify the full path to the executable:
```
@: ~/.local/bin/dying :: ... ::
```

Whitespaces between chunks can also be preserved:
```
@ some command call
:: text ::


^
these newlines are preserved in new file

^
after the first non-command-chunk, non-text-chunk, non-whitespace,
everything will be discarded
```
