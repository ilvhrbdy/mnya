use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
    fs::File,
    hash,
    io::{Read, Write as IoWrite},
    iter::Peekable,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

const ENV_INPUT_DIRECTORY: &str = "NYA_DIRECTORY";

const USAGE: &str = "Usage:
    mnya [FLAGS] <OUTPUT FILE NAME>*

Flags:
    -h                  - print this message
    -print              - write to STDOUT instead of a file
    -i <INPUT FILE>     - evaluate input file instead of reading NYA_DIRECTORY;
                          PWD will be used to create output file,
                          if neither `NYA_DIRECTORY` nor `-print` is provided

Env:
    NYA_DIRECTORY      - default nya folder; act as working directory

<OUTPUT FILE NAME> could be built from multiple arguments that do not start with `-`
After successful evaluation, the path to the output file will be printed to STDOUT.
All of the [Err] and [Warn] will be printed to STDERR.";

fn trim_in_place(s: &mut String) {
    if let Some(end_idx) = s.rfind(|c: char| !c.is_whitespace()) {
        s.truncate(end_idx + 1);
    } else {
        s.clear();
        return;
    }

    if let Some(start_idx) = s.find(|c: char| !c.is_whitespace()) {
        let _ = s.drain(..start_idx);
    }
}

fn to_string(e: impl Display) -> String {
    e.to_string()
}

#[derive(Debug)]
pub enum ErrorKind {
    NoIndexChunk,
    NonIntegerIndexValue,
    UnclosedDelimiter {
        delimiter: &'static str,
        opening_line: usize,
        opening_position: usize,
    },
    UnallowedLabelCharacter(char),
    UndeclaredLabel(String),
    LabelRedeclaration(Label), // contains second declaration
    EvaluatingScript(String),
    MissingInputTextBlock,
}

#[derive(Debug)]
pub struct Error<'src_lt> {
    source: &'src_lt str,
    kind: ErrorKind,
    line: usize,
    position: usize,
}

impl Display for Error<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        fn point_on_err(
            f: &mut Formatter,
            err_line: usize,
            err_position: usize,
            text: &str,
        ) -> fmt::Result {
            let display_lines_range = err_line.saturating_sub(4)..err_line;
            let err_cursor_pad = err_position.saturating_sub(1);
            let err_cursor_pad_char = if err_cursor_pad == 0 { "" } else { " " };

            let couple_of_lines = text
                .lines()
                .enumerate()
                .filter(|(n, _)| display_lines_range.contains(n));

            for (n, line) in couple_of_lines {
                let line_num = n + 1;
                writeln!(f, "{line_num:<4}|   {line:<4}")?;
            }

            writeln!(
                f,
                "{spc:<4}    {err_cursor_pad_char:err_cursor_pad$}^",
                spc = " "
            )
        }

        let err_line = self.line;
        let err_position = self.position;

        write!(f, "{err_line}:{err_position}: ")?;

        match &self.kind {
            ErrorKind::NoIndexChunk => {
                writeln!(f, "first command must be file index chunk: `@# <index>`")?
            }
            ErrorKind::NonIntegerIndexValue => {
                writeln!(f, "value for index of file must be an integer")?
            }
            ErrorKind::UndeclaredLabel(label_name) => {
                writeln!(f, "trying to access undeclared label `{label_name}`")?
            }
            ErrorKind::MissingInputTextBlock => {
                writeln!(f, "expected `TextBlock` as input for commands:")?
            }
            ErrorKind::LabelRedeclaration(second) => {
                let name = &second.name;

                writeln!(f, "label `{name}` already exist:")?;
                point_on_err(f, err_line, err_position, self.source)?;

                writeln!(
                    f,
                    "---> previous declaration at {}:{} here:",
                    second.line, second.position
                )?;
                point_on_err(f, second.line, second.position, self.source)?;

                return Ok(());
            }
            ErrorKind::UnallowedLabelCharacter(ch) => {
                writeln!(f, "unallawed label character `{ch}`, bruh:")?
            }
            ErrorKind::EvaluatingScript(err) => {
                writeln!(f, "script error: {err}")?; // TODO: proper formatting
            }
            ErrorKind::UnclosedDelimiter {
                delimiter,
                opening_line,
                opening_position,
            } => {
                let delim_line = opening_line;
                let delim_position = opening_position - delimiter.len();

                writeln!(
                    f,
                    "unclosed chunk delimiter `{delimiter}`\n---> starts at {delim_line}:{delim_position} here:"
                )?;
                point_on_err(f, *delim_line, delim_position, self.source)?;

                writeln!(
                    f,
                    "---> and suppose to end at {err_line}:{err_position} here:"
                )?;
                point_on_err(f, err_line, err_position, self.source)?;

                return Ok(());
            }
        }

        point_on_err(f, err_line, err_position, self.source)
    }
}

impl std::error::Error for Error<'_> {}

#[derive(Debug)]
enum Input {
    Dir(PathBuf),
    File(PathBuf),
}

#[derive(Debug)]
enum Output {
    Stdout,
    File(PathBuf),
}

fn parse_args(mut args: impl Iterator<Item = String>) -> Result<(Input, Output), String> {
    let mut mb_input = None::<Input>;
    let mut mb_output = None::<Output>;
    let working_dir = std::env::var(ENV_INPUT_DIRECTORY).ok();
    let get_working_dir_path = || working_dir.as_ref().map(PathBuf::from);

    loop {
        let Some(mut arg) = args.next() else {
            break;
        };

        match arg.as_str() {
            "-h" => return Err(USAGE.into()),
            "-i" => match args.next() {
                Some(file_name) => mb_input = Some(Input::File(PathBuf::from(file_name))),
                None => return Err("[Err] input file for `-i` flag where?".into()),
            },
            "-print" => mb_output = Some(Output::Stdout),
            a if a.starts_with("-") => {
                return Err("[Err] i don't know what this flag means..".into());
            }
            _ if !matches!(mb_output, Some(Output::Stdout)) => {
                arg.push('_');
                for a in args {
                    arg.push_str(&a);
                    arg.push('_');
                }
                let _ = arg.pop(); // remove trailing '_'

                let mut path = get_working_dir_path()
                    .map_or_else(|| std::env::current_dir().map_err(to_string), Ok)?;

                path.push(arg);
                mb_output = Some(Output::File(path));

                break;
            }
            _ => (),
        }
    }

    let input = mb_input
        .or_else(|| get_working_dir_path().map(Input::Dir))
        .ok_or_else(||
            format!("[Err] either provide an input note file with `-i` flag or set `{ENV_INPUT_DIRECTORY}`, where notes are stored")
        )?;

    let output = mb_output.ok_or("[Err] throw some words for new note file name!")?;

    Ok((input, output))
}

#[derive(Debug, Copy, Clone, Default, Eq, PartialEq)]
pub enum ChunkKind {
    // @# ...
    Index,
    // @ ...
    OneLineCmd,
    // @@ ... @@
    // closed_by_self = @@ ... @@
    // !closed_by_self = @@ .. :: or EOF
    CmdBlock {
        closed_by_self: bool,
    },
    // :: ... ::
    TextBlock,
    // to preserve whitespaces between commands
    // but it will save only first whitespaces before the DiscardedText
    //
    // so this shit will capture whitespaces:
    // @a: 1
    //           < spaces
    // some text < discarded
    // @b: 2
    //
    // and this one will not
    // @a: 1
    // some text < discarded
    //           < discarded
    // @b: 2
    #[default]
    Whitespaces,
    DiscardedText,
}

impl ChunkKind {
    pub fn delimiters(&self) -> (&'static str, &'static str) {
        match self {
            ChunkKind::Index => ("@#", ""),
            ChunkKind::OneLineCmd => ("@", ""),
            ChunkKind::CmdBlock {
                closed_by_self: true,
            } => ("@@", "@@"),
            ChunkKind::CmdBlock {
                closed_by_self: false,
            } => ("@@", ""),
            ChunkKind::TextBlock => ("::", "::"),
            _ => ("", ""),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub kind: ChunkKind,
    pub text: String,
    pub line: usize,
    pub position: usize,
}

impl Default for Chunk {
    fn default() -> Self {
        Self {
            line: 1,
            position: 1,
            kind: ChunkKind::default(),
            text: String::default(),
        }
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let (open_del, close_del) = self.kind.delimiters();
        let text = &self.text;

        write!(f, "{open_del}{text}{close_del}")
    }
}

#[derive(Debug, Clone, Default)]
pub struct Label {
    pub name: String,
    pub line: usize,
    pub position: usize,
}

impl hash::Hash for Label {
    fn hash<H>(&self, h: &mut H)
    where
        H: hash::Hasher,
    {
        self.name.hash(h)
    }
}

impl std::cmp::PartialEq for Label {
    fn eq(&self, other: &Label) -> bool {
        self.name.eq(&other.name)
    }
}

impl std::cmp::Eq for Label {}

#[derive(Debug)]
pub struct Script {
    pub text: String,
    pub line: usize,
    pub position: usize,
}

#[derive(Debug)]
pub enum Cmd {
    // @: script :: text ::
    // or
    // @@: script :: tet ::
    AnonEval(Script),
    // @ label1 label2 .. :: text ::
    // or
    // @@ label1 label2 .. :: text ::
    Eval(Vec<Label>),
    // @ label: script
    // or
    // @@ label: script @@
    Decl { label: Label, script: Script },
}

pub fn collect_chunks_from_stream<'src_lt>(
    stream: &mut Peekable<impl Iterator<Item = char>>,
    n_chunks: usize,
    chunks_buf: &mut Vec<Chunk>,
    source: &'src_lt str, // for error reporting
) -> Result<(), Error<'src_lt>> {
    let mut text = String::new();
    let mut collecting_kind = ChunkKind::Whitespaces;
    let mut collected_chunks_count = 0;

    let mut chunk_start_line = 1;
    let mut chunk_start_position = 1;

    let mut cursor_line = 1;
    let mut cursor_position = 1;

    // idk how to call it
    macro_rules! state {
        (push) => {{
            let chunk_is_empty_whitespace =
                matches!(collecting_kind, ChunkKind::Whitespaces) && text.is_empty();

            if !chunk_is_empty_whitespace {
                // with this chunk.position will point on the start of the text
                let delim_len = collecting_kind.delimiters().0.len();

                chunks_buf.push(Chunk {
                    text: text.clone(),
                    kind: collecting_kind,
                    line: chunk_start_line,
                    position: chunk_start_position + delim_len,
                });
            }

            !chunk_is_empty_whitespace
        }};
        ($kind:expr) => {{
            if state!(push) {
                collected_chunks_count += 1;
            }
            chunk_start_line = cursor_line;
            chunk_start_position = cursor_position;
            collecting_kind = $kind;
            text.clear();
        }};
    }

    macro_rules! return_err_unclosed {
        ($delim:expr) => {
            return Err(Error {
                source,
                line: cursor_line,
                position: cursor_position,
                kind: ErrorKind::UnclosedDelimiter {
                    delimiter: $delim,
                    opening_line: chunk_start_line,
                    opening_position: chunk_start_position,
                },
            });
        };
    }

    while collected_chunks_count < n_chunks {
        let mb_ch = stream.next();
        let mb_next_ch = stream.peek();

        // *mega master super print debugger*
        // println!("{mb_ch:?} {mbnext_ch:?} {collecting_kind:?} {cursor_line}:{cursor_position}");

        // TODO: maybe a way to escape '::' '@' '@@' ?
        match collecting_kind {
            // Index = '@# ..'
            // Cmd = '@ ...'
            // collect until  '\n' or '::' or 'EOF'
            ChunkKind::Index | ChunkKind::OneLineCmd => match (mb_ch, mb_next_ch) {
                (Some(':'), Some(':')) => {
                    let _ = stream.next(); // remove second ':' from the iterator
                    cursor_position += 1;
                    state!(ChunkKind::TextBlock);
                }
                (Some(ch), next_ch) => {
                    if next_ch.is_none() {
                        let _ = state!(push);
                        break;
                    } else if ch == '\n' {
                        state!(ChunkKind::Whitespaces);
                    }

                    text.push(ch);
                }
                _ => break,
            },
            // Text = ':: .. ::'
            // collect until '::'
            ChunkKind::TextBlock => match (mb_ch, mb_next_ch) {
                (Some(':'), Some(':')) => {
                    state!(ChunkKind::Whitespaces);

                    let _ = stream.next(); // remove second ':' from the iterator
                    cursor_position += 1;
                }
                (Some(ch), next_ch) => {
                    text.push(ch);

                    if next_ch.is_none() {
                        return_err_unclosed!("::");
                    }
                }
                _ => break,
            },
            // CmdBlock = '@@ .. @@'
            // collect until '@@'
            ChunkKind::CmdBlock {
                ref mut closed_by_self,
            } => match (mb_ch, mb_next_ch) {
                (Some(':'), Some(':')) => {
                    *closed_by_self = false;
                    state!(ChunkKind::TextBlock);

                    let _ = stream.next(); // remove second ':' from the iterator
                    cursor_position += 1;
                }
                (Some('@'), Some('@')) => {
                    *closed_by_self = true;
                    state!(ChunkKind::Whitespaces);

                    let _ = stream.next(); // remove second '@' from the iterator
                    cursor_position += 1;
                }
                (Some(ch), next_ch) => {
                    text.push(ch);
                    if next_ch.is_none() {
                        return_err_unclosed!("@@");
                    }
                }
                _ => break,
            },
            ChunkKind::DiscardedText | ChunkKind::Whitespaces => match (mb_ch, mb_next_ch) {
                (Some('@'), Some('#')) => {
                    state!(ChunkKind::Index);

                    let _ = stream.next(); // remove '#' from the iterator
                    cursor_position += 1;
                }
                (Some(':'), Some(':')) => {
                    state!(ChunkKind::TextBlock);

                    let _ = stream.next(); // remove second ':' from the iterator
                    cursor_position += 1;
                }
                (Some('@'), Some('@')) => {
                    state!(ChunkKind::CmdBlock {
                        closed_by_self: false
                    });

                    let _ = stream.next(); // remove second '@' from the iterator
                    cursor_position += 1;
                }
                (Some(ch), next_ch) => {
                    if ch == '@' {
                        state!(ChunkKind::OneLineCmd);
                    } else if matches!(collecting_kind, ChunkKind::Whitespaces) {
                        if ch.is_whitespace() {
                            text.push(ch);
                        } else {
                            state!(ChunkKind::DiscardedText);
                        }
                    }

                    if next_ch.is_none() {
                        let _ = state!(push);
                        break;
                    }
                }
                _ => break,
            },
        }

        if let Some('\n') = mb_ch {
            cursor_line += 1;
            cursor_position = 1;
        } else {
            cursor_position += 1;
        }
    }

    Ok(())
}

// returns None if the chunk is not Cmd or CmdBlock
pub fn parse_chunk_into_cmd<'src_lt>(
    source: &'src_lt str,
    chunk: &Chunk,
) -> Result<Cmd, Error<'src_lt>> {
    // first label is a script to execute and others are arguments
    let mut args = Vec::<Label>::new();
    let mut cursor_line = chunk.line;
    let mut cursor_position = chunk.position;
    let mut chars = chunk.text.chars().peekable();

    'chars_loop: loop {
        let Some(ch) = chars.next() else {
            break 'chars_loop;
        };

        match (args.len(), ch) {
            // @: <script>
            // or
            // @@: <script> @@
            (0, ':') => {
                let mut text = chars.collect();
                trim_in_place(&mut text);

                return Ok(Cmd::AnonEval(Script {
                    text,
                    line: cursor_line,
                    position: cursor_position,
                }));
            }
            // @ <label>: <script>
            // or
            // @@ <label>: <script> @@
            (1, ':') => {
                let label = args.pop().unwrap();
                let mut text = chars.collect();
                trim_in_place(&mut text);
                let script = Script {
                    text,
                    line: cursor_line,
                    position: cursor_position,
                };

                return Ok(Cmd::Decl { label, script });
            }

            // @ <label1> <label2> ...
            (_, ch) if ch.is_alphanumeric() => {
                let mut name = String::new();

                let line = cursor_line;
                let position = cursor_position;

                name.push(ch);
                while let Some(c) = chars.next_if(|c| c.is_alphanumeric() || *c == '_') {
                    name.push(c);
                    cursor_position += 1;
                }

                args.push(Label {
                    name,
                    position,
                    line,
                })
            }
            (_, ch) => {
                if !ch.is_whitespace() {
                    return Err(Error {
                        source,
                        kind: ErrorKind::UnallowedLabelCharacter(ch),
                        line: cursor_line,
                        position: cursor_position,
                    });
                }
            }
        }

        if ch == '\n' {
            cursor_line += 1;
            cursor_position = 1;
        } else {
            cursor_position += 1;
        }
    }

    Ok(Cmd::Eval(args))
}

fn run_bash_script(
    script_label: &str,
    script: &Script,
    script_args: &[&str],
    input_text: &mut String,
) -> Result<(), String> {
    // println!("runing script: {script_label}\n{script:?}\n{script_args:?}");

    let mut child = Command::new("bash")
        .arg("-c")
        .arg(&script.text)
        .arg(script_label) // $0, doesn't do anything
        .args(script_args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(to_string)?;

    child
        .stdin
        .take()
        .expect("stdin")
        .write_all(input_text.as_bytes())
        .map_err(to_string)?;

    if !child.wait().map_err(to_string)?.success() {
        let stderr = &mut child.stdout.take().expect("stderr");
        return Err(std::io::read_to_string(stderr).map_err(to_string)?);
    }

    let stdout = &mut child.stdout.take().expect("stdout");
    input_text.clear();
    stdout.read_to_string(input_text).map_err(to_string)?;

    Ok(())
}

fn execute_cmd_decl<'src_lt>(
    label: Label,
    script: Script,
    decls: &mut HashMap<Label, Script>,
    err_stack: &mut Vec<Error<'src_lt>>,
    source: &'src_lt str,
) {
    let Some((existing_label, _)) = decls.remove_entry(&label) else {
        let _ = decls.insert(label, script);
        return;
    };

    err_stack.push(Error {
        source,
        kind: ErrorKind::LabelRedeclaration(existing_label),
        line: label.line,
        position: label.position,
    });
}

fn execute_cmd_anon_eval<'src_lt>(
    script: &Script,
    input: &mut String,
    err_stack: &mut Vec<Error<'src_lt>>,
    source: &'src_lt str,
) {
    let args: &[&str] = &[];

    let Err(e) = run_bash_script("<AnonymousScript>", script, args, input) else {
        return;
    };

    err_stack.push(Error {
        source,
        kind: ErrorKind::EvaluatingScript(e.to_string()),
        line: script.line,
        position: script.position,
    });
}

fn execute_cmd_eval<'src_lt>(
    labels: Vec<Label>,
    input: &mut String,
    decls: &HashMap<Label, Script>,
    err_stack: &mut Vec<Error<'src_lt>>,
    source: &'src_lt str,
) {
    let mut main_script = None::<(&Label, &Script)>;
    let mut args = Vec::<&str>::new();
    let mut abort = false;

    for l in labels {
        let Some((label, script)) = decls.get_key_value(&l) else {
            abort = true;
            let Label {
                name,
                line,
                position,
            } = l;

            err_stack.push(Error {
                source,
                kind: ErrorKind::UndeclaredLabel(name),
                line,
                position,
            });

            continue;
        };

        if main_script.is_none() {
            main_script = Some((label, script));
        } else {
            args.push(&script.text);
        }
    }

    if abort {
        return;
    }

    let Some((label, script)) = main_script else {
        return;
    };

    let Err(e) = run_bash_script(&label.name, script, &args, input) else {
        return;
    };

    err_stack.push(Error {
        source,
        kind: ErrorKind::EvaluatingScript(e.to_string()),
        line: script.line,
        position: script.position,
    });
}

fn evaluate_cmd<'src_lt>(
    cmd: Cmd,
    input: &mut String,
    decls: &mut HashMap<Label, Script>,
    err_stack: &mut Vec<Error<'src_lt>>,
    source: &'src_lt str,
) {
    match cmd {
        Cmd::AnonEval(script) => execute_cmd_anon_eval(&script, input, err_stack, source),
        Cmd::Eval(labels) => execute_cmd_eval(labels, input, decls, err_stack, source),
        Cmd::Decl { label, script } => execute_cmd_decl(label, script, decls, err_stack, source),
    }
}

fn update_index_chunk<'src_lt>(
    chunk: &mut Chunk,
    source: &'src_lt str,
) -> Result<(), Error<'src_lt>> {
    assert_eq!(chunk.kind, ChunkKind::Index);
    let idx = get_index_value_from_chunk(chunk, source)?;
    let nex_idx = idx + 1;
    let in_text_idx_len = chunk.text.trim().len();
    let in_text_idx_pos = chunk
        .text
        .find(|ch: char| ch.is_numeric())
        .expect("integer must be validated by `get_index_value_from_chunk`");
    let idx_range = in_text_idx_pos..(in_text_idx_pos + in_text_idx_len);
    chunk.text.replace_range(idx_range, &nex_idx.to_string());

    let updated_idx = get_index_value_from_chunk(chunk, source)?;
    assert_eq!(updated_idx, nex_idx);

    Ok(())
}

fn evaluate_chunk<'src_lt>(
    chunk: &mut Chunk,
    decls: &mut HashMap<Label, Script>,
    call_stack: &mut Vec<Cmd>,
    err_stack: &mut Vec<Error<'src_lt>>,
    source: &'src_lt str,
) {
    match chunk.kind {
        ChunkKind::OneLineCmd | ChunkKind::CmdBlock { .. } => {
            match parse_chunk_into_cmd(source, chunk) {
                Ok(cmd) => call_stack.push(cmd),
                Err(e) => err_stack.push(e),
            }
        }
        ChunkKind::Index => {
            if let Err(e) = update_index_chunk(chunk, source) {
                err_stack.push(e);
            }
        }
        ChunkKind::DiscardedText => {
            if call_stack
                .iter()
                .any(|cmd| matches!(cmd, Cmd::Eval { .. } | Cmd::AnonEval { .. }))
            {
                err_stack.push(Error {
                    source,
                    kind: ErrorKind::MissingInputTextBlock,
                    line: chunk.line,
                    position: chunk.position,
                });
            }
        }
        ChunkKind::TextBlock => {
            for cmd in call_stack.drain(..) {
                evaluate_cmd(cmd, &mut chunk.text, decls, err_stack, source);
            }
        }
        ChunkKind::Whitespaces => (),
    }
}

fn get_index_value_from_chunk<'src_lt>(
    chunk: &Chunk,
    source: &'src_lt str,
) -> Result<usize, Error<'src_lt>> {
    if !matches!(chunk.kind, ChunkKind::Index) {
        return Err(Error {
            kind: ErrorKind::NoIndexChunk,
            source,
            line: chunk.line,
            position: chunk.position,
        });
    }

    let text = &chunk.text;
    let trimmed = text.trim();

    let err_at = |line: usize, position: usize| Error {
        source,
        kind: ErrorKind::NonIntegerIndexValue,
        line,
        position,
    };

    if text.is_empty() {
        return Err(err_at(chunk.line, chunk.position));
    }

    let text_position = chunk.position
        + chunk
            .text
            .find(|ch: char| !ch.is_whitespace())
            .ok_or_else(|| err_at(chunk.line, chunk.position))?;

    trimmed
        .parse::<usize>()
        .map_err(|_| err_at(chunk.line, text_position))
}

fn collect_nya_files_in_dir(
    dir: &Path,
    found_files: &mut HashMap<usize, PathBuf>,
    chunks_buf: &mut Vec<Chunk>, // needed for recursion
) -> Result<(), String> {
    let entries = std::fs::read_dir(dir)
        .map_err(|e| format!("failed to read directory `{p}`:\n{e}", p = dir.display()))?;

    for maybe_entry in entries {
        chunks_buf.clear();

        let entry = maybe_entry.map_err(|e| {
            format!(
                "failed to read DirEntry in note directory `{p}`:\n{e}",
                p = dir.display()
            )
        })?;

        let entry_path = entry.path();
        let entry_fmt = entry_path.display();
        let is_nya_file = entry_path
            .extension()
            .is_some_and(|ext| ext.to_str().unwrap() == "nya");

        // eprintln!("[Info] Reading entry `{entry_fmt}`");

        if is_nya_file {
            let source = match std::fs::read_to_string(&entry_path) {
                Ok(s) => s,
                Err(read_err) => {
                    eprintln!("[Warn] skipping `{entry_fmt}`: {read_err}",);
                    continue;
                }
            };

            let mut stream = source.chars().peekable();
            if let Err(parse_err) = collect_chunks_from_stream(&mut stream, 1, chunks_buf, &source)
            {
                eprintln!("[Warn] skipping `{entry_fmt}`: {parse_err}");
                continue;
            }

            let Some(chunk) = chunks_buf.first() else {
                eprintln!("[Warn] skipping `{entry_fmt}`: file is empty");
                continue;
            };

            let idx = match get_index_value_from_chunk(chunk, &source) {
                Ok(value) => value,
                Err(err) => {
                    eprintln!("[Warn] skipping `{entry_fmt}: {err}`");
                    continue;
                }
            };

            if let Some(p) = found_files.insert(idx, entry_path.to_path_buf()) {
                eprintln!(
                    "[Warn] skipping `{entry_fmt}: duplicate index found `{idx}`, previously declared in `{p2}`",
                    p2 = p.display()
                );
                continue;
            }
        } else if entry_path.is_dir() {
            collect_nya_files_in_dir(&entry_path, found_files, chunks_buf)?;
        }
    }

    Ok(())
}

fn collect_chunks_from_file(
    path: &Path,
    source_buf: &mut String,
    chunks: &mut Vec<Chunk>,
) -> Result<(), String> {
    File::open(path)
        .map_err(to_string)?
        .read_to_string(source_buf)
        .map_err(to_string)?;
    let mut stream = source_buf.chars().peekable();
    collect_chunks_from_stream(&mut stream, usize::MAX, chunks, source_buf).map_err(to_string)?;

    Ok(())
}

fn collect_chunks_from_last_file_in_dir(
    dir: &Path,
    source_buf: &mut String,
    chunks: &mut Vec<Chunk>,
) -> Result<(), String> {
    let mut found = HashMap::<usize, PathBuf>::new();
    collect_nya_files_in_dir(dir, &mut found, chunks).map_err(to_string)?;

    let Some((_, path)) = found
        .into_iter()
        .max_by(|(k, _), (other_k, _)| k.cmp(other_k))
    else {
        return Err(format!(
            "`{}` doesn't contain any `.nya` file",
            dir.display()
        ));
    };

    chunks.clear();
    collect_chunks_from_file(&path, source_buf, chunks).map_err(to_string)?;

    Ok(())
}

fn main() {
    let (input, output) = match parse_args(std::env::args().skip(1)) {
        Ok(res) => res,
        Err(e) => return eprintln!("{e}"),
    };

    let mut chunks = Vec::<Chunk>::new();
    let mut decls = HashMap::<Label, Script>::new();
    let mut call_stack = Vec::<Cmd>::new();
    let mut err_stack = Vec::<Error>::new();
    let mut source = String::new();

    let collecting_result = match input {
        Input::File(f) => collect_chunks_from_file(&f, &mut source, &mut chunks),
        Input::Dir(d) => collect_chunks_from_last_file_in_dir(&d, &mut source, &mut chunks),
    };

    if let Err(e) = collecting_result {
        return eprintln!("[Err] {e}");
    }

    // println!("pre eval {chunks:#?}");

    for chunk in &mut chunks {
        evaluate_chunk(chunk, &mut decls, &mut call_stack, &mut err_stack, &source)
    }

    // println!("post eval: {chunks:#?}");

    if !call_stack.is_empty() {
        let last_chunk = chunks.last().expect("at least one chunk must exist");

        err_stack.push(Error {
            source: &source,
            kind: ErrorKind::MissingInputTextBlock,
            line: last_chunk.line,
            position: last_chunk.position,
        });
    }

    if !err_stack.is_empty() {
        for err in err_stack {
            eprintln!("[Err] {err}");
        }

        return;
    }

    source.clear();
    for chunk in chunks {
        source.push_str(&chunk.to_string());
    }

    let output_path = match output {
        Output::Stdout => return println!("{source}"),
        Output::File(p) => p,
    };

    let path_fmt = output_path.display();

    let mut output_file = match File::create(&output_path) {
        Ok(f) => f,
        Err(e) => return eprintln!("[Err] failed to create `{path_fmt}`: {e}"),
    };

    if let Err(e) = write!(output_file, "{source}") {
        return eprintln!("[Err] failed to save output to `{path_fmt}`: {e}");
    }

    println!("{path_fmt}");
}
