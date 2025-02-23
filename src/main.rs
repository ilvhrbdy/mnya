use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter, Write as FmtWrite},
    fs::File,
    hash,
    io::{Read, Write as IoWrite},
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
                          if neither NYA_DIRECTORY nor -print is provided

Env:
    NYA_DIRECTORY      - default nya folder; act as working directory

<OUTPUT FILE NAME> could be built from multiple arguments that do not start with -
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
enum ErrorKind {
    NoIndexChunk,
    NonIntegerIndexValue,
    UnclosedDelimiter {
        delimiter: &'static str,
        opening_line: usize,
        opening_position: usize,
    },
    UnallowedLabelCharacter(char),
    UndefinedLabel(String),
    LabelRedefinition(Label), // contains second definition
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
                writeln!(
                    f,
                    "{line_num:<3}{ptr}|   {line:<4}",
                    ptr = if line_num == err_line { ">" } else { " " }
                )?;
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
                writeln!(f, "first command must be file index chunk: @# <index>")?
            }
            ErrorKind::NonIntegerIndexValue => {
                writeln!(f, "value for index of file must be an integer")?
            }
            ErrorKind::UndefinedLabel(label_name) => {
                writeln!(f, "trying to access undefined label {label_name}")?
            }
            ErrorKind::MissingInputTextBlock => {
                writeln!(f, "expected TextBlock as input for commands:")?
            }
            ErrorKind::LabelRedefinition(second) => {
                let name = &second.name;

                writeln!(f, "label {name} already exist:")?;
                point_on_err(f, err_line, err_position, self.source)?;

                writeln!(
                    f,
                    "{ptr:->5} previous definition at {}:{} here:",
                    second.line,
                    second.position,
                    ptr = ">",
                )?;
                point_on_err(f, second.line, second.position, self.source)?;

                return Ok(());
            }
            ErrorKind::UnallowedLabelCharacter(ch) => {
                writeln!(f, "unallawed label character {ch}, bruh:")?
            }
            ErrorKind::EvaluatingScript(err) => {
                writeln!(f, "script error:")?;
                for l in err.lines() {
                    writeln!(f, "{ptr:->5} {l}", ptr = ">")?;
                }
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
                    "unclosed chunk delimiter {delimiter}\n---> starts at {delim_line}:{delim_position} here:"
                )?;
                point_on_err(f, *delim_line, delim_position, self.source)?;

                writeln!(
                    f,
                    "{ptr:->5} and suppose to end at {err_line}:{err_position} here:",
                    ptr = ">"
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

fn parse_args(
    mut args: impl Iterator<Item = String>,
) -> Result<(Input, Output), Box<dyn std::error::Error>> {
    let expanded = |mut input: String| {
        if input.starts_with("~") {
            let Ok(home) = std::env::var("HOME") else {
                return input;
            };

            input.replace_range(..1, &home);
        }

        input
    };

    let mut mb_input = None::<Input>;
    let mut mb_output = None::<Output>;
    let mut output_name = String::new();
    let env_dir = std::env::var(ENV_INPUT_DIRECTORY).ok().map(expanded);

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-h" => return Err(USAGE.into()),
            "-print" => mb_output = Some(Output::Stdout),
            "-i" => match args.next() {
                Some(file_name) => mb_input = Some(Input::File(PathBuf::from(file_name))),
                None => return Err("input file for -i flag where?".into()),
            },
            a if a.starts_with("-") => {
                return Err("i don't know what this flag means..".into());
            }
            _ if !matches!(mb_output, Some(Output::Stdout)) => {
                output_name.push_str(&arg);
                output_name.push('_');
            }
            _ => (),
        }
    }

    let output = if let Some(out @ Output::Stdout) = mb_output {
        out
    } else if output_name.trim().is_empty() {
        return Err("throw some words for enw note file name".into());
    } else {
        let _ = output_name.pop(); // remove trailing '_'

        let mut path = if let Some(dir) = &env_dir {
            PathBuf::from(dir)
        } else {
            std::env::current_dir()?
        };

        path.push(output_name);

        Output::File(path)
    };

    let input = mb_input
        .or_else(|| env_dir.map(PathBuf::from).map(Input::Dir))
        .ok_or_else(|| {
            format!("either provide an input .nya file with -i flag or set {ENV_INPUT_DIRECTORY}")
        })?;

    // throw error before evaluation
    // input path will be checked later with read_to_string
    if let Output::File(f) = &output {
        if f.exists() {
            return Err(format!("{} already exists", f.display()).into());
        }
    }

    Ok((input, output))
}

fn run_bash_script(
    script_label: &str,
    script: &Script,
    script_args: &[&str],
    input_text: &mut String,
) -> Result<(), Box<dyn std::error::Error>> {
    // println!("runing script: {script_label}\n{s}\n{script_args:?}", s = script.text);

    let mut child = Command::new("bash")
        .arg("-c")
        .arg(&script.text)
        .arg(script_label) // $0, doesn't do anything
        .args(script_args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;

    child
        .stdin
        .take()
        .expect("stdin")
        .write_all(input_text.as_bytes())?;

    if !child.wait()?.success() {
        let stderr = &mut child.stderr.take().expect("stderr");

        return Err(std::io::read_to_string(stderr)?.into());
    }

    let stdout = &mut child.stdout.take().expect("stdout");
    input_text.clear();
    stdout.read_to_string(input_text).map_err(to_string)?;

    Ok(())
}

#[derive(Debug)]
struct TwoCharsWindowIter<'src_lt> {
    source: &'src_lt str,
    source_bytes: &'src_lt [u8],
    idx: usize,
    cursor_line: usize,
    cursor_position: usize,
}

impl TwoCharsWindowIter<'_> {
    fn back(&mut self) {
        let new_idx = self.idx.saturating_sub(1);

        if self.idx == new_idx {
            return;
        }

        self.idx = new_idx;
        let prev_ch = self.source_bytes[new_idx] as char;

        if prev_ch != '\n' {
            self.cursor_position -= 1;
            return;
        }

        self.cursor_line -= 1;
        self.cursor_position = self.source.lines().nth(self.cursor_line).unwrap().len();
    }
}

impl Iterator for TwoCharsWindowIter<'_> {
    type Item = (char, Option<char>);

    fn next(&mut self) -> Option<Self::Item> {
        let i = self.idx;
        let t = self.source_bytes;

        let first = t.get(i).map(|b| *b as char)?;
        let mb_second = t.get(i + 1).map(|b| *b as char);

        self.idx += 1;

        if first == '\n' {
            self.cursor_line += 1;
            self.cursor_position = 1;
        } else {
            self.cursor_position += 1;
        }

        Some((first, mb_second))
    }
}

impl<'sl> From<&'sl str> for TwoCharsWindowIter<'sl> {
    fn from(source: &'sl str) -> Self {
        Self {
            source,
            source_bytes: source.as_bytes(),
            idx: 0,
            cursor_line: 1,
            cursor_position: 1,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ChunkKind {
    // @# ...
    Index,
    // @ ...
    OneLineCmd,
    // @@ ... @@
    // closed_by_self = @@ ... @@
    // !closed_by_self = @@ .. :: or EOF
    CmdBlock { closed_by_self: bool },
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
    // start of the chunk, excluding delimiter
    pub line: usize,
    pub position: usize,
}

impl Default for Chunk {
    fn default() -> Self {
        Self {
            line: 1,
            position: 1,
            kind: ChunkKind::Whitespaces,
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

fn collect_next_chunk<'src_lt>(
    stream: &mut TwoCharsWindowIter<'src_lt>,
) -> Option<Result<Chunk, Error<'src_lt>>> {
    let mut chunk = Chunk::default();

    let is_useless =
        |chunk: &Chunk| matches!(chunk.kind, ChunkKind::Whitespaces) && chunk.text.is_empty();

    macro_rules! start_chunk {
        ($kind:expr) => {{
            chunk.kind = $kind;
            let (opening, _) = chunk.kind.delimiters();
            let delim_len = opening.len();
            chunk.position = delim_len + stream.cursor_position;
            chunk.line = stream.cursor_line;

            // first char of delim is eaten at this point
            for _ in 0..delim_len.saturating_sub(1) {
                let _ = stream.next();
            }
        }};
    }

    macro_rules! close_and_return_chunk {
        () => {{
            let (_, closing) = chunk.kind.delimiters();
            // first char of delim is eaten at this point
            for _ in 0..closing.len().saturating_sub(1) {
                let _ = stream.next();
            }
            return Some(Ok(chunk));
        }};
    }

    macro_rules! return_err_unclosed {
        () => {
            let (opening, closing) = chunk.kind.delimiters();
            return Some(Err(Error {
                source: stream.source,
                line: stream.cursor_line,
                position: stream.cursor_position,
                kind: ErrorKind::UnclosedDelimiter {
                    delimiter: closing,
                    opening_line: chunk.line,
                    opening_position: chunk.position - opening.len(),
                },
            }));
        };
    }

    while let Some(next_chs) = stream.next() {
        // TODO: maybe a way to escape '::' '@' '@@' ?
        match chunk.kind {
            // Index = '@# ..'
            // Cmd = '@ ...'
            // collect until  '\n' or '::' or 'EOF'
            ChunkKind::Index | ChunkKind::OneLineCmd => match next_chs {
                (':', Some(':')) => {
                    stream.back();
                    return Some(Ok(chunk));
                }
                ('\n', _) => {
                    stream.back();
                    return Some(Ok(chunk));
                }
                (ch, mb_next) => {
                    chunk.text.push(ch);
                    if mb_next.is_none() {
                        return Some(Ok(chunk));
                    }
                }
            },
            // Text = ':: .. ::'
            // collect until '::'
            ChunkKind::TextBlock => match next_chs {
                (':', Some(':')) => close_and_return_chunk!(),
                (ch, next_ch) => {
                    chunk.text.push(ch);
                    if next_ch.is_none() {
                        return_err_unclosed!();
                    }
                }
            },
            // CmdBlock = '@@ .. @@'
            // collect until '@@'
            ChunkKind::CmdBlock {
                ref mut closed_by_self,
            } => match next_chs {
                (':', Some(':')) => {
                    *closed_by_self = false;
                    stream.back();
                    return Some(Ok(chunk));
                }
                ('@', Some('@')) => {
                    *closed_by_self = true;
                    close_and_return_chunk!();
                }
                (ch, next_ch) => {
                    chunk.text.push(ch);
                    if next_ch.is_none() {
                        return_err_unclosed!();
                    }
                }
            },
            ChunkKind::DiscardedText | ChunkKind::Whitespaces => {
                let current_is_useless = is_useless(&chunk);
                macro_rules! start_if_useless {
                    ($kind:expr) => {{
                        if current_is_useless {
                            start_chunk!($kind);
                        } else {
                            stream.back();
                            return Some(Ok(chunk));
                        }
                    }};
                }

                match next_chs {
                    ('@', Some('#')) => start_if_useless!(ChunkKind::Index),
                    (':', Some(':')) => start_if_useless!(ChunkKind::TextBlock),
                    ('@', Some('@')) => start_if_useless!(ChunkKind::CmdBlock {
                        closed_by_self: false
                    }),
                    ('@', _) => start_if_useless!(ChunkKind::OneLineCmd),
                    (ch, _) if chunk.kind == ChunkKind::Whitespaces => {
                        if ch.is_whitespace() {
                            chunk.text.push(ch);
                        } else {
                            start_if_useless!(ChunkKind::DiscardedText);
                        }
                    }
                    _ => (),
                }
            }
        }
    }

    if is_useless(&chunk) {
        return None;
    }

    Some(Ok(chunk))
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
    Define { label: Label, script: Script },
}

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

                return Ok(Cmd::Define { label, script });
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

fn execute_cmd_define<'src_lt>(
    label: Label,
    script: Script,
    defines: &mut HashMap<Label, Script>,
    err_stack: &mut Vec<Error<'src_lt>>,
    source: &'src_lt str,
) {
    let Some((existing_label, _)) = defines.remove_entry(&label) else {
        let _ = defines.insert(label, script);
        return;
    };

    err_stack.push(Error {
        source,
        kind: ErrorKind::LabelRedefinition(existing_label),
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
    defines: &HashMap<Label, Script>,
    err_stack: &mut Vec<Error<'src_lt>>,
    source: &'src_lt str,
) {
    let mut main_script = None::<(&Label, &Script)>;
    let mut args = Vec::<&str>::new();
    let mut abort = false;

    for l in labels {
        let Some((label, script)) = defines.get_key_value(&l) else {
            abort = true;
            let Label {
                name,
                line,
                position,
            } = l;

            err_stack.push(Error {
                source,
                kind: ErrorKind::UndefinedLabel(name),
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

fn execute_cmd<'src_lt>(
    cmd: Cmd,
    input: &mut String,
    defines: &mut HashMap<Label, Script>,
    err_stack: &mut Vec<Error<'src_lt>>,
    source: &'src_lt str,
) {
    match cmd {
        Cmd::AnonEval(script) => execute_cmd_anon_eval(&script, input, err_stack, source),
        Cmd::Eval(labels) => execute_cmd_eval(labels, input, defines, err_stack, source),
        Cmd::Define { label, script } => {
            execute_cmd_define(label, script, defines, err_stack, source)
        }
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
        .expect("integer must be validated by get_index_value_from_chunk");
    let idx_range = in_text_idx_pos..(in_text_idx_pos + in_text_idx_len);
    chunk.text.replace_range(idx_range, &nex_idx.to_string());

    let updated_idx = get_index_value_from_chunk(chunk, source)?;
    assert_eq!(updated_idx, nex_idx);

    Ok(())
}

fn evaluate_chunk<'src_lt>(
    chunk: &mut Chunk,
    defines: &mut HashMap<Label, Script>,
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
                execute_cmd(cmd, &mut chunk.text, defines, err_stack, source);
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
) -> Result<bool, String> {
    let mut all_succeeded = true;
    let dir_fmt = dir.display();
    let entries =
        std::fs::read_dir(dir).map_err(|e| format!("failed to read directory {dir_fmt}: {e}"))?;

    'entries_loop: for maybe_entry in entries {
        macro_rules! go_next {
            ($lbl:lifetime, $msg:expr) => {{
                all_succeeded = false;
                eprintln!($msg);
                continue $lbl;
            }};
            ($msg:expr) => {{
                go_next!('entries_loop, $msg);
            }}
        }

        let entry = match maybe_entry {
            Ok(e) => e,
            Err(err) => {
                go_next!("[Warn] failed to read entry in note directory {dir_fmt}:\n{err}")
            }
        };

        let entry_path = entry.path();
        let entry_fmt = entry_path.display();
        let is_nya_file = entry_path
            .extension()
            .is_some_and(|ext| ext.to_str().unwrap() == "nya");

        // eprintln!("[Info] Reading entry {entry_fmt}");

        if !is_nya_file {
            continue 'entries_loop;
        } else if entry_path.is_dir() {
            all_succeeded = collect_nya_files_in_dir(&entry_path, found_files)?;
            continue 'entries_loop;
        }

        let source = match std::fs::read_to_string(&entry_path) {
            Ok(s) => s,
            Err(read_err) => {
                go_next!("[Warn] skipping {entry_fmt}: {read_err}");
            }
        };

        let mut stream = TwoCharsWindowIter::from(source.as_str());

        let idx_chunk = 'searching_chunk: loop {
            match collect_next_chunk(&mut stream) {
                Some(Ok(Chunk {
                    kind: ChunkKind::Whitespaces,
                    ..
                })) => continue 'searching_chunk,
                Some(Ok(
                    ch @ Chunk {
                        kind: ChunkKind::Index,
                        ..
                    },
                )) => break 'searching_chunk ch,
                Some(Err(e)) => go_next!('entries_loop, "[Warn] skipping {entry_fmt}:\n{e}"),
                _ => {
                    go_next!('entries_loop, "[Warn] skipping {entry_fmt}: couldn't find an index chunk")
                }
            }
        };

        let idx = match get_index_value_from_chunk(&idx_chunk, &source) {
            Ok(value) => value,
            Err(err) => {
                go_next!("[Warn] skipping {entry_fmt}:\n{err}");
            }
        };

        if let Some(p) = found_files.insert(idx, entry_path.to_path_buf()) {
            let other_f = p.display();
            go_next!(
                "[Warn] skipping {entry_fmt}: duplicate index found {idx}, previously defineded in {other_f}"
            );
        }
    }

    Ok(all_succeeded)
}

fn read_nya_file(path: &Path) -> Result<(String, Vec<Chunk>), String> {
    let mut chunks_buf = Vec::<Chunk>::new();

    let source_buf = std::fs::read_to_string(path).map_err(to_string)?;
    let mut stream = TwoCharsWindowIter::from(source_buf.as_str());

    while let Some(res) = collect_next_chunk(&mut stream) {
        chunks_buf.push(res.map_err(to_string)?);
    }

    Ok((source_buf, chunks_buf))
}

fn find_last_nya(dir: &Path) -> Result<Option<PathBuf>, String> {
    let mut found = HashMap::<usize, PathBuf>::new();
    let all_succeeded = collect_nya_files_in_dir(dir, &mut found).map_err(to_string)?;

    if !all_succeeded {
        return Err(format!(
            "there was some issues while reading {}",
            dir.display()
        ));
    }

    let mb_path = found
        .into_iter()
        .max_by(|(k, _), (other_k, _)| k.cmp(other_k))
        .map(|(_, p)| p);

    Ok(mb_path)
}

fn create_new_nya(
    path: &Path,
    mut source: String,
    chunks: impl IntoIterator<Item = Chunk>,
) -> Result<(), String> {
    let mut f = File::create(path).map_err(to_string)?;

    for chunk in chunks.into_iter() {
        write!(&mut source, "{chunk}").map_err(to_string)?;
    }

    write!(&mut f, "{source}",).map_err(to_string)?;

    Ok(())
}

fn handle_dir_input(dir: PathBuf, output: Output) {
    match find_last_nya(&dir) {
        Ok(Some(f)) => return handle_file_input(f, output),
        Err(e) => return eprintln!("[Err] {e}"),
        _ => (),
    }

    let dir_fmt = dir.display();

    let Output::File(f) = output else {
        return eprintln!("[Warn] couldn't find any .nya files in {dir_fmt}");
    };

    eprintln!("[Info] creating your first .nya in {dir_fmt}");

    let index_chunk = Chunk {
        kind: ChunkKind::Index,
        text: " 1".into(),
        ..Default::default()
    };

    if let Err(e) = create_new_nya(&f, String::new(), Some(index_chunk)) {
        eprintln!("[Err] failed to create {}: {e}", f.display())
    }

    println!("{}", f.display())
}

fn handle_file_input(path: PathBuf, output: Output) {
    let mut defines = HashMap::<Label, Script>::new();
    let mut call_stack = Vec::<Cmd>::new();
    let mut err_stack = Vec::<Error>::new();

    let (source, mut chunks) = match read_nya_file(&path) {
        Ok(s) => s,
        Err(e) => return eprintln!("[Err] in {}:\n{e}", path.display()),
    };

    for chunk in &mut chunks {
        evaluate_chunk(
            chunk,
            &mut defines,
            &mut call_stack,
            &mut err_stack,
            &source,
        )
    }

    if !call_stack.is_empty() {
        let last_chunk = chunks.last().expect("at least one chunk must exist");

        err_stack.push(Error {
            source: &source,
            kind: ErrorKind::MissingInputTextBlock,
            line: last_chunk.line,
            position: last_chunk.position,
        });
    }

    let path_fmt = path.display();
    if !err_stack.is_empty() {
        eprintln!("[Err] in {path_fmt}");

        for err in err_stack {
            eprintln!("{err}");
        }

        return;
    }

    let output_path = match output {
        Output::Stdout => {
            for chunk in chunks {
                print!("{chunk}");
            }
            return;
        },
        Output::File(p) => p,
    };

    let path_fmt = output_path.display();

    if let Err(e) = create_new_nya(&output_path, source, chunks) {
        return eprintln!("[Err] failed to create {path_fmt}: {e}");
    }

    println!("{path_fmt}");
}

fn main() {
    let (input, output) = match parse_args(std::env::args().skip(1)) {
        Ok(res) => res,
        Err(e) => return eprintln!("[Err] {e}"),
    };

    match input {
        Input::File(f) => handle_file_input(f, output),
        Input::Dir(d) => handle_dir_input(d, output),
    }
}
