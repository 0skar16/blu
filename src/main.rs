#![feature(fs_try_exists)]
#![feature(let_chains)]
#![feature(path_file_prefix)]
use anyhow::{bail, Result};
use blu::{
    lexer::{Lexer, LexerError},
    parser::{Parser as BluParser, ParserError},
};
use clap::{Args, Parser, Subcommand};
use colored::Colorize;
use serde::{Deserialize, Serialize};
use std::{
    env::{current_dir, set_current_dir},
    path::PathBuf,
    process::{exit, Command},
    time::Instant,
};
use walkdir::WalkDir;
#[derive(Parser, Debug)]
#[command(author, version, about="A compiler for the Blu language", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    New { name: PathBuf },
    Build {},
    Init {},
    Compile(CompileArgs),
}

#[derive(Args, Debug)]
struct CompileArgs {
    input: PathBuf,
    #[arg(long, short)]
    output: String,
}
fn main() -> Result<()> {
    let blu_home = if let Some(home) = home::home_dir() {
        let blu = home.join(".blu");
        std::fs::create_dir_all(blu.join("modules"))?;
        Some(blu)
    } else {
        None
    };
    let cli = Cli::try_parse()?;
    match cli.command {
        Commands::Compile(CompileArgs { input, output }) => {
            let code = std::fs::read_to_string(&input)?;
            let filename = if let Some(str) = input.file_name() {
                str.to_string_lossy().to_string()
            } else {
                "N/A".to_string()
            };
            let lex_start = Instant::now();
            let token_stream = map_lexer_err!(Lexer::new(code.chars()).tokenize(), filename);
            println!("Lex: {}", (Instant::now() - lex_start).as_micros());
            let parse_start = Instant::now();
            let ast = map_parser_err!(BluParser::new(token_stream).parse(), filename);
            println!("Parse: {}", (Instant::now() - parse_start).as_micros());

            let compile_start = Instant::now();
            let compiled = blu::compiler::compile(ast);
            println!("Compile: {}", (Instant::now() - compile_start).as_micros());
            if output == "-" {
                print!("{}", compiled);
            } else {
                std::fs::write(output, compiled)?;
            }
        }
        Commands::New { name } => {
            let mut filename = "unknown".to_string();
            if let Some(str) = name.to_string_lossy().to_string().split("/").last() {
                filename = str.to_string();
            }
            println!(
                "{} new project, named: {}",
                "Creating".green().bold(),
                filename
            );
            std::fs::create_dir_all(name.join("src"))?;
            std::fs::create_dir_all(name.join("target"))?;
            std::fs::write(
                name.join("blu.yml"),
                format!(include_str!("default_blu.yml"), filename),
            )?;
            std::fs::write(name.join("src/main.blu"), "print(\"Hello world!\");")?;
        }
        Commands::Build {} => {
            let dir = current_dir()?;
            if !std::fs::try_exists(dir.join("blu.yml"))? {
                bail!("Blu manifest isn't in the current directory!");
            }
            let _ = std::fs::remove_dir_all(dir.join("target"));
            std::fs::create_dir_all(dir.join("target"))?;
            let manifest: BluManifest =
                serde_yaml::from_reader(std::fs::File::open(dir.join("blu.yml"))?)?;
            set_current_dir(dir.join("target"))?;
            for cmd in manifest.pre_compile {
                let out = Command::new("/usr/bin/sh")
                    .args(["-c", &format!("\"{:?}\"", cmd)])
                    .output()?;
                let str = std::str::from_utf8(&out.stderr);
                if !out.status.success() {
                    err!(format!(
                        "Pre compile command `{}` in [{}] erorred!\n\t{}",
                        cmd,
                        manifest.name,
                        str.unwrap_or("Couldn't decode!").replace("\n", "\n\t")
                    ));
                }
            }

            build_dir(dir.join("src"), dir.join("target"))?;
            if manifest.include.len() > 0 {
                set_current_dir(&dir)?;
                std::fs::create_dir_all(dir.join("target").join("modules"))?;
                for include in manifest.include {
                    let path = if let Some(path) = include.path && let Ok(exists) = std::fs::try_exists(path.join("blu.yml")) && exists {
                        Some(path)
                    }else if let Some(ref blu_home) = blu_home && let Ok(exists) = std::fs::try_exists(blu_home.join("modules").join(&include.name.replace(".", "/")).join("blu.yml")) && exists {
                        Some(blu_home.join("modules").join(&include.name.replace(".", "/")))
                    } else {
                        None
                    };
                    if let Some(path) = path {
                        let mod_manifest: BluManifest =
                            serde_yaml::from_reader(std::fs::File::open(path.join("blu.yml"))?)?;
                        std::fs::create_dir_all(
                            dir.join("target").join("modules").join(&mod_manifest.name),
                        )?;
                        set_current_dir(
                            dir.join("target").join("modules").join(&mod_manifest.name),
                        )?;
                        for cmd in mod_manifest.pre_compile {
                            let out = Command::new("/usr/bin/sh")
                                .args(["-c", &format!("\"{:?}\"", cmd)])
                                .output()?;
                            let str = std::str::from_utf8(&out.stderr);
                            if !out.status.success() {
                                err!(format!(
                                    "Pre compile command `{}` in [{}] erorred!\n\t{}",
                                    cmd,
                                    manifest.name,
                                    str.unwrap_or("Couldn't decode!").replace("\n", "\n\t")
                                ));
                            }
                        }
                        set_current_dir(&dir)?;
                        build_dir(
                            path.join("src"),
                            dir.join("target").join("modules").join(&mod_manifest.name),
                        )?;
                        for cmd in mod_manifest.post_compile {
                            let out = Command::new("/usr/bin/sh")
                                .args(["-c", &format!("\"{:?}\"", cmd)])
                                .output()?;
                            let str = std::str::from_utf8(&out.stderr);
                            if !out.status.success() {
                                err!(format!(
                                    "Post compile command `{}` in [{}] erorred!\n\t{}",
                                    cmd,
                                    mod_manifest.name,
                                    str.unwrap_or("Couldn't decode!").replace("\n", "\n\t")
                                ));
                            }
                        }
                    } else {
                        bail!("Couldn't locate the path for module {}", include.name);
                    }
                }
            }
            set_current_dir(dir.join("target"))?;
            for cmd in manifest.post_compile {
                let out = Command::new("/usr/bin/sh")
                    .args(["-c", &format!("\"{:?}\"", cmd)])
                    .output()?;
                let str = std::str::from_utf8(&out.stderr);
                if !out.status.success() {
                    err!(format!(
                        "Post compile command `{}` in [{}] erorred!\n\t{}",
                        cmd,
                        manifest.name,
                        str.unwrap_or("Couldn't decode!").replace("\n", "\n\t")
                    ));
                }
            }
        }
        Commands::Init {} => {
            let dir = current_dir()?;
            let mut filename = "unknown".to_string();
            if let Some(str) = dir.to_string_lossy().to_string().split("/").last() {
                filename = str.to_string();
            }
            std::fs::create_dir_all(dir.join("src"))?;
            std::fs::create_dir_all(dir.join("target"))?;
            std::fs::write(
                dir.join("blu.yml"),
                format!(include_str!("default_blu.yml"), filename),
            )?;
            std::fs::write(dir.join("src/init.blu"), "print(\"Hello world!\");")?;
        }
    }
    Ok(())
}
#[derive(Debug, Serialize, Deserialize)]
struct BluManifest {
    name: String,
    authors: Vec<String>,
    include: Vec<Inclusion>,
    compile: BuildType,
    pre_compile: Vec<String>,
    post_compile: Vec<String>,
}
#[derive(Debug, Serialize, Deserialize)]
pub enum BuildType {
    #[serde(rename = "tree")]
    Tree,
}
#[derive(Debug, Serialize, Deserialize)]
pub struct Inclusion {
    name: String,
    path: Option<PathBuf>,
}
fn build_dir(dir: PathBuf, out: PathBuf) -> Result<()> {
    let root = dir.to_string_lossy().to_string();
    let new_root = out.to_string_lossy().to_string();
    for src in WalkDir::new(dir).into_iter() {
        if let Ok(path) = src && path.metadata()?.is_file() {
            let path = path.into_path();
            let new_path = path.to_string_lossy().to_string().replace(&root, &new_root).replace(".blu", ".lua");
            let new_path = PathBuf::from(new_path);
            if let Some(parent) = new_path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            let ext = path.extension();
            if ext.is_none() || ext.unwrap().to_str() != Some("blu") {
                std::fs::copy(path, new_path)?;
                continue;
            }
            let src = std::fs::read_to_string(&path)?;
            let err_filename = path.to_string_lossy().to_string().replace(&root, "");
            let token_stream = map_lexer_err!(Lexer::new(src.chars()).tokenize(), err_filename);
            let ast = map_parser_err!(BluParser::new(token_stream).parse(), err_filename);
            let compiled = blu::compiler::compile(ast);
            std::fs::write(new_path, compiled)?;
        }
    }
    Ok(())
}
#[macro_export]
macro_rules! map_lexer_err {
    ($lout:expr, $filename:expr) => {
        $lout.map_err(|l_err| {
            let error = match l_err {
                LexerError::UnexpectedChar(char, line, col) => format!("Lexer errored:\n\tUnexpected char [{}] at {}:{}:{}", char, $filename, line, col),
                LexerError::UnexpectedEof(line, col) => format!("Lexer errored:\n\tUnexpected end of file at {}:{}:{}", $filename, line, col),
                LexerError::ExpectedCharNotExisting(char, line, col) => format!("Lexer errored:\n\tExpected char [{}], but didn't get one at {}:{}:{}", char, $filename, line, col),
            };
            err!(error);
        }).unwrap()
    };
}
#[macro_export]
macro_rules! map_parser_err {
    ($pout:expr, $filename:expr) => {
        $pout.map_err(|p_err| {
            let error = match p_err {
                ParserError::UnexpectedTokenEx(tok, ex_tok) => format!("Parser errored:\n\tUnexpected token [{:?}:`{}`] instead of [{:?}] at {}:{}:{}", tok.token, tok.contents, ex_tok, $filename, tok.line, tok.col),
                ParserError::UnexpectedToken(tok) => format!("Parser errored:\n\tUnexpected token [{:?}:`{}`] at {}:{}:{}", tok.token, tok.contents, $filename, tok.line, tok.col),
                ParserError::UnexpectedTokenExKind(tok, ex_tok_kind) => format!("Parser errored:\n\tUnexpected token [{:?}:`{}`] instead of [{:?}] at {}:{}:{}", tok.token, tok.contents, ex_tok_kind, $filename, tok.line, tok.col),
                ParserError::UnexpectedEos => format!("Parser errored:\n\tUnexpected end of Token Stream"),
                ParserError::UnexpectedEosEx(tk) => format!("Parser errored:\n\tUnexpected end of Token Stream, expected: [{:?}]", tk),
                ParserError::UnexpectedEosExKind(tk) => format!("Parser errored:\n\tUnexpected end of Token Stream, expected: [{:?}]", tk),
                ParserError::DoubleDefaultCase(col, line) => format!("Parser errored:\n\tUnexpected double default case of a match statement: [{}:{col}:{line}]", $filename),
            };
            err!(error);

        }).unwrap()
    };
}
#[macro_export]
macro_rules! err {
    ($error:expr) => {
        eprintln!("   {}: {}", "error".red().bold(), $error);
        exit(1);
    };
}
