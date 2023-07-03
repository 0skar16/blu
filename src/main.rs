#![feature(fs_try_exists)]
#![feature(let_chains)]
#![feature(path_file_prefix)]
use std::{path::PathBuf, env::{current_dir, set_current_dir}, process::Command};
use anyhow::{Result, bail};
use blu::parser::parse_blu;
use clap::{Parser, Subcommand, Args};
use colored::Colorize;
use serde::{Serialize, Deserialize};
use walkdir::WalkDir;
#[derive(Parser, Debug)]
#[command(author, version, about="A compiler for the Blu language", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand,Debug)]
enum Commands {
    New{
        name: PathBuf,
    },
    Build{

    },
    Init{
        
    },
    Compile(CompileArgs),
}

#[derive(Args,Debug)]
struct CompileArgs {
    input: PathBuf,
    #[arg(long,short)]
    output: String,
}
fn main() -> Result<()> {
    let blu_home = if let Some(home) = home::home_dir() {
        let blu = home.join(".blu");
        std::fs::create_dir_all(blu.join("modules"))?;
        Some(blu)
    }else{
        None
    };
    let cli = Cli::try_parse()?;
    match cli.command {
        Commands::Compile(CompileArgs { input, output }) => {
            let code = std::fs::read_to_string(&input)?;
            let filename = if let Some(str) = input.file_name() {
                str.to_string_lossy().to_string()
            }else {
                "N/A".to_string()
            };
            let ast = parse_blu(code.as_str(), filename)?;
            let compiled = blu::compiler::compile(ast);
            if output == "-" {
                print!("{}", compiled);
            }else{
                std::fs::write(output, compiled)?;
            }
        },
        Commands::New { name } => {
            let mut filename = "unknown".to_string();
            if let Some(str) = name.to_string_lossy().to_string().split("/").last() {
                filename = str.to_string();
            }
            println!("{} new project, named: {}", "Creating".green().bold(), filename);
            std::fs::create_dir_all(name.join("src"))?;
            std::fs::create_dir_all(name.join("target"))?;
            std::fs::write(name.join("blu.yml"), format!(include_str!("default_blu.yml"), filename))?;
            std::fs::write(name.join("src/main.blu"), "print(\"Hello world!\");")?;
        },
        Commands::Build {  } => {
            let dir = current_dir()?;
            if !std::fs::try_exists(dir.join("blu.yml"))? {
                bail!("Blu manifest isn't in the current directory!");
            }
            let _ = std::fs::remove_dir_all(dir.join("target"));
            std::fs::create_dir_all(dir.join("target"))?;
            let manifest: BluManifest = serde_yaml::from_reader(std::fs::File::open(dir.join("blu.yml"))?)?;
            set_current_dir(dir.join("target"))?;
            for cmd in manifest.pre_compile {
                let _cmd = Command::new("usr/bin/sh").args(["-c", &format!("\"{:?}\"", cmd)]).output()?;
            }

            build_dir(dir.join("src"), dir.join("target"))?;
            if manifest.include.len() > 0 {
                set_current_dir(&dir)?;
                std::fs::create_dir_all(dir.join("target").join("modules"))?;
                for include in manifest.include {
                    let path = if let Some(path) = include.path && let Ok(exists) = std::fs::try_exists(path.join("blu.yml")) && exists {
                        Some(path)
                    }else if let Some(ref blu_home) = blu_home && let Ok(exists) = std::fs::try_exists(blu_home.join(&include.name).join("blu.yml")) && exists {
                        Some(blu_home.join(&include.name))
                    } else {
                        None
                    };
                    if let Some(path) = path {
                        let mod_manifest: BluManifest = serde_yaml::from_reader(std::fs::File::open(path.join("blu.yml"))?)?;
                        std::fs::create_dir_all(dir.join("target").join("modules").join(&mod_manifest.name))?;
                        set_current_dir(dir.join("target").join("modules").join(&mod_manifest.name))?;
                        for cmd in mod_manifest.pre_compile {
                            let _cmd = Command::new("usr/bin/sh").args(["-c", &format!("\"{:?}\"", cmd)]).output()?;
                        }
                        set_current_dir(&dir)?;
                        build_dir(path.join("src"), dir.join("target").join("modules").join(&mod_manifest.name))?;
                        for cmd in mod_manifest.post_compile {
                            let _cmd = Command::new("usr/bin/sh").args(["-c", &format!("\"{:?}\"", cmd)]).output()?;
                        }
                    }else{
                        bail!("Couldn't locate the path for module {}", include.name);
                    }
                }
            }
            set_current_dir(dir.join("target"))?;
            for cmd in manifest.post_compile {
                let _cmd = Command::new("usr/bin/sh").args(["-c", &format!("\"{:?}\"", cmd)]).output()?;
            }
        },
        Commands::Init {} => {
            let dir = current_dir()?;
            let mut filename = "unknown".to_string();
            if let Some(str) = dir.to_string_lossy().to_string().split("/").last() {
                filename = str.to_string();
            }
            std::fs::create_dir_all(dir.join("src"))?;
            std::fs::create_dir_all(dir.join("target"))?;
            std::fs::write(dir.join("blu.yml"), format!(include_str!("default_blu.yml"), filename))?;
            std::fs::write(dir.join("src/init.blu"), "print(\"Hello world!\");")?;
        },
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
    post_compile: Vec<String>
}
#[derive(Debug, Serialize, Deserialize)]
pub enum BuildType {
    #[serde(rename = "tree")]
    Tree
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
            let filename = if let Some(str) = path.file_name() {
                str.to_string_lossy().to_string()
            }else {
                "N/A".to_string()
            };
            let src = std::fs::read_to_string(&path)?;
            let ast = parse_blu(&src, filename)?;
            let compiled = blu::compiler::compile(ast);
            dbg!(&path);
            let new_path = path.to_string_lossy().to_string().replace(&root, &new_root).replace(".blu", ".lua");
            dbg!(&new_path);
            std::fs::write(new_path, compiled)?;
        }
    }
    Ok(())
}