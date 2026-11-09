/* gemtext2pdf: read Gemtext on stdin, write PDF to stdout or to the file
   named with -o. The PDF is made by piping mom through groff, which must
   be on the PATH. */

use std::error::Error;
use std::fs::{self, File};
use std::path::PathBuf;
use std::process::{Command, ExitCode, Stdio};

use clap::Parser;

use gemtext2md::{exit_code, run, mom::{write_mom, StyleName}};

#[derive(Parser)]
#[command(about = "Convert Gemtext on stdin to PDF")]
struct Args {
    /// Write the PDF to FILENAME instead of stdout
    #[arg(short = 'o', value_name = "FILENAME")]
    output: Option<PathBuf>,

    /// Visual style of the document
    #[arg(long, value_enum, default_value_t = StyleName::Default)]
    style: StyleName,

    /// Leave *asterisks* alone instead of setting the text in italics
    #[arg(long)]
    no_italics: bool
}

fn pdf(args: &Args) -> Result<(), Box<dyn Error>> {
    let stdout = match &args.output {
        Some(path) => Stdio::from(File::create(path).map_err(|e| {
            format!("{}: {}", path.display(), e)
        })?),
        None       => Stdio::inherit()
    };

    let mut groff = Command::new("groff")
        .args(["-K", "utf-8", "-mom", "-Tpdf"])
        .stdin(Stdio::piped())
        .stdout(stdout)
        .spawn()
        .expect("couldn't run groff; is it on the PATH?");

    // write_mom closes groff's stdin when it finishes, so groff sees EOF
    let groff_in = groff.stdin.take().expect("groff has no stdin");
    let italics = !args.no_italics;
    let result = run(|rx| write_mom(rx, groff_in, args.style, italics));

    if result.is_err() {
        // don't let groff finish a PDF of half a document
        let _ = groff.kill();
        let _ = groff.wait();
        return result;
    }

    let status = groff.wait()?;
    if !status.success() {
        return Err(format!("groff failed ({status})").into());
    }

    Ok(())
}

fn main() -> ExitCode {
    let args = Args::parse();
    let result = pdf(&args);

    // don't leave an empty or partial PDF behind
    if let (Err(_), Some(path)) = (&result, &args.output) {
        let _ = fs::remove_file(path);
    }

    exit_code("gemtext2pdf", result)
}
