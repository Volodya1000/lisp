import sys
import click
from pathlib import Path

from compiler.wasm_compiler import WasmCompiler
from scripts.lisp_core import parse_and_analyze
from wasm_runner import WasmRunner


@click.group()
def cli():
    pass


@cli.command()
@click.argument('file', type=click.Path(exists=True, path_type=Path))
@click.option('-o', '--output', type=click.Path(path_type=Path), help='Output .wat file path')
def compile(file: Path, output: Path | None):
    """Compile Lisp file to WebAssembly Text (WAT)"""
    try:
        ast = parse_and_analyze(file)
        wat = WasmCompiler().compile(ast)

        if output:
            output.write_text(wat, encoding='utf-8')
            click.echo(f"Successfully compiled to {output}")
        else:
            click.echo(wat)
    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


@cli.command()
@click.argument('file', type=click.Path(exists=True, path_type=Path))
def run(file: Path):
    """Run Lisp program via WASM compiler"""
    try:
        ast = parse_and_analyze(file)
        wat = WasmCompiler().compile(ast)

        click.echo(f"--- Executing {file} ---")
        WasmRunner().run(wat)
    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


if __name__ == '__main__':
    cli()