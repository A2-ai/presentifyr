#!/bin/bash

# uv_setup.sh venv_directory python-pptx_version uv_version [python_version]

# Check if uv is installed.
if ! command -v uv &> /dev/null; then
  echo "'uv' is not installed. Installing version $3..."
  curl --proto '=https' --tlsv1.2 -LsSf "https://github.com/astral-sh/uv/releases/download/$3/uv-installer.sh" | sh
fi

if ! grep -q 'export PATH="$HOME/.cargo/bin:$PATH"' $HOME/.bashrc; then
    echo "$HOME/.cargo/bin is not in PATH, adding it now..."
    echo 'export PATH="$HOME/.cargo/bin:$PATH"' >> $HOME/.bashrc
    export PATH="$HOME/.cargo/bin:$PATH"
fi

source $HOME/.profile
source $HOME/.bashrc

if [ ! -d "$1/.venv" ]; then
  echo "Creating venv at $1/.venv"
  if [ -n "$4" ]; then
    uv venv "$1/.venv" --python="$4"  # Use the Python version provided in $4
  else
    uv venv "$1/.venv"  # Default version if $4 is not provided
  fi
fi

source "$1/.venv/bin/activate"

# Check if python-pptx is installed, install it if not
if ! python -c "import pptx" &> /dev/null; then
  if [ -n "$2" ]; then
    uv pip install "python-pptx==$2"
  else
    uv pip install "python-pptx==1.0.2" # default version this branch should never run from R
  fi
fi
