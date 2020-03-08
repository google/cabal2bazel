# Copyright 2020 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Helper functions to run multiple actions as one.

Every Skylark action (e.g., ctx.action()) is hermetic, and all input/output
files must be specified without depending on the contents of any particular
file.  This module gives us a little more flexibility by:
- Letting us group multiple commands into a single action.
- Abstracting over "archives" that group multiple files together into a single
  output.  This is helpful since we don't know the names of some GHC output
  files (in particular, *.hi and *.o) without running GHC itself.
"""

def _escape_arg(arg):
    """Put quotes around the input string, escaping slashes and quotes within."""

    # A little bit of a hack: don't escape arguments that splice in another
    # shell command "$(...)".
    if arg.startswith("$(") and arg.endswith(")"):
        return arg

    # Escape backslashes.  However, in the pattern "\$", leave it as-is so that we can
    # emit "$" without the shell evaluating them.  For example: "-rpath=\$EXEC_ORIGIN/..."
    return "\"" + arg.replace("\\", "\\\\").replace("\\\\$", "\\$").replace("\"", "\\\"") + "\""

def _render_action(action):
    """Turn an action struct into a single string command for bash to run.

    Args:
      action: A struct consisting of the following fields:
        cmd: A command to run.  This may either be a string or an executable File.
        args: An optional list of strings arguments which will be escaped and
          then appended to the cmd.

    Returns:
      A string consisting of the command followed by the escaped arguments (if
      any).
    """
    if hasattr(action.cmd, "path"):
        cmd = action.cmd.path
    else:
        # extra str() for PathFragment.
        cmd = str(action.cmd)

    args = getattr(action, "args", [])

    return " ".join([cmd] + [_escape_arg(arg) for arg in args])

def multi_action(
        ctx,
        actions = [],
        inputs = depset(),
        outputs = [],
        mnemonic = None,
        progress_message = "",
        env = None):
    """Combine multiple actions into a single hermetic Bazel action.

    Example usage:

        multi_action(ctx, actions=[
            struct(cmd="echo", args=["hello"]),
            struct(cmd="echo", args=["world"]),
        ])

        multi_action(ctx, actions=[
            struct(cmd="echo hello"),
            struct(cmd="echo world")
        ])

    Each action struct consists of cmd (the command to run) and, optionally, a
    list of args that will be quoted and appended to the cmd.  If the cmd is a
    File, it will automatically be added to the list of inputs.


    Args:
      ctx: The current rule context.
      actions: A list of action structs consisting of a cmd and (optionally)
        "args" field.
      inputs: A list of input Files for the given actions.
      outputs: A list of output Files created by the given actions.
      mnemonic: A one-word description of the action.
      progress_message: A progress message to show the user during the build.

    Returns:
      A single ctx.action() that runs all the given actions in sequence.
    """
    # Note that we specify arguments of multi_action() like "mnemonic" and
    # "progress_message" explicitly, rather than via kwargs.  This way, other
    # things like "arguments" which would be valid for ctx.action aren't passed
    # in here accidentally.  (It also makes the calling code a little nicer since
    # otherwise kwargs *must* be after non-kwargs.)

    executable_inputs = [
        action.cmd
        for action in actions
        if hasattr(action.cmd, "path")
    ]

    # Put each command on a separate line to make "bazel build -s" more readable.
    # (Bazel will pass this string into "bash -c", so we can treat it like
    # a shell script.)
    command = "\n".join(
        # Fail if there are any errors, including from $(..) expressions, pipes,
        # or undefined variables.
        ["set -eu -o pipefail"] +
        [_render_action(action) for action in actions],
    )
    ctx.actions.run_shell(
        command = command,
        inputs = depset(executable_inputs, transitive = [inputs]),
        mnemonic = mnemonic,
        progress_message = progress_message,
        outputs = outputs,
        env = env,
    )
