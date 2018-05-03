# Copyright 2018 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    https://www.apache.org/licenses/LICENSE-2.0
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
  # A little bit of a hack: don't escape arguments that contain a wildcard.
  # TODO(judahjacobson): Remove this after we switch to proto-lens.  This is
  # only used to list the source files in the archive output of hprotoc.
  if arg.find("*") >= 0:
    return arg
  return "\"" + arg.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

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

  if not hasattr(action, "args"):
    return cmd

  return " ".join([cmd] + [_escape_arg(arg) for arg in action.args])

def multi_action(ctx, actions=[], inputs=[], outputs=[], mnemonic=None,
                 progress_message=""):
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

  executable_inputs = [action.cmd for action in actions
                       if hasattr(action.cmd, "path")]
  # Put each command on a separate line to make "bazel build -s" more readable.
  # (Bazel will pass this string into "bash -c", so we can treat it like
  # a shell script.)
  command = "\n".join(
      # Fail if there are any errors, including from $(..) expressions or pipes.
      ["set -e -o pipefail"]
      + [_render_action(action) for action in actions])
  ctx.action(
      command=command,
      inputs=executable_inputs + inputs,
      mnemonic=mnemonic,
      progress_message=progress_message,
      outputs=outputs,
  )

def new_archive(ctx, name):
  """Create a new file object for an archive.

  Args:
    ctx: The current rule context.
    name: The name of the directory (relative to the current rule's package)
      that should be stored in this archive.

  Returns:
    A file object for the archive.
  """
  return ctx.new_file(name + ".tar")

def unarchived_path(path):
  """Get the path of the unarchived directory.

  Args:
    path: The path to the archive file.

  Returns:
    The path to the directory that this file will expand to.
  """
  if not path.endswith(".tar"):
    fail("Path %s doesn't end in \".tar\"" % path)
  return path[0:-4]

def _expand_dir_cmd(ctx, archive):
  """Expands a .tar archive file atomically from the other rules.

  Several rules might be trying to read files from the same archive.
  This is OK on a build machine where everything is isolated.  We try
  to share most of the logic between.

  """
  bin_dir = ctx.bin_dir.path + "/"
  path = bin_dir + ctx.label.package + "/" + ctx.label.name + ".archive.temp"
  return "\n".join([
      "# Expanding " + str(archive.path),
      # First, unarchive it to a temporary directory.
      "  mkdir -p %s" % bin_dir + ctx.label.package,
      "  ARCHIVE_TEMP=$(mktemp -d %s.XXXX)" % path,
      "  tar -xf %s -C $ARCHIVE_TEMP" % archive.path,
      # The archive contained a single top-level directory, e.g.,
      # foo.interfaces/{Bar,Baz.hi}.  Copy that top-level directory out of the
      # temp dir into the package location alongside the .tar file.
      # We use rsync so that (1) two simultaneous rules can't interfere with
      # each other when filling the same directory with the same contents, and
      # (2) we make sure the expanded directory is always up to date with the
      # .tar file, even if the latter was (re)generated during the build and then
      # copied locally.
      # There are two reasons why direct tar extraction doesn't work:
      # (1) tar does not write files atomically, so partially extracted files
      # can be observed by a concurrent reader, and (2) tar does not remove
      # any stale files that do not exist in the archive.
      "  rsync -r --delete $ARCHIVE_TEMP/%s %s 2>/dev/null || true" % (
          unarchived_path(archive.basename), archive.dirname),
      # Remove the temporary directory.
      "  rm -rf $ARCHIVE_TEMP",
  ])


def archiving_action(ctx, actions=[], input_archives=[], output_archives=[],
                     inputs=[], outputs=[], **kwargs):
  """Run multiple actions, wrapping some inputs/outputs in archives.

  This function is like multi_action, except that it lets us specify certain
  archives to be expanded before running and to be created after running.

  Args:
    ctx: The current rule context.
    actions: A list of action structs (see multi_action for more details).
    input_archives: A list of archive Files to be unarchived before running
      these actions.
    output_archives: A list of archive Files to be created after running
      these actions.
    inputs: Other input files needed by the given actions.
    outputs: Other output files produced by the given actions.
    **kwargs: Other arguments of multi_action.
  """

  # Archives are constructed as follows:
  # For the directory "foo/bar/baz/" in the Bazel package "foo/bar",
  # containing the files "foo/bar/baz/{one,two}.txt":
  # - The archive file is "foo/bar/baz.tar"
  # - Within the archive, the file names are "baz/{one,two}.txt".
  # To debug this, use "tar -tf {bazel-bin/.../file.tar}" to see which files
  # are stored in which archives.
  all_actions = (
      # Remove the directories that correspond to the output archives, so that
      # the outputs don't contain any obsolete files from previous runs.
      [struct(cmd="rm", args=["-rf", unarchived_path(t.path)])
       for t in output_archives]
      # Pre-create any output directories (needed in particular for protos)
      + [struct(cmd="mkdir", args=[unarchived_path(t.path)])
         for t in output_archives]
      + [struct(cmd=_expand_dir_cmd(ctx, t)) for t in input_archives]
      + actions
      + [struct(cmd="tar",
                args=[
                    # Clear metadata to make this more reproducible.
                    "--mtime=1980-01-01",
                    "--owner=0",
                    "--group=0",
                    "-cf", t.path,
                    "-C", t.dirname,
                    unarchived_path(t.basename)
                ])
         for t in output_archives])
  multi_action(
      ctx,
      actions=all_actions,
      inputs=inputs + input_archives,
      outputs=outputs + output_archives,
      **kwargs)
